library(tidyverse)
library(jsonlite)
library(sf)
# helper functions
set_start_position_names <- function(x, y) {
  names(x$positions[[1]]) <- y
  x
}

set_tag_names <- function(x) {
  if (!is_empty(x$tags)) {
    names(x$tags) <- str_c("id_", 1:length(x$tags))
  }
  x
}

read_event_data <- function(data_path) {
  read_json(data_path) %>% 
    modify(set_start_position_names, c("start_y", "start_x")) %>% 
    modify(set_tag_names) %>% 
    map_df(unlist) %>% 
    janitor::clean_names() %>% 
    rename(start_y = positions_start_y,
           start_x = positions_start_x,
           end_y   = positions_y,
           end_x   = positions_x) %>% 
    rename_with(~str_remove(., "_id$"), .cols = starts_with("tags_id_")) %>% 
    mutate_at(.vars = vars(ends_with("_y"), ends_with("_x"), "event_sec"), .funs = as.double) %>% 
    mutate(end_x = if_else(is.na(end_x), start_x, end_x), # always a foul
           end_y = if_else(is.na(end_y), start_y, end_y)) %>% 
    select(event_id, sub_event_name, starts_with("tags_id"), everything())
}

# event data
england <- read_event_data("json_files/events/events_England.json") 
france <- read_event_data("json_files/events/events_France.json")
germany <- read_event_data("json_files/events/events_Germany.json")
italy <- read_event_data("json_files/events/events_Italy.json")
spain <- read_event_data("json_files/events/events_Spain.json")

# player data
players <- read_json("json_files/players.json")
for (i in which(sapply(players, function(x) x[['currentNationalTeamId']]) == 'null')) {
  players[[i]]$currentNationalTeamId <- NA
}

team_id_index <- c(which(sapply(players, function(x) is.null(x[['currentTeamId']]))),
                   which(sapply(players, function(x) x[['currentTeamId']]) == 'null')
) 

for (i in team_id_index) {
  players[[i]]$currentTeamId <- NA
}

player_data <- players %>% 
  map_df(unlist)
player_data %>% saveRDS("data/players.Rds")

# team data
teams <- read_json("json_files/teams.json") %>% 
  map_df(unlist) %>% 
  janitor::clean_names() %>%
  rename(area_alpha_3 = area_alpha3code,
         area_alpha_2 = area_alpha2code)
teams %>% saveRDS("data/teams.Rds")

# joining event and team data 

join_event_team_data <- function(event_data) {
  event_data %>% 
    left_join(teams, by = c("team_id" = "wy_id")) %>% 
    select(-(area_id:type))
}

england <- join_event_team_data(england) 
france <- join_event_team_data(france) 
germany <- join_event_team_data(germany) 
italy <- join_event_team_data(italy)
spain <- join_event_team_data(spain) 

# create possession id

make_possession_id <- function(event_data) {
  possession_list = c()
  all_matches <- event_data$match_id %>% unique() 
  
  for (match in 1:length(all_matches)) {
    print(all_matches[match])
    temp_list = c(1)
    pid = 1 # possession id, resets for each match 
    match_df <- event_data %>% filter(match_id == all_matches[match])
    
    for (i in 2:nrow(match_df)) {
      #print(i)
      if (match_df[i,]$event_name %in% c("Duel", "Others on the ball")) { 
        passes <- which(match_df$event_name == "Pass")
        previous_pass <- ifelse(purrr::is_empty(passes[passes < i]), i, passes[passes < i] %>% max())
        
        if (i > previous_pass) {
          next_pass <- previous_pass
        } else {
          next_pass <- passes[passes > i] %>% min()
        }
        if (match_df$name[previous_pass] == match_df$name[next_pass]) { # if there are consecutive duels, but the original team is able to make a pass , that is one possession
          temp_list <- c(temp_list, pid)
        } else {
          if (match_df$event_name[i+1] %in% c("Duel") | match_df$name[i] == match_df$name[previous_pass]) {
            temp_list <- c(temp_list, pid)
          } else {
            pid = pid+1
            temp_list <- c(temp_list, pid)
          }
        }
      } else if (match_df[i,]$event_name %in% c("Foul", "Free Kick", "Interruption",  "Offside", "Shot", "Save attempt")) {# automatically new possession everytime there is a free kick,  shot, or save attempt  
        if (match_df[i-1,]$event_name %in% c("Foul", "Free Kick", "Interruption",  "Offside", "Shot", "Save attempt")) { # if play ends in a foul, that completes the possession
          pid = pid + 1
          temp_list <- c(temp_list, pid)
        } else {
          temp_list <- c(temp_list, pid)
        }
      } else if (match_df[i,]$event_name == "Pass") { # if there are consecutive passes 
        
        passes <- which(match_df$event_name == "Pass")
        previous_pass <- ifelse(purrr::is_empty(passes[passes < i]), i, passes[passes < i] %>% max())
        
        if (match_df$name[i] == match_df$name[previous_pass]) { # deals with duels that end up with the original team still with possession
          temp_list <- c(temp_list, pid)
        } else {
          pid = pid + 1
          temp_list <- c(temp_list, pid)
        }
      } else {
        temp_list <- c(temp_list, pid)
      }
    }
    possession_list <- c(possession_list, temp_list)
  }
  return(possession_list)
}

england <- england %>% 
  mutate(poss_id = make_possession_id(england))
france <- france %>% 
  mutate(poss_id = make_possession_id(france))
germany <- germany %>% 
  mutate(poss_id = make_possession_id(germany))
italy <- italy %>% 
  mutate(poss_id = make_possession_id(italy))
spain <- spain %>% 
  mutate(poss_id = make_possession_id(spain))


# make sf linestring for each event 
make_line <- function(start_x, start_y, end_x, end_y) {
  st_linestring(matrix(c(start_x, end_x, start_y,  end_y), 2, 2))
}

# https://stackoverflow.com/questions/51918536/r-create-linestring-from-two-points-in-same-row-in-dataframe

add_linestring_to_df <- function(event_data) {
  event_data %>% 
    mutate_at(.vars = vars(ends_with("_y"), ends_with("_x"), "event_sec"), .funs = as.double) %>% 
    select(start_x, end_x, start_y, end_y) %>% 
    purrr::pmap(make_line)  %>% 
    st_as_sfc() %>% 
    {tibble(event_data, line_geometry = .)} %>% 
    st_sf() 
}

england <- add_linestring_to_df(england)


england %>% saveRDS("data/england.Rds")
france %>% saveRDS("data/france.Rds")
germany %>% saveRDS("data/germany.Rds")
italy %>% saveRDS("data/germany.Rds")
spain %>% saveRDS("data/spain.Rds")

### OLD 
# library(jsonlite)
# library(tidyverse)
# 
# ## helper functions
# find_list_columns = function(x) {
#   col_check = map_dfr(x, 
#                       ~ map(., length)) %>%
#     map_lgl(~ any(. > 1 | . == 0, na.rm = TRUE))
#   
#   list_cols = names(col_check)[col_check] 
#   
#   fix_list_cols = function(entry, name) {
#     if (name %in% list_cols) {
#       list(entry)
#     } else {
#       entry
#     }
#   }
#   
#   map_dfr(x,
#           function(char) {
#             map2(char, names(char), fix_list_cols) %>%
#               as_tibble()
#           }
#   ) 
# }
# 
# 
# 
# generate_event_data = function(event_json, match_json) {
#   
#   match_df = find_list_columns(match_json) %>% 
#     rename(match_wyId = wyId)
#   
#   for (i in which(sapply(event_json, function(x) x[['subEventId']]) == '')) {
#     event_json[[i]]$subEventId <- NA
#   }
#   
#   event_df = find_list_columns(event_json) %>% 
#     unnest(positions) %>% 
#     filter(row_number() %% 2 == 1)
#   
#   
#   coordinates_tbl = find_list_columns(event_df$positions)
#   event_df = cbind(event_df %>% select(-positions),
#                    coordinates_tbl)
#   
#   df = event_df %>% 
#     left_join(full_player_data, by=c("playerId" = "wyId")) %>% 
#     left_join(team_data, by=c("teamId" = "team_wyId")) %>% 
#     left_join(match_df, by=c("matchId" = "match_wyId"))
#   
#   return(df)
# }
# 
# players = read_json("players.json")
# teams = read_json("teams.json")
# 
# team_data = find_list_columns(teams)
# 
# team_data  <- team_data %>% 
#   rename(team_wyId = wyId)
# 
# for (i in which(sapply(players, function(x) x[['currentNationalTeamId']]) == 'null')) {
#   players[[i]]$currentNationalTeamId <- NA
# }
# 
# team_id_index = c(which(sapply(players, function(x) is.null(x[['currentTeamId']]))),
#                   which(sapply(players, function(x) x[['currentTeamId']]) == 'null')
# ) 
# 
# for (i in team_id_index) {
#   players[[i]]$currentTeamId <- NA
# }
# 
# player_data = find_list_columns(players)
# 
# player_roles = find_list_columns(player_data$role) 
# colnames(player_roles) <- paste0("role_", colnames(player_roles)) 
# 
# full_player_data = cbind(player_data %>% select(-role),
#                          player_roles %>% select(-role_code3)) %>% 
#   mutate(fullName = paste(firstName, middleName, lastName))
# 
# en_matches = read_json("matches/matches_England.json")
# en_events = read_json("events/events_England.json")
# england = generate_event_data(en_events, en_matches)
# saveRDS(england, "england_events.Rds")
# 
# fr_matches = read_json("matches/matches_France.json")
# fr_events = read_json("events/events_France.json")
# 
# france = generate_event_data(en_events, en_matches)
# saveRDS(france, "france_events.Rds")
# 
# ger_matches = read_json("matches/matches_Germany.json")
# ger_events = read_json("events/events_Germany.json")
# 
# germany = generate_event_data(en_events, en_matches)
# saveRDS(germany, "germany_events.Rds")
# 
# it_matches = read_json("matches/matches_Italy.json")
# it_events = read_json("events/events_Italy.json")
# 
# italy = generate_event_data(en_events, en_matches)
# saveRDS(italy, "italy_events.Rds")
# 
# sp_matches = read_json("matches/matches_Spain.json")
# sp_events = read_json("events/events_Spain.json")
# 
# spain = generate_event_data(en_events, en_matches)
# saveRDS(spain, "spain_events.Rds")
# 


