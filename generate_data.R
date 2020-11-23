library(tidyverse)
library(jsonlite)

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
    select(event_id, sub_event_name, starts_with("tags_id"), everything())
}

# event data
england <- read_event_data("json_files/events/events_England.json")
france <- read_event_data("json_files/events/events_France.json")
germany <- read_event_data("json_files/events/events_Germany.json")
italy <- read_event_data("json_files/events/events_Italy.json")
spain <- read_event_data("json_files/events/events_Spain.json")

england %>% saveRDS("data/england.Rds")
france %>% saveRDS("data/france.Rds")
germany %>% saveRDS("data/germany.Rds")
italy %>% saveRDS("data/italy.Rds")
spain %>% saveRDS("data/spain.Rds")

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


