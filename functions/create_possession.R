library(tidyverse)
library(scoutr)

# This function requires you to join the event_data with the team_data. 
# The function creates a new column, poss_id, which indicates the possession associated with each event. 

make_possession_id <- function(event_data) {
  possession_list = c()
  all_matches <- event_data$match_id %>% unique() 
  
  for (match in 1:length(all_matches)) {
    print(all_matches[match]) # can be commented out
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
        if (match_df[i-1,]$event_name == "Free Kick" & match_df[i,]$name == match_df[i-1,]$name) { #if previous event is free kick  and same team, same  possession
          temp_list <- c(temp_list, pid)
        } else if (match_df$name[i] == match_df$name[previous_pass]) { # deals with duels that end up with the original team still with possession
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
  event_data %>% 
    mutate(poss_id = possession_list)
}

## EXAMPLE (w/ first game)

first_game <- read_events(system.file("extdata", "events_england.json", package = "scoutr"))
teams <- read_teams(system.file("extdata", "teams.json", package = "scoutr"))

join_event_team_data <- function(event_data) {
  event_data %>% 
    left_join(teams, by = c("team_id" = "wy_id")) %>% 
    select(-(area_id:type))
}

first_game <- join_event_team_data(first_game) 
first_game <- make_possession_id(first_game)
first_game %>% View()
