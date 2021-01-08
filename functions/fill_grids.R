library(tidyverse)
library(sf)
library(scoutr)

# Requires the creation of a "box" dataframe

# the event__data must contain the following:
# - adjusted coordinates & calculation of pass length & speed
# - the coordinates of the 5x5 (or any dimension) grids, 
# - a column with linestring objects  (create_linestring.R) 
# - the possession id (from create_possession.R)

# AND the list of intersections  (create_intersections.R)


# this function doesn't calculate the mean/median  - unsure if I should add that into this function 
fill_grids <- function(event_data, box, intersection_list, metric) {
  total_speeds = list()
  for (b in 1:nrow(box)){
    total_speeds[[b]] <- c(NA)
  }
  
  for (i in 1:nrow(event_data)) {
    if (i %% 2000 == 1)  print(i)
    for (j in intersection_list[[i]]) {
      total_speeds[[j]] <- c(total_speeds[[j]], event_data %>% tibble::as_tibble() %>% {.[i,]} %>% pull(metric))
    } 
  }
  #agg_total_speeds <- map_dbl(total_speeds, metric, na.rm=TRUE) 
  return(total_speeds)
}

## EXAMPLE
first_game <- read_events(system.file("extdata", "events_england.json", package = "scoutr"))

# from create_possession.R
teams <- read_teams(system.file("extdata", "teams.json", package = "scoutr"))

join_event_team_data <- function(event_data) {
  event_data %>% 
    left_join(teams, by = c("team_id" = "wy_id")) %>% 
    select(-(area_id:type))
}

first_game <- join_event_team_data(first_game) 
first_game <- make_possession_id(first_game)

# initial mutation  of dataframe (might make this into function later)
first_game <- first_game %>% 
  mutate_at(.vars = vars(ends_with("_y")), .funs = funs(100 - (. %>% as.double()))) %>% # rotates coordinates about x axis 
  mutate(adj_start_x = start_x * 1.05,
         adj_end_x = end_x * 1.05,
         adj_start_y = start_y * 0.7,
         adj_end_y = end_y * 0.7) %>%
  group_by(match_id, match_period, poss_id) %>% 
  mutate(duration = lead(event_sec) - (event_sec)) %>% 
  ungroup() %>% 
  filter(!is.na(duration)) %>% 
  mutate(adj_EW_dist = abs(adj_end_x - adj_start_x),
         adj_NS_dist = abs(adj_end_y - adj_start_y),
         adj_total_dist = sqrt(adj_EW_dist^2 + adj_NS_dist^2),
         
         adj_EW_speed = if_else(duration == 0, adj_EW_dist, adj_EW_dist / duration),
         adj_NS_speed = if_else(duration == 0, adj_NS_dist, adj_NS_dist / duration),
         adj_total_speed = if_else(duration == 0, adj_total_dist, adj_total_dist / duration)
  ) %>% 
  filter(event_name == "Pass" | sub_event_name %in% c("Free Kick", "Throw in", "Goal kick", "Corner","Free kick cross")) %>% 
  filter(!((end_x == 0 & end_y == 100) | (end_y == 0 & end_x == 100)))

# from create_linestring.R
first_game <- first_game %>% create_adj_linestring() 
load('boxes.Rda')

# from create_intersections.R
intersections <- create_intersections(first_game, boxes_adj_df, "adj_line_geometry")

# Here is the actual function.
speeds <- fill_grids(
  first_game,
  boxes_adj_df,
  intersections,
  "adj_total_speed"
)