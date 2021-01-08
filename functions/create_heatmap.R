library(tidyverse)
library(sf)
library(scoutr)

# this requires you to run  the first four functions (create_intersections.R, create_linstring.R, create_possession.R,  fill_grids.R), and a boxes dataframe
create_heatmap <- function(boxes, metric, pitch_length = 105, pitch_width = 70,
                               min_x_coord = 0, max_x_coord = 105, min_y_coord = 0, max_y_coord = 70)  {
  
  if (any(c(min_x_coord, max_x_coord, min_y_coord, max_y_coord) %% 5 != 0)) stop("Coordinates must be multiples of 5. Plese provide a different coordinate value.")  
  if (any(c(min_x_coord, max_x_coord, min_y_coord, max_y_coord) < 0)) stop("Coordinates must be positive.")
  if (any(c(min_x_coord, max_x_coord) > 105)) stop("x-coordinates must be less than 105.")
  if (any(c(min_y_coord, max_y_coord) > 70)) stop("y-coordinates must be less than 70")
  
  start_x <- seq(from = (min_x_coord/5) + 1, to = (min_x_coord/5) + 1 + 273, by= 21)
  end_x <- seq(from = max_x_coord/5, to = max_x_coord/5 + 273, by = 21)
  
  x_dim_boxes <- c()
  for (i in 1:length(start_x)) {
    x_dim_boxes <- c(x_dim_boxes, seq(start_x[i], end_x[i]))
  }
  
  start_y <- (21 * min_y_coord / 5) + 1
  end_y <- (21 * max_y_coord / 5) 
  y_dim_boxes <- seq(start_y, end_y)
  
  boxes %>% 
    mutate(`Speed (m/s)` = metric) %>% 
    slice(intersect(x_dim_boxes, y_dim_boxes)) %>% 
    ggplot() + 
    geom_sf(aes(fill = `Speed (m/s)`), color = NA) +
    geom_point(x = pitch_length / 2, y = pitch_width / 2, shape = 1, size = 20, color = "#252525") +
    geom_rect(xmin = 0, 
              xmax = 16.5, 
              ymin = (pitch_width - 40.3) / 2, 
              ymax = (pitch_width - 40.3) / 2 + 40.3,
              fill = "white", color = "#252525", alpha = 0) +
    geom_rect(xmin = pitch_length - 16.5, 
              xmax = pitch_length, 
              ymin = (pitch_width - 40.3) / 2, 
              ymax = (pitch_width - 40.3) / 2 + 40.3,
              fill = "white", color = "#252525", alpha = 0) +
    geom_rect(xmin = 0, 
              xmax = 5.5, 
              ymin = (pitch_width - 40.3) / 2 + 11, 
              ymax = (pitch_width - 40.3) / 2 + 40.3 - 11,
              fill = "white", color = "#252525", alpha = 0) +
    geom_rect(xmin = pitch_length - 5.5, 
              xmax = pitch_length, 
              ymin = (pitch_width - 40.3) / 2 + 11, 
              ymax = (pitch_width - 40.3) / 2 + 40.3 - 11,
              fill = "white", color = "#252525", alpha = 0) +
    geom_segment(aes(x = 0, y = 0, xend = pitch_length, yend = 0), color = "#252525") + 
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = pitch_width), color = "#252525") +
    geom_segment(aes(x = pitch_length, y = 0, xend = pitch_length, yend = pitch_width), color = "#252525") +
    geom_segment(aes(x = 0, y = pitch_width, xend = pitch_length, yend = pitch_width), color = "#252525") +
    geom_segment(aes(x = pitch_length / 2, y = 0, xend = pitch_length / 2, yend = pitch_width), color = "#252525") +
    #scale_fill_gradient2(low  = "red", mid = "white", high = "blue") + 
    scale_fill_gradient(low = "#ffffb2", high = "#bd0026", na.value = "#cdff9c") +
    scale_x_continuous(breaks = seq(0, pitch_length, 15), labels = seq(0, pitch_length, 15)) +
    scale_y_continuous(breaks = seq(0, pitch_width, 10), labels = seq(0, pitch_width, 10)) +
    theme_bw() +
    labs(x = "", y = "") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#cdff9c"), 
          axis.line = element_line(colour = "#252525")) 
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

# from fill_grids.R
speeds <- fill_grids(
  first_game,
  boxes_adj_df,
  intersections,
  "adj_total_speed"
)


## This is the actual function
median_speeds <- map_dbl(speeds, median, na.rm=TRUE)
# this looks at the whole pitch
create_heatmap(boxes = boxes_adj_df, metric = median_speeds)


# you can filter out parts of the pitch as well, which readjusts the scale
# this looks at the attacking part of the pitch
create_heatmap(boxes = boxes_adj_df, metric = median_speeds, min_x_coord = 50)
# here is a random box
create_heatmap(boxes = boxes_adj_df, metric = median_speeds, min_x_coord = 25, max_x_coord = 100, min_y_coord = 10, max_y_coord = 55)


