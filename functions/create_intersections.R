library(tidyverse)
library(sf)
library(scoutr)

# Requires the creation of a "box" dataframe, a dataframe the contains the coordinates of the 5x5 (or any dimension) grids & a dataframe with linestring objects  (create_linestring.R)

# I'll probably make another function that  creates the grids,  but for now I'll just include what I have in the example. 

# For each event  (pass or free kick), this functions returns a list that keeps track of each "grid" the pass/free kick intersects. 
create_intersections <- function(event_data, box, geometry) {
  # LINESTRING objects created from transformed/rescaled coordinates 
  
  if (is.null(event_data %>% pull(geometry))) stop("Please create dataframe with a LINESTRING geometry column.")
  
  intersections = list()
  
  for (i in 1:nrow(event_data)) {
    intersections[[i]] <- box %>% 
      st_intersects(event_data[i,] %>% pull(geometry)) %>% 
      purrr::map_int(length) %>% 
      {which(. > 0)} 
    
    if (i %% 2000 == 1)  print(i)
  }
  return(intersections)
}

first_game <- read_events(system.file("extdata", "events_england.json", package = "scoutr"))
first_game <- first_game %>% 
  mutate_at(.vars = vars(ends_with("_y")), .funs = funs(100 - (. %>% as.double()))) %>% # rotates coordinates about x axis 
  mutate(adj_start_x = start_x * 1.05,
         adj_end_x = end_x * 1.05,
         adj_start_y = start_y * 0.7,
         adj_end_y = end_y * 0.7) 

first_game <- first_game %>% create_adj_linestring() 
load('boxes.Rda')

intersections <- create_intersections(first_game, boxes_adj_df, "adj_line_geometry")
intersections[[1]] # the first pass intersects the following squares

#to check  that this is true: 
boxes_adj_df %>% 
  slice(intersections[[1]]) %>% 
  ggplot() + geom_sf() +
  geom_sf(data = first_game %>% slice(1)) + 
  xlim(0,105) + ylim(0,70)




