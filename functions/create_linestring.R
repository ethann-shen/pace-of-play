library(tidyverse)
library(sf)
library(scoutr)

# This requires the coordinates to be transformed. I've done that in the example 

# This is a helper function  for the main function
make_adj_line <- function(adj_start_x, adj_start_y, adj_end_x, adj_end_y) {
  st_linestring(matrix(c(adj_start_x, adj_end_x, adj_start_y, adj_end_y), 2, 2))
}

# This is the main  function which creates a column, "adj_line_geometry", that is a linestring object for each event
create_adj_linestring <- function(event_data) {
  event_data %>% tibble::as_tibble() %>% #select(-line_geometry) %>% 
    select(adj_start_x, adj_end_x, adj_start_y, adj_end_y) %>% 
    purrr::pmap(make_adj_line) %>% 
    st_as_sfc() %>% 
    {tibble(event_data, adj_line_geometry = .)} %>% 
    st_sf() 
}



## EXAMPLE

first_game <- read_events(system.file("extdata", "events_england.json", package = "scoutr"))
first_game <- first_game %>% 
  mutate_at(.vars = vars(ends_with("_y")), .funs = funs(100 - (. %>% as.double()))) %>% # rotates coordinates about x axis 
  mutate(adj_start_x = start_x * 1.05,
         adj_end_x = end_x * 1.05,
         adj_start_y = start_y * 0.7,
         adj_end_y = end_y * 0.7) 

first_game <- first_game %>% create_adj_linestring() 