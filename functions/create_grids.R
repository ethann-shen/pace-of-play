library(tidyverse)
library(sf)

# creates a dataframe of grids
create_square_grids <- function(pitch_length = 105, pitch_width = 70, dimension) {
  boxes = list()
  
  x_length <- pitch_length / dimension
  y_length <- pitch_width / dimension
  
  for (i in 0:(x_length - 1)) {
    for (j in 0:(y_length - 1)) {
      boxes[[(i+1) + (x_length*j)]] <- list(matrix(c(i*dimension,pitch_width - dimension-(j*dimension), 
                                                     dimension+i*dimension,pitch_width - dimension-(j*dimension), 
                                                     dimension+i*dimension,pitch_width - dimension+dimension-(j*dimension), 
                                                     i*dimension,pitch_width - dimension+dimension-(j*dimension), 
                                                     i*dimension,pitch_width - dimension-(j*dimension)), 
                                                   ncol=2, byrow=TRUE)) %>% 
        st_polygon() %>% st_geometry() %>% as_tibble() 
    }
  }
  
  do.call(rbind, boxes) %>% st_as_sf() 
}

# EXAMPLE
create_square_grids(dimension = 5) %>% mutate(rowid = row_number()) %>% 
  ggplot() + geom_sf() +geom_sf_label(aes(label = rowid))

create_square_grids(dimension = 7) %>% mutate(rowid = row_number()) %>% 
  ggplot() + geom_sf() + geom_sf_label(aes(label = rowid))
