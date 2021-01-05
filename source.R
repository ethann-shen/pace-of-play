## Intersections Function


make_adj_intersections <- function(event_data, box, geometry, passes_only = TRUE) {
  # LINESTRING objects created from transformed/rescaled coordinates 
  
  if (is.null(event_data %>% pull(geometry))) stop("Please create dataframe with a LINESTRING geometry column.")
  
  if (passes_only==TRUE) {
    event_data_passes <- event_data %>% filter(event_name == "Pass")
  } else {
    event_data_passes <- event_data %>% filter(event_name == "Pass" | sub_event_name %in% c("Free Kick", "Throw in", "Goal kick", "Corner","Free kick cross"))
  }
  
  intersections = list()
  
  for (i in 1:nrow(event_data_passes)) {
    intersections[[i]] <- box %>% 
      st_intersects(event_data_passes[i,] %>% pull(geometry)) %>% 
      purrr::map_int(length) %>% 
      {which(. > 0)} 
    
    if (i %% 2000 == 1)  print(i)
  }
  return(intersections)
}


## Filling Grids Function


fill_rescaled_grids <- function(event_data, box, intersection_list, metric, passes_only = TRUE) {
  total_speeds = list()
  for (b in 1:nrow(box)){
    total_speeds[[b]] <- c(NA)
  }
  
  if (passes_only==TRUE) {
    event_data_passes <- event_data %>% filter(event_name == "Pass")
  } else {
    event_data_passes <- event_data %>% filter(event_name == "Pass" | sub_event_name %in% c("Free Kick", "Throw in", "Goal kick", "Corner","Free kick cross"))
  }
  
  for (i in 1:nrow(event_data_passes)) {
    if (i %% 2000 == 1)  print(i)
    for (j in intersection_list[[i]]) {
      total_speeds[[j]] <- c(total_speeds[[j]], event_data_passes %>% tibble::as_tibble() %>% {.[i,]} %>% pull(metric))
    } 
  }
  #agg_total_speeds <- map_dbl(total_speeds, metric, na.rm=TRUE) 
  return(total_speeds)
}

##  Heatmap Function

plot_pitch_heatmap <- function(boxes, metric, pitch_length = 105, pitch_width = 70)  {
  boxes %>% 
    mutate(`Speed (m/s)` = metric) %>% 
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
    scale_fill_gradient(#low = "#ffffcc", high = "#800026", 
      low = "#ffffb2", high = "#bd0026", na.value = "#cdff9c", ) +
    scale_x_continuous(breaks = seq(0, pitch_length, 15), labels = seq(0, pitch_length, 15)) +
    scale_y_continuous(breaks = seq(0, pitch_width, 10), labels = seq(0, pitch_width, 10)) +
    theme_bw() +
    labs(x = "", y = "") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#cdff9c"), 
          axis.line = element_line(colour = "#252525"))
}
