library(ggplot2)
library(dplyr)

defaulttheme <- theme(axis.title = element_text(size = 11, color = "gray10"),
                      axis.text = element_text(size = 9, color ="gray10"),
                      axis.ticks = element_line(color = "gray10"),
                      panel.background = element_blank(),
                      panel.border = element_rect(fill = NA, color = "gray10"),
                      legend.key = element_blank(), 
                      legend.position = "top",
                      legend.box = "vertical",
                      legend.box.just = "center",
                      legend.box.spacing = unit(0.1,"cm"),
                      legend.title = element_text(size = 10, color = "gray10"),
                      legend.text = element_text(size = 9, color = "gray10"))

#Plot observations as points and model predictions as lines in a time series
series_plot <- function(
    observations,
    x,
    y,
    group,
    predictions = NULL
){
  p <- ggplot(data = observations,
         aes(x = {{x}}, y = {{y}}, group = {{group}}, color = {{group}})) + 
    geom_point() +
    defaulttheme
  
    if (!is.null(predictions)) {
      p + geom_line(data = predictions) +
        scale_y_continuous(limits = c(
          min(c(0, observations %>% pull({{y}}), predictions %>% pull({{y}}))),
          max(observations %>% pull({{y}}), predictions %>% pull({{y}}))
      ))
    } else {
    p + scale_y_continuous(limits = c(
          min(c(0, observations %>% pull({{y}}))),
          max(observations %>% pull({{y}}))
      ))
    }
}

#Plot observations as points and model predictions as lines in a 2D phase plane
phase_plane_2D <- function(
    observations,
    names,
    values,
    x,
    y,
    predictions = NULL
){
  #Pivot observation and prediction datasets
  observations <- observations %>% 
    pivot_wider(names_from = {{names}}, values_from = {{values}})
  
  if (!is.null(predictions)) {
    predictions <- predictions %>% 
      pivot_wider(names_from = {{names}}, values_from = {{values}})
  }

  
  #Create plot
  p <- ggplot(data = observations, aes(x = {{x}}, y = {{y}})) + 
    geom_point() +
    defaulttheme
  
  if (!is.null(predictions)) {
    p + geom_path(data = predictions)
  } else {
    p
  }
}