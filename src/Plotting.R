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

#Plot observations as points
series_plot <- function(
    data,
    x,
    y,
    group
){
  ggplot(data = data,
         aes(x = {{x}}, y = {{y}}, group = {{group}}, color = {{group}})) + 
    geom_point() +
    scale_y_continuous(limits = c(min(c(0, data %>% pull({{y}}))), #plot 0
                                  max(data %>% pull({{y}})))) +
    defaulttheme
}

#Plot observations as points and model predictions as lines
predictions_plot <- function(
    data,
    x,
    y,
    group,
    predictions
){
  ggplot(data = data,
         aes(x = {{x}}, y = {{y}}, group = {{group}}, color = {{group}})) + 
    geom_point() +
    geom_line(data = predictions) +
    scale_y_continuous(limits = c(min(c(0, data %>% pull({{y}}),
                                        predictions %>% pull({{y}}))), #plot 0
                                  max(data %>% pull({{y}}),
                                      predictions %>% pull({{y}})))) +
    defaulttheme
}