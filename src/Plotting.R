library(ggplot2)

defaulttheme <- theme(axis.title = element_text(size = 11, color = "gray10"),
                      axis.text = element_text(size = 8, color ="gray10"),
                      axis.ticks = element_line(color = "gray10"),
                      panel.background = element_blank(),
                      panel.border = element_rect(fill = NA, color = "gray10"),
                      legend.key = element_blank(), 
                      legend.position = "top",
                      legend.title = element_text(size = 10, color = "gray10"),
                      legend.text = element_text(size = 9, color = "gray10"))