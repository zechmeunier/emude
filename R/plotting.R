defaulttheme <- ggplot2::theme(axis.title = ggplot2::element_text(size = 11, color = "black"),
                      axis.text = ggplot2::element_text(size = 9, color ="black"),
                      axis.ticks = ggplot2::element_line(color = "black"),
                      panel.background = ggplot2::element_blank(),
                      panel.border = ggplot2::element_rect(fill = NA, color = "black"),
                      legend.key = ggplot2::element_blank(),
                      legend.position = "top",
                      legend.box = "vertical",
                      legend.box.just = "center",
                      legend.box.spacing = ggplot2::unit(0.1,"cm"),
                      legend.title = ggplot2::element_text(size = 11, color = "black"),
                      legend.text = ggplot2::element_text(size = 9, color = "black"))
#' Series Plot
#'
#' Plot observations as points and model predictions as lines in a time series
#'
#' @param observations DataFrame of observed data to use to create the plot
#' @param x description
#' @param y description
#' @param group description
#' @param predictions
#' @return description
#' @export
series_plot <- function(
    observations,
    x,
    y,
    group,
    predictions = NULL
){
  p <- ggplot2::ggplot(data = observations,
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

#' Phase Plane 2D
#'
#' Plot observations as points and model predictions as lines in a 2D phase plane
#' @param observations description
#' @param names description
#' @param values description
#' @param x description
#' @param y description
#' @param predictions description
#' @return description
#' @export
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
