#' @importFrom ggplot2 theme
defaulttheme <- theme(axis.title = element_text(size = 11, color = "black"),
                      axis.text = element_text(size = 9, color ="black"),
                      axis.ticks = element_line(color = "black"),
                      panel.background = element_blank(),
                      panel.border = element_rect(fill = NA, color = "black"),
                      legend.key = element_blank(),
                      legend.position = "top",
                      legend.box = "vertical",
                      legend.box.just = "center",
                      legend.box.spacing = unit(0.1,"cm"),
                      legend.title = element_text(size = 11, color = "black"),
                      legend.text = element_text(size = 9, color = "black"))

#' Plot time series
#'
#' `series_plot()` creates a time series figure with observations as points and
#' model predictions as lines.
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#'
#' @param observations A data frame of observed values over time.
#' @param x The column in `observations` containing the values for the time
#' variable (e.g., days), which will be displayed on the x-axis.
#' @param y The column in `observations` containing the values for the response
#' variable (e.g., species abundances), which will be displayed on the y-axis.
#' @param group The column containing the identifier for the response variable
#' (e.g., series or species name). Also defaults to the color scheme, but this
#' can be adjusted with user-defined call to `aes(color = variable)`.
#' @param predictions A data frame of predicted values over time, with column
#' names matching the column names of `observations`. Optional.
#'
#' @return A `ggplot2` figure showing the time series of observations and model
#' predictions. Can be adjusted with additional `ggplot2` functions.
#'
#' @details
#' As with `ggplot2` aesthetics, do not enclose the column names in quotes.
#'
#' @export
#'
#' @examples
#' df <- data.frame("time" = rep(seq(1,100),4),
#'                  "species" = c(rep("A",100),rep("B",100),rep("C",100),rep("D",100)),
#'                  "abundance" = rpois(400,40))
#' series_plot(observations = df, x = time, y = abundance, group = species)
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


#' Plot two-dimensional phase plane
#'
#' `phase_plane_2D()` creates a two-dimensional phase plane showing how the two
#' state variables co-occur at each time point. Observations are plotted as points
#' and model predictions as lines.
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import metR
#'
#' @param observations A long-format data frame of observed values over time.
#' @param names The column in `observations` containing the identifier for the
#' response variable (e.g., series or species name).
#' @param values The column in `observations` containing the values for the
#' response variable (e.g., species abundances).
#' @param x The state variable to be displayed on the x-axis.
#' @param y The state variable to be displayed on the y-axis.
#' @param model A trained model that will be passed to the `get_right_hand_side()`
#' function, which yields the vector field. Optional.
#' @param predictions A long-format data frame of predicted values over time, with
#' column names matching the column names of `observations`. Optional.
#' @param vectors Boolean to display the vector field or not (default).
#' @param grid_interval The number of intervals separating grid points to display in each direction.
#' Default is 10 in both the horizontal and vertical directions, yielding 121 grid points.
#' @param vector_color The color of the vectors.
#' @param mag_scale Boolean to display the vector magnitude on the original scale
#' or a standard scale (default).
#' @param covariates A data frame of covariate data. Optional.
#' @param covariates_values The numeric column in the covariate data frame used to display
#' facet panels. The column is cut into four intervals and the median value is
#' displayed as the panel label.
#'
#' @return A `ggplot2` figure showing the phase plane of observations and model
#' predictions. Can be adjusted with additional `ggplot2` functions.
#'
#' @details
#' As with `ggplot2` aesthetics, do not enclose the column names in quotes.
#'
#' @export
#'
#' @examples
#' df <- data.frame("time" = rep(seq(1,100),4),
#'                  "species" = c(rep("A",100),rep("B",100),rep("C",100),rep("D",100)),
#'                  "abundance" = c(rpois(200,40),rpois(200,10)))
#' phase_plane_2D(observations = df, names = species, values = abundance,
#'                x = A, y = C) + ggplot2::labs(x =  "Species A", y = "Species C")
phase_plane_2D <- function(
    observations,
    names,
    values,
    x,
    y,
    model,
    predictions = NULL,
    vectors = FALSE,
    grid_interval = 10,
    vector_color = "#529ee0",
    mag_scale = FALSE,
    covariates = NULL,
    covariates_values = NULL
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
    geom_point(aes(color = "Observations")) +
    scale_color_manual(name = "Data Type",
                       values = c("Observations" = "black",
                                  "Predictions" = "aquamarine3")) +
    defaulttheme

  if (!is.null(predictions)) {
    p = p + geom_path(data = predictions, aes(color = "Predictions"))
  }

  if (vectors) {
    xcol <- observations %>% select({{x}})
    ycol <- observations %>% select({{y}})
    x_step <- (max(xcol)-min(xcol))/grid_interval
    y_step <- (max(ycol)-min(ycol))/grid_interval
    xvals <- seq(min(xcol), max(xcol), x_step)
    yvals <- seq(min(ycol), max(ycol), y_step)
    grid <- crossing(xvals, yvals)
    model_function = get_right_hand_side(model = model)
    if (!is.null(covariates)){
      facets <- covariates %>%
        group_by(bin = cut({{covariates_values}}, breaks = 4)) %>%
        summarise(median_val = round(median({{covariates_values}}),3))
      dgrid <- grid %>%
        cross_join(facets) %>%
        rowwise() %>%
        mutate(
          u = list(model_function(0, c(xvals, yvals), median_val)),
          dx = u[[1]],
          dy = u[[2]],
          angle = atan(dy/dx) * (180/pi),
          offset = case_when(
            dx >= 0 ~ 0,
            dx < 0 ~ 180),
          mag = sqrt(dx**2 + dy**2)/sqrt(x_step**2 + y_step**2)) %>%
        ungroup() %>%
        select(-u)
      p = p + facet_wrap(vars(median_val))
    }

    else{
      dgrid <- grid %>%
        rowwise() %>%
        mutate(
          u = list(model_function(0, c(xvals, yvals))),
          dx = u[[1]],
          dy = u[[2]],
          angle = atan(dy/dx) * (180/pi),
          offset = case_when(
            dx >= 0 ~ 0,
            dx < 0 ~ 180),
          mag = sqrt(dx**2 + dy**2)/sqrt(x_step**2 + y_step**2)) %>%
        ungroup() %>%
        select(-u)}

    if (mag_scale) {
      p = p + geom_vector(data = dgrid, aes(x = xvals, y = yvals,
                                            angle = angle + offset, mag = mag),
                          color = vector_color, show.legend = FALSE)
    }
    else{
      p = p + geom_vector(data = dgrid, aes(x = xvals, y = yvals,
                                            angle = angle + offset, mag = 1),
                          color = vector_color, show.legend = FALSE)
    }
  }

  return(p)

}


