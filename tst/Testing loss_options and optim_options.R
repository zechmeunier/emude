#Set up
library(emude)
library(JuliaCall)
library(ModelMetrics)
library(tidyverse)
library(deSolve)
julia_setup(JULIA_HOME = "C:/Users/zdmeunier/.julia/juliaup/julia-1.12.1+0.x64.w64.mingw32/bin")
emude_setup()
lynxhare1 <- read.csv("./ESA2025/MacLulich 1937.csv")
NODE <- NODE(data = lynxhare1, time_column_name = "Year")
#works
train_UDE(
  NODE,
  loss_function = "joint likelihood",
  optimizer = "Adam",
  loss_options = list(observation_error = 0.025),
  optim_options = list(maxiter = 100)
)
#works
train_UDE(
  NODE,
  loss_function = "marginal likelihood",
  optimizer = "Adam",
  loss_options = list(process_error = 0.025),
  optim_options = list(maxiter = 100)
)
#works
train_UDE(
  NODE,
  loss_function = "derivative matching",
  optimizer = "Adam",
  loss_options = list(d = 1, remove_ends = 0),
  optim_options = list(step_size = 1, maxiter = 100)
)
#works
train_UDE(
  NODE,
  loss_function = "shooting",
  optimizer = "Adam",
  loss_options = list(),
  optim_options = list(maxiter = 100)
)
#works
train_UDE(
  NODE,
  loss_function = "multiple shooting",
  optimizer = "Adam",
  loss_options = list(pred_length = 7),
  optim_options = list(maxiter = 100)
)
#works but took forever
train_UDE(
  NODE,
  loss_function = "joint likelihood",
  optimizer = "BFGS",
  loss_options = list(process_error = 0.025, observation_error = 0.025),
  optim_options = list(initial_step_norm = 0.1)
)
#works
train_UDE(
  NODE,
  loss_function = "marginal likelihood",
  optimizer = "BFGS",
  loss_options = list(process_error = 0.025, observation_error = 0.025),
  optim_options = list(initial_step_norm = 0.1)
)
#works
train_UDE(
  NODE,
  loss_function = "derivative matching",
  optimizer = "BFGS",
  loss_options = list(d = 10, remove_ends = 0),
  optim_options = list(initial_step_norm = 0.1)
)
#works
train_UDE(
  NODE,
  loss_function = "shooting",
  optimizer = "BFGS",
  loss_options = list(),
  optim_options = list(initial_step_norm = 0.1)
)
#works
train_UDE(
  NODE,
  loss_function = "multiple shooting",
  optimizer = "BFGS",
  loss_options = list(pred_length = 7),
  optim_options = list(initial_step_norm = 0.1)
)
