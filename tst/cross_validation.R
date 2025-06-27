


library(tidyverse)
library(JuliaCall)
julia_setup(JULIA_HOME = '/Applications/Julia-1.10.app/Contents/Resources/julia/bin')
julia_eval("import Pkg; Pkg.activate(\".\")")
julia_library("UniversalDiffEq")
julia_eval("Pkg.status(\"UniversalDiffEq\")")
source("src/emude.R")
source("src/model_constructors.R")
source("src/model_testing.R")
source("src/training_functions.R")
source("src/model_analysis.R")


data = tibble(
  "t" = c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0), 
  "x" = c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0) , 
  "y" = c(2.0,4.0,8.0,16.0,32.0,64.0,128.0,256.0,512.0,1024.0)/1000
)

model <- NODE(data=data,hidden_units = 10, time_column_name = "t")

cross_validation(
    model,
    k = 10,
    loss_function = "derivative matching",
    optimizer = "ADAM",
    regularization_weight = 0.0,
    verbose = TRUE,
    loss_options = list(),
    optim_options = list(maxiter = 5, step_size = 0.5),
    path = NULL)
  
  

julia_eval("import Pkg; Pkg.status(\"UniversalDiffEq\")")



