derivsCovariates <- function(u, X, nn, p, t){
  du1 = p$r*u[1]* X[1] - nn[1]
  du2 = p$theta*nn[1] - p$m*u[2] * X[1]
  return(c(du1,du2))
}

years = 1845:1935

X = data.frame(
  Year = years,
  d = rpois(years, 1)
)

UDE1_rel <- custom_derivatives(data = lynxhare1_rel_train,
                               derivs = derivsCovariates,
                               initial_parameters = list(r=0.5, theta=0.1, m = 0.2),
                               time_column_name = "Year",
                               covariates = X)

train_UDE(UDE1_rel)
