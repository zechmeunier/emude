# emude_setup <- function(pkg_check=TRUE){
#   julia <- JuliaCall::julia_setup(installJulia = TRUE)
#   if(pkg_check) JuliaCall::julia_install_package_if_needed("UniversalDiffEq")
#   JuliaCall::julia_library("UniversalDiffEq")
#   functions <- JuliaCall::julia_eval("filter(isascii, replace.(string.(propertynames(UniversalDiffEq)),\"!\"=>\"_bang\"))")
#   ude <- julia_pkg_import("UniversalDiffEq",functions)
#   ude
# }

#' @import JuliaCall
#' @export
emude_setup <- function() {
  JuliaCall::julia_install_package_if_needed("UniversalDiffEq")
  JuliaCall::julia_install_package_if_needed("ComponentArrays")
  JuliaCall::julia_install_package_if_needed("Lux")
  JuliaCall::julia_install_package_if_needed("Random")
  JuliaCall::julia_install_package_if_needed("DiffEqFlux")
  JuliaCall::julia_install_package_if_needed("DataFrames")
  JuliaCall::julia_install_package_if_needed("JSON")
  JuliaCall::julia_library("UniversalDiffEq")
  JuliaCall::julia_library("ComponentArrays")
  JuliaCall::julia_library("Lux")
  JuliaCall::julia_library("Random")
  JuliaCall::julia_library("DiffEqFlux")
  JuliaCall::julia_library("DataFrames")
  # This finds the absolute path inside your package
  setup_path <- system.file("R", "helpers.jl", package = "emude")
  # Normalize it so Julia receives a clean, absolute path (with forward slashes)
  setup_path <- normalizePath(setup_path, winslash = "/", mustWork = TRUE)
  # Pass that path into the Julia include command
  JuliaCall::julia_command(paste0('include("', setup_path, '")'))
  return("Setup completed")
}
