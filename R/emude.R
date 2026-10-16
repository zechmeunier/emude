# emude_setup <- function(pkg_check=TRUE){
#   julia <- JuliaCall::julia_setup(installJulia = TRUE)
#   if(pkg_check) JuliaCall::julia_install_package_if_needed("UniversalDiffEq")
#   JuliaCall::julia_library("UniversalDiffEq")
#   functions <- JuliaCall::julia_eval("filter(isascii, replace.(string.(propertynames(UniversalDiffEq)),\"!\"=>\"_bang\"))")
#   ude <- julia_pkg_import("UniversalDiffEq",functions)
#   ude
# }


#' Set up the emude package
#'
#' `emude_setup()` checks that all required Julia packages are installed and loaded,
#' then sources necessary Julia functions in the R package. The function does not require any
#' parameters, nor should any be passed, and it should be run after loading the `emude` package.
#' @import JuliaCall
#' @export
#'
#' @examples
#' library(emude)
#' emude_setup()
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
  JuliaCall::julia_library("JSON")
  emudepath <- system.file(package = "emude")
  JuliaCall::julia_source(paste0(emudepath,'/Julia/helpers.jl'))
  return("Setup completed")
}
