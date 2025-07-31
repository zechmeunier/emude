emude_setup <- function(pkg_check=TRUE){
  julia <- JuliaCall::julia_setup(installJulia = TRUE)
  if(pkg_check) JuliaCall::julia_install_package_if_needed("UniversalDiffEq")
  JuliaCall::julia_library("UniversalDiffEq")
  functions <- JuliaCall::julia_eval("filter(isascii, replace.(string.(propertynames(UniversalDiffEq)),\"!\"=>\"_bang\"))")
  ude <- julia_pkg_import("UniversalDiffEq",functions)
  ude
}

