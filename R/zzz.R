#' @importFrom units install_unit remove_unit

.onLoad <- function(libname, pkg) {
  # Use GForce Optimisations in data.table operations
  # details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
  options(datatable.optimize = Inf) # nocov

units::install_unit("veh")
}


.onUnload <- function(libpath){
  units::remove_unit("veh") #nocov
}
