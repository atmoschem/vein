#' Generic function Speed
#'
#' @description Generic function
#'
#' @param veh object
#' @param ... ignored
#' @export
Vehicles <- function(veh, ...){
  UseMethod("Vehicles", veh)
}
