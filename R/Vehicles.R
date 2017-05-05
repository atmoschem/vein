#' Generic function for class "Vehicles"
#'
#' @description Generic function for class "Vehicles"
#'
#' @return Objects of class "Vehicles"
#'
#' @param veh Object which can be "matrix", "data.frame" or "numeric"
#' @param ... ignored
#'
#' @rdname Vehicles
#' @name Vehicles
#' @title Vehicles
#' @export
Vehicles <- function(veh, ...) {
  UseMethod("Vehicles", veh)
}
