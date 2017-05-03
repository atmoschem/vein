#' Generic function of class "Vehicles"
#'
#' @description Returns object with class "Vehicles"
#'
#' @return Construtor of objects with class "Vehicles"
#' @export
Vehicles <- function(veh, ...) {
  UseMethod("Vehicles", veh)
}
