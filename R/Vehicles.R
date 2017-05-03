#' Generic function of class "Vehicles"
#'
#' @description Returns amount of vehicles at each age
#'
#' @return Define vehicle classes inhetis of data.frame
#' @export
#' @examples \dontrun{
#' }
Vehicles <- function(veh, ...) {
  UseMethod("Vehicles", veh)
}
