#' Generic function of class "Emissions"
#'
#' @description Returns object with class "Emissions"
#'
#' @return Construtor of objects with class "Emissions"
#' @export
Emissions <- function(veh, ...) {
  UseMethod("Emissions", veh)
}
