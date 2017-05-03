#' Generic function of class "EmissionsList"
#'
#' @description Returns object with class "EmissionsList"
#'
#' @return Construtor of objects with class "EmissionsList"
#' @export
EmissionsList <- function(veh, ...) {
  UseMethod("EmissionsList", veh)
}
