#' Generic function of class "EmissionsArray"
#'
#' @description Returns object with class "EmissionsArray"
#'
#' @return Construtor of objects with class "EmissionsArray"
#' @export
EmissionsArray <- function(veh, ...) {
  UseMethod("EmissionsArray", veh)
}
