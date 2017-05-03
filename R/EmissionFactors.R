#' Generic function of class "EmissionFactors"
#'
#' @description Returns object with class "EmissionFactors"
#'
#' @return Construtor of objects with class "EmissionFactors"
#' @export
EmissionFactors <- function(ef, ...) {
  UseMethod("EmissionFactors", ef)
}
