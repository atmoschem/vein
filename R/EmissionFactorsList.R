#' Generic function of class "EmissionFactorsList"
#'
#' @description Returns object with class "EmissionFactorsList"
#'
#' @return Construtor of objects with class "EmissionFactorsList"
#' @export
EmissionFactors <- function(ef, ...) {
  UseMethod("EmissionFactors", ef)
}
