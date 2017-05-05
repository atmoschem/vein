#' Generic function for lass "EmissionFactorsList"
#'
#' @description Generic function for class "EmissionFactorsList"
#'
#' @return Objects of class "EmissionFactorsList" or "units"
#'
#' @param ef Object which can be "list" with "numeric" elements
#' @param ... ignored
#'
#' @rdname EmissionFactorsList
#' @name EmissionFactorsList
#' @title EmissionFactors
#' @export
EmissionFactorsList <- function(ef, ...) {
  UseMethod("EmissionFactorsList", ef)
}
