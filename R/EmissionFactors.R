#' Generic function for lass "EmissionFactors"
#'
#' @description Generic function for class "EmissionFactors"
#'
#' @return Objects of class "EmissionFactors" or "units"
#'
#' @param ef Object which can be "matrix", "data.frame" or "numeric"
#' @param ... ignored
#'
#' @rdname EmissionFactors
#' @name EmissionFactors
#' @title EmissionFactors
#' @export
EmissionFactors <- function(ef, ...) {
  UseMethod("EmissionFactors", ef)
}
