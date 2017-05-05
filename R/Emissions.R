#' Generic function for lass "Emissions"
#'
#' @description Generic function for class "Emissions"
#'
#' @return Objects of class "Emissions" or "units"
#'
#' @param e Object which can be "matrix", "data.frame" or "numeric"
#' @param ... ignored
#'
#' @rdname Emissions
#' @name Emissions
#' @title Emissions
#' @export
Emissions <- function(e, ...) {
  UseMethod("Emissions", e)
}
