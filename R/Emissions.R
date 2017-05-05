#' Generic function Emissions
#'
#' @description Generic function
#'
#' @param e object
#' @param ... ignored
#' @export
Emissions <- function(e, ...){
  UseMethod("Emissions", e)
}
