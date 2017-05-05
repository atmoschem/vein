#' Generic function EmissionFactors
#'
#' @description Generic function
#'
#' @param ef object
#' @param ... ignored
#' @export
EmissionFactors <- function(ef, ...){
  UseMethod("EmissionFactors", ef)
}
