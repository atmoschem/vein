#' Generic function EmissionFactorsList
#'
#' @description Generic function
#'
#' @param ef object
#' @param ... ignored
#' @export
EmissionFactorsList <- function(ef, ...){
  UseMethod("EmissionFactorsList", ef)
}
