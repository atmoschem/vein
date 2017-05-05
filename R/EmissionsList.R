#' Generic function EmissionsArray
#'
#' @description Generic function
#'
#' @param e object
#' @param ... ignored
#' @export
EmissionsList <- function(e, ...){
  UseMethod("EmissionsList", e)
}
