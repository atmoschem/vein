#' Generic function EmissionsArray
#'
#' @description Generic function
#'
#' @param e object
#' @param ... ignored
#' @export
EmissionsArray <- function(e, ...){
  UseMethod("EmissionsArray", e)
}
