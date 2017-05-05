#' Generic function Speed
#'
#' @description Generic function
#'
#' @param spd object
#' @param ... ignored
#' @export
Speed <- function(spd, ...){
  UseMethod("Speed", spd)
}
