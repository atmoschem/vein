#' Generic function for class "Speed"
#'
#' @description Generic function for class "Speed"
#'
#' @return Objects of class "Speed"
#'
#' @param spd Object which can be "matrix", "data.frame" or "numeric"
#' @param ... ignored
#'
#' @rdname Speed
#' @name Speed
#' @title Speed
#' @export
Speed <- function(spd, ...) {
  UseMethod("Speed", spd)
}
