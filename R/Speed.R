#' Generic function of class "Speed"
#'
#' @description Returns object with class "Speed"
#'
#' @return Construtor of objects with class "Speed"
#' @export
Speed <- function(spd, ...) {
  UseMethod("Speed", spd)
}
