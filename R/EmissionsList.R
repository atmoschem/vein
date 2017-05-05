#' Generic function for lass "EmissionsList"
#'
#' @description Generic function for class "EmissionsList"
#'
#' @return Objects of class "Emissionslist"
#'
#' @param e Object which can be "list"
#' @param ... ignored
#'
#' @rdname EmissionsList
#' @name EmissionsList
#' @title EmissionsList
#' @export
EmissionsList <- function(e, ...) {
  UseMethod("EmissionsList", e)
}
