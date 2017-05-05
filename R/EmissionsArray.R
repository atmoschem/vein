#' Generic function for lass "EmissionsArray"
#'
#' @description Generic function for class "EmissionsArray"
#'
#' @return Objects of class "EmissionsArray"
#'
#' @param e Object which can be "array"
#' @param ... ignored
#'
#' @rdname EmissionsArray
#' @name EmissionsArray
#' @title EmissionsArray
#' @export
EmissionsArray <- function(e, ...) {
  UseMethod("EmissionsArray", e)
}
