#' Matrix and vector multiplication
#'
#' @description \code{matvect} it is a helper function to multiply matrices
#' with vector by rows or columns
#'
#' @param df Numeric Data-frame or matrix.
#' @param x Numeric vector.
#' @param by Character, with two value "row" or "col"
#' @return data-frame
#' @name vein-deprecated
#' @seealso \code{\link{vein-deprecated}}
#' @keywords internal
NULL

#' @rdname vein-deprecated
#'
#' @export
#' @examples {
#' # do not run
#' # DEPRECATED
#' }
matvect <- function(df, x, by = "row"){
  .Deprecated("matvect")
  "matvect"
}
