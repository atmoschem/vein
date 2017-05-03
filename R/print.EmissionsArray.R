#' Printing class "EmissionsArray"
#'
#' @description A simple method for printing objects with class "EmissionsArray"
#'
#' @param e Object with class "EmissionsArray"
#' @param default when T call method print.default
#' \code{\link{print.default}}. When F returns messages with array structure
#' @return Print method
#' @export
#' @examples \dontrun{
#' # Do not run
#' }
print.EmissionsArray <- function(e, default=F,  ...) {
  if ( default == TRUE ) {
    print.default(e)
  } else if (is.array(e)) {
    cat("This EmissionsArray has\n", dim(e)[1], "streets\n",
        dim(e)[2], "vehicle categories\n", dim(e)[3], "hours\n",
        dim(e)[4], "days\n")
    print(head(e))
  }
}
