#' Printing class "Emissions"
#'
#' @description A simple method for printing objects with class "Emissions".
#'
#' @param e Object with class "Emissions"
#' @param all when T call method print.data.frame \code{\link{print.data.frame}}.
#' When F applies a  default print to each column
#' @param ... ignored
#' @return Print method
#' @rdname print.Emissions
#' @export
Emissions <- function(e, ...) {
  UseMethod("Emissions", e)
}
#' @examples \dontrun{
#' # Do not run
#' eco <- 2:15
#' as.Emissions(eco)
#' dfco <- as.Emissions(as.data.frame(eco))
#' print(dfco)
#' print(dfco, all = F)
#' }
print.Emissions <- function(e, all = TRUE) {
  if(all ==TRUE) {
    print.data.frame(e)
  } else {
    sapply(e, print, zero.print=".")
  }
}
