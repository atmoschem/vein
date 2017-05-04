#' Printing class "EmissionFactors"
#'
#' @description A simple method for printing objects with class
#' "EmissionFactors".
#' @title Print for EmissionFactors
#' @param ef Object with class "EmissionFactors"
#' @param all when T call method print.data.frame \code{\link{print.data.frame}}.
#' When F applies a  default print to each column
#' @param ... ignored
#' @return Print method
#' @rdname print.EmissionFactors
#' @title  Print for EmisionFactors
#' @export
EmissionFactors <- function(ef, ...) {
  UseMethod("EmissionFactors", ef)
}
#' @examples \dontrun{
#' # Do not run
#' data(fe2015)
#' names(fe2015)
#' class(fe2015)
#' df <- fe2015[fe2015$Pollutant=="CO", c(ncol(fe2015)-1,ncol(fe2015))]
#' ef1 <- as.EmissionFactors(df)
#' print(ef1)
#' print(ef1, all = F)

#' }
print.EmissionFactors <- function(ef, all = TRUE) {
  if(all ==TRUE) {
    print.data.frame(ef)
  } else {
    sapply(ef, print, zero.print=".")
  }
}
