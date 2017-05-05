#' Summary of EmissionFactors
#'
#' @description The object with class "EmissionFactors" is esentially a
#' data.frame with columns as type of vehicles and rows as age. The summary
#' method considers this by arguments by, mean and default to produce summaries
#' the information of EmissionFactors in a helpful way.
#'
#' @return Summary for vehicle classes
#'
#' @param ef Object of class EmissionFactors
#'
#' @seealso \code{\link{summary}}
#' @export
#' @name summary.EmissionFactors
#' @examples \dontrun{
#' data(fe2015)
#' names(fe2015)
#' class(fe2015)
#' df <- fe2015[fe2015$Pollutant=="CO", c(ncol(fe2015)-1,ncol(fe2015))]
#' ef1 <- as.EmissionFactors(df)
#' summary(ef1)
#' }
summary.EmissionFactors <- function(ef) {
  cat("Mean EmissionFactors in study area = \n")
  print(summary.data.frame(ef))
}
