#' Summary of Emissions
#'
#' @description The object with class "EmissionFactors" is esentially a
#' data.frame with columns as type of vehicles and rows as age. The summary
#' method considers this by arguments by, mean and default to produce summaries
#' the information of EmissionFactors in a helpful way.
#'
#' @return Summary for vehicle classes
#' @param ef Object of class EmissionFactors
#' @seealso \code{\link{summary}}
#' @export
#' @examples \dontrun{
#' data(fe2015)
#' names(fe2015)
#' class(fe2015)
#' df <- fe2015[fe2015$Pollutant=="CO", c(ncol(fe2015)-1,ncol(fe2015))]
#' ef1 <- as.EmissionFactors(df)
#' summary(ef1)
#' }
summary.Emissions <- function(e, by = "age", ...) {
  if(by =="age") {
    avemi <- sum(seq(1,ncol(e))*colSums(e)/sum(e))
    cat("Total emissions by age in study area = \n")
    print(summary(colSums(e), ...))#
    cat("\nAverage age = ", round(avemi,2))#,"  sd = ",round(sdage,2),"\n\n")
  } else if (by=="streets") {
    cat("Emissions by street in study area = \n")
    print(summary(rowSums(e)))
  } else if (by == "all") {
    cat("Emissions by age and street in study area = \n")
    print(summary(unlist(e), ...))
  } else if (by == "default") {
    cat("Summary for each type of vehicle by street = \n")
    print(summary.data.frame(e), ...)
  }
}
