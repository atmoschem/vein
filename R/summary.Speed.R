#' Summary of Speed
#'
#' @description The object with class "Speed" is esentially a data.frame
#' with columns as type of vehicles and rows as streets. The summary method
#' considers this by arguments by, mean and default to produce summaries
#' the information of vehicles in a helpful way.
#'
#' @return Summary for vehicle classes
#' @param by This parameter allows 4 values: "col", "streets", "all" and
#' "default" "col" returns a summary of the sum of the columns
#' via \code{\link{colSums}}, "streets" summary of the sum of the streets
#' via \code{\link{rowSums}}, "all" returns a simple summary to each combunation
#' columns x street and "default" the default summary.data.frame via
#' \code{\link{summary.data.frame}}
#' @seealso \code{\link{summary}}
#' @export
#' @examples \dontrun{
#' data(net)
#' data(pc_profile)
#' pcw <- temp_fact(net$ldv+net$hdv, pc_profile)
#' class(pcw)
#' df <- netspeed(pcw, net$ps,net$ffs, net$capacity, net$lkm, alpha = 1,isList=F)
#' class(df)
#' summary(df, by="age")
#' summary(df, by="streets")
#' summary(df, by="all")
#' summary(df, by="default")
#' }
summary.Speed <- function(spd, by = "col", ...) {
  if(by =="col") {
    cat("Mean Speeds by column in study area = \n")
    print(summary(colMeans(spd), ...))#
  } else if (by=="streets") {
    summary(rowMeans(spd), ...)
    cat("Mean speeds by street in study area = \n")
    print(summary(rowMeans(spd)))
  } else if (by == "all") {
    cat("Speeds by columns and street in study area = \n")
    print(summary(unlist(spd), ...))
  } else if (by == "default") {
    cat("Summary for each type of vehicle by street = \n")
    print(summary.data.frame(spd), ...)
  }
}
