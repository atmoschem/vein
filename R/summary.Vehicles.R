#' Summary of Vehicles
#'
#' @description The object with class "Vehicles" is esentially a data.frame
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
#' @param ... ignored
#' @rdname summary.Vehicles
#' @export
Vehicles <- function(veh, ...) {
  UseMethod("Vehicles", veh)
}
#' @examples \dontrun{
#' data(net)
#' lt <- as.Vehicles(net$hdv)
#' class(lt)
#' plot(lt)
#' #with data.frames
#' LT_B5 <- age_hdv(x = lt,name = "LT_B5")
#' summary(LT_B5, by="age")
#' summary(LT_B5, by="streets")
#' summary(LT_B5, by="all")
#' summary(LT_B5, by="default")
#' }
summary.Vehicles <- function(veh, by = "col") {
  if(by =="col") {
    avage <- sum(seq(1,ncol(veh))*colSums(veh)/sum(veh))
    cat("Vehicles by columns in study area = \n")
    print(summary(colSums(veh)))#
    cat("\nAverage = ", round(avage,2))#,"  sd = ",round(sdage,2),"\n\n")
  } else if (by=="streets") {
    summary(rowSums(veh))
    avveh <- mean(rowSums(veh), na.rm=T)
    cat("Vehicles by street in study area = \n")
    print(summary(rowSums(veh)))
    cat("\nAverage = ", round(avveh,2))#,"  sd = ",round(sdveh,2),"\n\n")
  } else if (by == "all") {
    cat("Vehicles by age and street in study area = \n")
    print(summary(unlist(veh)))
  } else if (by == "default") {
    cat("Summary for each type of vehicle by street = \n")
    print(summary.data.frame(veh))
  }
}
