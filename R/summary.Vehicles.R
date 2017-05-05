#' Summary of vein classes
#'
#' @description The summary methods
#'
#' @return Summary for vein classes
#'
#' @param object An object with class  of vein
#' @param ... ignored
#'
#' @seealso \code{\link{summary}}
#' @name summary.Vehicles
#' @aliases summary.Vehicles
#' @examples \dontrun{
#' # do not run
#' }
#' @export
summary.Vehicles <- function(object, ...) {
  veh <- object
    avage <- sum(seq(1,ncol(veh))*colSums(veh)/sum(veh))
    cat("Vehicles by columns in study area = \n")
    print(summary(colSums(veh)) )
    cat("\nAverage = ", round(avage,2))
    summary(rowSums(veh))
    avveh <- mean(rowSums(veh), na.rm=T)
    cat("Vehicles by street in study area = \n")
    print(summary(rowSums(veh)))
    cat("\nAverage = ", round(avveh,2))
}

