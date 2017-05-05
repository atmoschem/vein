#' Plot of Vehicles
#'
#' @description The object with class "Vehicles" is esentially a data.frame
#' with columns as type of vehicles and rows as streets. Therefore the plot
#' seeks to inform the total number by type of vehicles
#'
#' @param veh An object with class "Vehicles"
#' @param by Character that determines the type of plot. It accept the values:
#' "col", "streets" and "default". When by is "col" it is shown a plot of the
#' sum of the columns (See \code{\link{colSums}}). When by is "streets"
#' performs  sum of the streets via \code{\link{rowSums}}.When by is "default"
#' it plots the default method for plot (See \code{\link{plot}}).
#' @param mean a logical value. When mean is TRUE and by is "col" it adds
#' the weighted mean with and when by is "streets" the mean.
#' @param xlab xlab
#' @param ... ignored
#'
#' @name plot.Vehicles
#' @rdname plot.Vehicles
#' @name plot.Vehicles
#' @title plot
#' @aliases NULL
NULL
#' @examples \dontrun{
#' # Do not run
#' data(net)
#' LT_B5 <- age_hdv(x = net$ldv,name = "LT_B5")
#' graphics::plot(colSums(LT_B5), xlab = "c")
#' }
#' @export
plot.Vehicles <- function(veh, by = "col", mean = TRUE, xlab = "Index", ...) {
  if (by == "default") {
    graphics::plot(veh, xlab = xlab, ...)
  } else if (by=="col" && mean == FALSE){
    Veh <- Vehicles(colSums(veh))
    graphics::plot(Veh, type="l",xlab = xlab, ...)
  } else if (by=="col" && mean == TRUE){
    avage <- sum(seq(1,ncol(veh)) * colSums(veh)/sum(veh))
    units(avage) <- with(units::ud_units, 1/h)
    Veh <- Vehicles(colSums(veh))
    graphics::plot(Veh, type="l",xlab = xlab, ...)
    graphics::abline(v = avage, col="red")
    cat("\nAverage = ",round(avage,2))
  } else if (by=="streets" && mean == FALSE){
    Veh <- Vehicles(rowSums(veh))
    graphics::plot(Veh, type="l", xlab = xlab, ...)
  } else if (by=="streets" && mean == TRUE){
    avveh <- mean(rowSums(veh), na.rm=T)
    Veh <- Vehicles(rowSums(veh))
    graphics::plot(Veh, type="l",xlab = xlab, ...)
    graphics::abline(h =  avveh, col="red")
    cat("\nAverage = ",round(avveh,2))
  }
}
