#' Plot of Vehicles
#'
#' @description The object with class "Vehicles" is esentially a data.frame
#' with columns as type of vehicles and rows as streets. Therefore the plot
#' seeks to inform the total number by type of vehicles
#'
#' @param veh An object with class "Vehicles"
#' @param by Character that determines the type of plot. It accept the values:
#' "col", "streets" and "default".
#' When by is "col" it is shown a plot of the sum of the columns
#' (See \code{\link{colSums}}).
#' When by is "streets" performs  sum of the streets via \code{\link{rowSums}}.
#' When by is "default" it plots the default method for plot
#' (See \code{\link{plot.default}}).
#' @param ... ignored
#' @param mean a logical value. When mean is TRUE and by is "col" it adds
#' the weighted mean with and when by is "streets" the mean.
#' @return Plot vehicles class
#' @rdname plot.Vehicles
#' @export
Vehicles <- function(veh, ...) {
  UseMethod("Vehicles", veh)
}
#' @examples \dontrun{
#' # Do not run
#' data(net)
#' lt <- as.Vehicles(net$hdv)
#' class(lt)
#' plot(lt)
#' LT_B5 <- age_hdv(x = lt,name = "LT_B5")
#' plot(1:50,LT_B5[30,])
#' plot(LT_B5)
#' plot(LT_B5, by = "col", mean = T)
#' plot(LT_B5, by="streets")
#' plot(LT_B5, by="streets", mean=T)
#' }
plot.Vehicles <- function(veh, by = "col", mean = TRUE, ...) {
  if (by == "default") {
    graphics::plot.default(veh, ...)
  } else if (by=="col" && mean == FALSE){
    Veh <- as.Vehicles(colSums(veh))
    graphics::plot(Veh, type="l")
  } else if (by=="col" && mean == TRUE){
    avage <- sum(seq(1,ncol(veh)) * colSums(veh)/sum(veh))
    units(avage) <- with(ud_units, 1/h)
    Veh <- as.Vehicles(colSums(veh))
    graphics::plot(Veh, type="l", main=paste(deparse(substitute(veh))), ...)
    graphics::abline(v = avage, col="red")
    cat("\nAverage = ",round(avage,2))
  } else if (by=="streets" && mean == FALSE){
    Veh <- as.Vehicles(rowSums(veh))
    graphics::plot(Veh, type="l", ...)
  } else if (by=="streets" && mean == TRUE){
    avveh <- mean(rowSums(veh), na.rm=T)
    Veh <- as.Vehicles(rowSums(veh))
    graphics::plot(Veh, type="l", main=paste(deparse(substitute(veh))), ...)
    graphics::abline(h =  avveh, col="red")
    cat("\nAverage = ",round(avveh,2))
  }
}
