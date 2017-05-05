#' Plot of Speed
#'
#' @description The object with class "Speed" is esentially a data.frame
#' with columns as type of vehicles and rows as streets. Therefore the plot
#' seeks to inform the total number by type of vehicles. This functions
#' plot the mean speed and the standard deviation.
#'
#' @param spd Object of class "Speed"
#' @param by Character that determines the type of plot. It accept the values:
#' "col", "streets" and "default". When by is "col" it is shown a plot of the
#' mean of the columns (See \code{\link{colMeans}}) and also the standard
#' deviation. When by is "streets" performs  sum of the streets via
#' \code{\link{rowSums}}. When by is "default" it plots the default method
#' for plot (See \code{\link{plot}}).
#' @param mean a logical value. When mean is TRUE and by is "col" it adds
#' the weighted mean with and when by is "streets" the mean.
#' @param distance Character specifying the units for distance. Default is "km"
#' @param time Character specifying the units for time Default is "h"
#' @param xlab xlab
#' @param ... ignored
#' @name plot.Speed
#' @rdname plot.Speed
#' @name plot.Speed
#' @title plot
#' @aliases NULL
NULL
#' @examples \dontrun{
#' data(net)
#' data(pc_profile)
#' pcw <- temp_fact(net$ldv+net$hdv, pc_profile)
#' class(pcw)
#' df <- netspeed(pcw, net$ps,net$ffs, net$capacity, net$lkm, alpha = 1,isList=F)
#' class(df)
#' }
#' @export
plot.Speed <- function(spd, by = "col", mean = TRUE, distance = "km", time="h",
                       xlab = "Index", ...) {
  if ( by == "default" ) {
    graphics::plot(spd, xlab = xlab, ...)
  } else if ( by == "col" && mean == FALSE ){
    Velocity <- Speed(colMeans(spd, na.rm = T))
    plot(Velocity, xlab = xlab, type = "l")
  } else if ( by == "col" && mean == TRUE ){
    Velocity <- Speed(colMeans(spd), distance = distance, time = time)
    VelocitySD <- Speed(unlist(lapply(spd,stats::sd)))
    smin <- Velocity - VelocitySD
    smax <- Velocity + VelocitySD
    avspd <- mean(Velocity, na.rm=T)
    graphics::plot(Velocity, type = "l", main=paste(deparse(substitute(spd))),
         xlab = xlab, ylim=c(min(smin),max(smax)), ...)
    graphics::abline(h = avspd, col="red")
    graphics::lines(smin, ylim=c(min(smin),max(smax)), col="grey", ...)
    graphics::lines(smax, ylim=c(min(smin),max(smax)), col="grey", ...)
  } else if ( by == "streets" && mean == FALSE ){
    Velocity <- Speed(rowMeans(spd), distance = distance, time = time)
    plot(Velocity, xlab = xlab, type="l", ...)
  } else if ( by=="streets" && mean == TRUE ){
    avveh <- mean(rowMeans(spd), na.rm=T)
    Velocity <- Speed(rowMeans(spd), distance = distance, time = time)
    graphics::plot(Velocity,type="l", xlab = xlab,
                   main=paste(deparse(substitute(spd))), ...)
    graphics::abline(h =  avveh, col = "red")
  }
}
