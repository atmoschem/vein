#' Plot of Speed
#'
#' @description The object with class "Speed" is esentially a data.frame
#' with columns as type of vehicles and rows as streets. Therefore the plot
#' seeks to inform the total number by type of vehicles. This functions
#' plot the mean speed and the standard deviation.
#'
#' @param by Character that determines the type of plot. It accept the values:
#' "col", "streets" and "default".
#' When by is "col" it is shown a plot of the mean of the columns
#' (See \code{\link{colMeans}}) and also the standard deviation.
#' When by is "streets" performs  sum of the streets via \code{\link{rowSums}}.
#' When by is "default" it plots the default method for plot
#' (See \code{\link{plot.default}}).
#' @param mean a logical value. When mean is TRUE and by is "col" it adds
#' the weighted mean with and when by is "streets" the mean.
#'
#' @return Plot Speed class
#' @export
#' @examples \dontrun{
#' data(net)
#' df <- netspeed(pcw, net$ps,net$ffs, net$capacity, net$lkm, alpha = 1,isList=F)
#' class(df)
#' plot(df$S1)
#' plot(df, by = "col", mean=F, xlab="test")
#' plot(df, by="streets", mean=F)
#' plot(df, by="streets", mean=T)
#' }
plot.Speed <- function(spd, by = "col", mean = TRUE, dist = "km", time="h",
                       xlab = NULL, ...) {
  if ( by == "default" ) {
    plot.default(spd, xlab = xlab, ...)
  } else if ( by == "col" && mean == FALSE ){
    Speed <- as.Speed(colMeans(spd, na.rm = T))
    plot(Speed, xlab = xlab, type = "l")
  } else if ( by == "col" && mean == TRUE ){
    Speed <- as.Speed(colMeans(spd), distance = dist, time=time)
    SpeedSD <- as.Speed(unlist(lapply(spd,sd)))
    smin <- Speed - SpeedSD
    smax <- Speed + SpeedSD
    avspd <- mean(Speed, na.rm=T)
    plot(Speed, type = "l", main=paste(deparse(substitute(spd))),
         xlab = xlab, ylim=c(min(smin),max(smax)), ...)
    abline(h = avspd, col="red")
    lines(smin, ylim=c(min(smin),max(smax)), col="grey", ...)
    lines(smax, ylim=c(min(smin),max(smax)), col="grey", ...)
  } else if ( by == "streets" && mean == FALSE ){
    Speed <- as.Speed(rowMeans(spd), distance = dist, time=time)
    plot(Speed, xlab = xlab, type="l", ...)
  } else if ( by=="streets" && mean == TRUE ){
    avveh <- mean(rowMeans(spd), na.rm=T)
    Speed <- as.Speed(rowMeans(spd), distance = dist, time=time)
    plot(Speed,type="l", xlab = xlab, main=paste(deparse(substitute(spd))), ...)
    abline(h =  avveh, col = "red")
  }
}
