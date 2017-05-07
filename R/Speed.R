#' Construction function for class "Speed"
#'
#' @description Returns a tranformed object with class "Speed" and units
#'  km/h. This functions includes two arguments, distance and time. Therefore,
#'  it is posibel to change the units of the speed to "m" to "s" for example.
#'  This function returns a dataframe with units for speed. When this function
#'  is applied to numeric vectors it add class "units".
#'
#' @return Constructor for class "Speed" or "units"
#'
#' @param x Object with class "data.frame", "matrix" or "numeric"
#' @param object Object with class "Speed"
#' @param ... ignored
#' @seealso \code{\link{units}}
#'
#' @rdname Speed
#' @aliases Speed print.Speed summary.Speed plot.Speed
#' @examples \dontrun{
#' data(net)
#' data(pc_profile)
#' speed <- Speed(net$ps)
#' class(speed)
#' plot(speed, type = "l")
#' pc_week <- temp_fact(net$ldv+net$hdv, pc_profile)
#' df <- netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm)
#' summary(df)
#' }
#' @export
Speed <- function(x, ...) {
  spd <- x
  if  ( is.matrix(spd) ) {
    spd <- as.data.frame(spd)
    for(i in 1:ncol(spd)){
      spd[,i] <- spd[,i]*units::parse_unit("km h-1")
    }
    class(spd) <- c("Speed",class(spd))
  } else if ( is.data.frame(spd) ) {
    for(i in 1:ncol(spd)){
      spd[,i] <- spd[,i]*units::parse_unit("km h-1")
    }
    class(spd) <- c("Speed",class(spd))
  } else if ( is.list(spd) && is.list(spd[[1]]) ) {
    for (i in 1:length(spd) ) {
      for (j in 1:length(spd[[1]]) ) {
        spd[[i]][[j]] <- spd[[i]][[j]]*units::parse_unit("km h-1")
      }
    }
    #SpeedList?
  } else if ( is.numeric(spd) ) {
    spd <- spd*units::parse_unit("km h-1")
  }
  return(spd)
}

#' @rdname Speed
#' @method print Speed
#' @export
print.Speed <- function(x, ...) {
  cat("Result for Speed ")
  print(unclass(x),  ...)
}

#' @rdname Speed
#' @method summary Speed
#' @export
summary.Speed <- function(object,  ...) {
  spd <- object
    cat("Speeds by columns and street in study area = \n")
    print(summary(unlist(spd)))
}


#' @rdname Speed
#' @method plot Speed
#' @export
plot.Speed <- function(x, ...) {
  spd <- x
    Velocity <- Speed(colMeans(spd))
    Velocity <- Velocity*units::parse_unit("km h-1")
    VelocitySD <- Speed(unlist(lapply(spd,stats::sd)))
    smin <- Velocity - VelocitySD
    smax <- Velocity + VelocitySD
    avspd <- mean(Velocity, na.rm=T)
    graphics::plot(Velocity, type = "l",
                    ylim=c(min(smin),max(smax)), ...)
    graphics::abline(h = avspd, col="red")
    graphics::lines(smin, ylim=c(min(smin),max(smax)), col="grey", ...)
    graphics::lines(smax, ylim=c(min(smin),max(smax)), col="grey", ...)
}
