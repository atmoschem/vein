#' Construction function for class "Speed"
#'
#' @description \code{Speed} returns a tranformed object with class "Speed" and units
#'  km/h. This functions includes two arguments, distance and time. Therefore,
#'  it is posibel to change the units of the speed to "m" to "s" for example.
#'  This function returns a dataframe with units for speed. When this function
#'  is applied to numeric vectors it add class "units".
#'
#' @return Constructor for class "Speed" or "units"
#'
#' @param x Object with class "data.frame", "matrix" or "numeric"
#' @param dist String indicating the units of the resulting distance in speed.
#' @param object Object with class "Speed"
#' @param pal Palette of colors available or the number of the position
#' @param rev Logical; to internally revert order of rgb color vectors.
#' @param fig1 par parameters for fig, \code{\link{par}}.
#' @param mai1 par parameters for mai, \code{\link{par}}.
#' @param fig2 par parameters for fig, \code{\link{par}}.
#' @param mai2 par parameters for mai, \code{\link{par}}.
#' @param fig3 par parameters for fig, \code{\link{par}}.
#' @param mai3 par parameters for mai, \code{\link{par}}.
#' @param ... ignored
#' Default is units is "km"
#' @importFrom units as_units
#' @importFrom fields image.plot
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
#' plot(df)
#' }
#' @export
Speed <- function(x, ..., dist = "km") {
  if  ( is.matrix(x) ) {
    spd <- as.data.frame(x)
    for(i in 1:ncol(spd)){
      spd[,i] <- spd[,i]*units::as_units(paste0(dist, " h-1"))
    }
    class(spd) <- c("Speed", "data.frame")
  } else if ( is.data.frame(x) ) {
    spd <- x
    for(i in 1:ncol(spd)){
      spd[,i] <- spd[,i]*units::as_units(paste0(dist, " h-1"))
    }
    class(spd) <- c("Speed",class(x))
  } else if ( is.list(x) ) {
    stop("List not supported")
    #SpeedList?
  } else if ( class(x) == "units" ) {
    spd <- x
    # message("x has the following units which wont be changed here")
    spd <- spd*units::set_units(paste0(dist, " h-1"))

  } else if( class(x) == "numeric" | class(x) == "integer" ) {
    spd <- x*units::as_units(paste0(dist, " h-1"))
  }
  return(spd)
}

#' @rdname Speed
#' @method print Speed
#' @export
print.Speed <- function(x, ...) {
  if(nrow(x) < 10 & ncol(x) < 10){
    NextMethod("print", x, right = TRUE)
  } else if (nrow(x) > 10 & ncol(x) < 10){
    print.data.frame(x[1:5, ], right = TRUE)
    cat(paste0("... and more ", nrow(x) - 5, " rows\n"))
  } else if(nrow(x) < 10 & ncol(x) > 10){
    print.data.frame(x[, 1:5], right = TRUE)
    cat(paste0("... and more ", ncol(x) - 5, " columns\n"))
  } else {
    print.data.frame(x[1:5, 1:5], right = TRUE)
    cat(paste0("... and more ", nrow(x) - 5, " rows\n"))
    cat(paste0("... and more ", ncol(x) - 5, " columns\n"))
  }
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
plot.Speed <- function(x,
                       pal = "mpl_inferno",
                       rev = FALSE,
                       fig1 = c(0,0.8,0,0.8),
                       fig2 = c(0,0.8,0.55,1),
                       fig3 = c(0.7,1,0,0.8),
                       mai1 = c(0.2, 0.82, 0.82, 0.42),
                       mai2 = c(1.3, 0.82, 0.82, 0.42),
                       mai3 = c(0.7, 0.62, 0.82, 0.42),
                       ...) {
  oldpar <- par(no.readonly = TRUE)       # code line i
  on.exit(par(oldpar))                    # code line i + 1

  if(ncol(x) > 1) {
    graphics::par(fig=fig1, #new=TRUE,
                  mai = mai1,
                  ...)

    fields::image.plot(
      x = 1:ncol(x),
      xaxt = "n",
      z =t(as.matrix(x))[, nrow(x):1],
      xlab = "",
      ylab = "streets",
      col = cptcity::cpt(pal = pal, rev = rev), horizontal = TRUE)

    graphics::par(fig=fig2,
                  mai = mai2,
                  new=TRUE,
                  ...)
    avage <- mean(unlist(x))
    graphics::plot(colMeans(x, na.rm = T),
                   type="l",
                   ylab = "mean",
                   xlab = "",
                   frame = FALSE,
                   xaxt = 'n')
    graphics::axis(3)

    graphics::abline(h = avage, col="red")
    cat("\nWeighted mean = ",round(avage,2))

    graphics::par(fig=fig3, new=TRUE,
                  mai = mai3,
                  ...)
    graphics::plot(x = rowMeans(x, na.rm = T), y = nrow(x):1,
                   type = "l", frame = FALSE, yaxt = "n", xlab = '',
                   ylab = '')
    graphics::abline(v = avage, col="red")

  } else {
    graphics::plot(unlist(x), type = "l", main = "1 column data")
  }
}
