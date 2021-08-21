#' Construction function for class "Vehicles"
#'
#' @description \code{Vehicles} returns a tranformed object with class "Vehicles" and units
#'  'veh'. The type of objects supported are of classes "matrix", "data.frame",
#'  "numeric" and "array". If the object is a matrix it is converted to data.frame.
#'  If the object is "numeric" it is converted to class "units".
#'
#' @return Objects of class "Vehicles" or "units"
#'
#' @param x Object with class "Vehicles"
#' @param object Object with class "Vehicles"
#' @param pal Palette of colors available or the number of the position
#' @param rev Logical; to internally revert order of rgb color vectors.
#' @param bk Break points in sorted order to indicate the intervals for assigning the colors.
#' @param fig1 par parameters for fig, \code{\link{par}}.
#' @param mai1 par parameters for mai, \code{\link{par}}.
#' @param fig2 par parameters for fig, \code{\link{par}}.
#' @param mai2 par parameters for mai, \code{\link{par}}.
#' @param fig3 par parameters for fig, \code{\link{par}}.
#' @param mai3 par parameters for mai, \code{\link{par}}.
#' @param ... ignored
#' @param time Character to be the time units as denominator, eg "1/h"
#' @importFrom units as_units install_unit
#' @importFrom graphics par plot abline
#' @importFrom fields image.plot
#'
#' @rdname Vehicles
#' @aliases Vehicles print.Vehicles summary.Vehicles plot.Vehicles
#' @examples \dontrun{
#' lt <- rnorm(100, 300, 10)
#' class(lt)
#' vlt <- Vehicles(lt)
#' class(vlt)
#' plot(vlt)
#' LT_B5 <- age_hdv(x = lt,name = "LT_B5")
#' summary(LT_B5)
#' plot(LT_B5)
#' }
#' @export
Vehicles <- function(x, ..., time) {
 # units::install_unit("veh")

  if(inherits(x, "sf")) {

    geo <- sf::st_geometry(x)

    e <- sf::st_set_geometry(x, NULL)
    for(i in 1:ncol(e)){
      e[,i] <- e[,i]*units::as_units("veh")
    }

    if(!missing(time)){
      for(i in 1:ncol(e)) e[,i] <- e[,i]*units::as_units(1, time)
    }
    veh <- sf::st_sf(e, geometry = geo)


  } else  if  (is.matrix(x) ) {

    veh <- as.data.frame(x)

    for(i in 1:ncol(veh)){
      veh[,i] <- veh[,i]*units::as_units("veh")
    }

    if(!missing(time)){
      for(i in 1:ncol(e)) e[,i] <- e[,i]*units::as_units(1, time)
    }

    class(veh) <- c("Vehicles",class(veh))

  } else if ( is.data.frame(x) ) {

    veh <- x

    for(i in 1:ncol(veh)){
      veh[,i] <- veh[,i]*units::as_units("veh")
    }

    if(!missing(time)){
      for(i in 1:ncol(e)) e[,i] <- e[,i]*units::as_units(1, time)
    }

    class(veh) <- c("Vehicles",class(x))

  } else if ( class(x) == "units" ) {

    veh <- x

    if(units(x)$numerator != "veh") stop("units are not 'veh'")

  } else if( class(x) == "numeric" | class(x) == "integer" ) {

    veh <- x*units::as_units("veh")

    if(!missing(time)){
      veh <- veh*units::as_units(1, time)
    }

  }
  return(veh)
}

#' @rdname Vehicles
#' @method print Vehicles
#' @export
print.Vehicles <- function(x, ...) {
  nr <- ifelse(nrow(x) <= 5, nrow(x), 5)
  if(ncol(x) == 1) {
    ndf <- names(x)
    df <- data.frame(ndf = x[1:nr, ])
    names(df) <- ndf
    print.data.frame(df)
  } else {
    print.data.frame(x[1:nr, ])
  }
  if(nrow(x) > 5)     cat(paste0("... and ", nrow(x) - 5, " more rows\n"))
}

#' @rdname Vehicles
#' @method summary Vehicles
#' @export
summary.Vehicles <- function(object, ...) {
  # units::install_unit("veh")
  veh <- object
  avage <- sum(seq(1,ncol(veh))*colSums(veh)/sum(veh))
  cat("\nVehicles by columns in study area = \n")
  print(summary(colSums(veh)) )
  cat("Average = ", round(avage,2),"\n")
  summary(rowSums(veh))
  avveh <- mean(rowSums(veh), na.rm=T)
  cat("Vehicles by street in study area = \n")
  print(summary(rowSums(veh)))
  cat("\nAverage = ", round(avveh,2))
}

#' @rdname Vehicles
#' @method plot Vehicles
#' @export
plot.Vehicles <- function(x,
                          pal = "colo_lightningmccarl_into_the_night",
                          rev = TRUE,
                          bk =  NULL,
                          fig1 = c(0,0.8,0,0.8),
                          fig2 = c(0,0.8,0.55,1),
                          fig3 = c(0.7,1,0,0.8),
                          mai1 = c(0.2, 0.82, 0.82, 0.42),
                          mai2 = c(1.3, 0.82, 0.82, 0.42),
                          mai3 = c(0.7, 0.62, 0.82, 0.42),
                          ...) {
  # # units::install_unit("veh", warn = F)
  # veh <- x
  # if ( inherits(veh, "data.frame") ) {
  #   avage <- sum(seq(1,ncol(veh)) * colSums(veh)/sum(veh))
  #   Veh <- colSums(veh)
  #   Veh <- Veh*units::as_units("veh")
  #   graphics::plot(Veh, type = "l", ...)
  #   graphics::abline(v = avage, col = "red")
  #   if(message){
  #   cat("\nAverage = ",round(avage,2))
  #   }}
  #
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
      breaks = bk,
      col = cptcity::cpt(pal = pal, rev = rev), horizontal = TRUE)

    graphics::par(fig=fig2,
                  mai = mai2,
                  new=TRUE,
                  ...)
    avage <- sum(seq(1,ncol(x)) * colSums(x)/sum(x))
    graphics::plot(colSums(x, na.rm = T),
                   type="l",
                   ylab = "sum",
                   xlab = "",
                   frame = FALSE,
                   xaxt = 'n')
    graphics::axis(3)

    graphics::abline(v = avage, col="red")
    cat("\nWeighted mean = ",round(avage,2))

    graphics::par(fig=fig3, new=TRUE,
                  mai = mai3,
                  ...)
    graphics::plot(x = rowSums(x, na.rm = T), y = nrow(x):1,
                   type = "l", frame = FALSE, yaxt = "n", xlab = '',
                   ylab = '')
    graphics::abline(v = mean(rowSums(x, na.rm = T), na.rm = T), col="red")

  } else {
    graphics::plot(unlist(x), type = "l", main = "1 column data")
  }

}

