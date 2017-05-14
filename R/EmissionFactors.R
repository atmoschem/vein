#' Construction function for class "EmissionFactors"
#'
#' @description Returns a tranformed object with class "EmissionFactors" and
#' units g/km.
#'
#' @return Objects of class "EmissionFactors" or "units"
#'
#' @param x Object with class "data.frame", "matrix" or "numeric"
#' @param object Object with class "EmissionFactors"
#' @param ... ignored
#' @rdname EmissionFactors
#' @aliases EmissionFactors print.EmissionFactors summary.EmissionFactors
#' plot.EmissionFactors
#' @examples \dontrun{
#' data(fe2015)
#' names(fe2015)
#' class(fe2015)
#' df <- fe2015[fe2015$Pollutant=="CO", c(ncol(fe2015)-1,ncol(fe2015))]
#' ef1 <- EmissionFactors(df)
#' class(ef1)
#' summary(ef1)
#' plot(ef1)
#' print(ef1)
#' }
#' @export
EmissionFactors <- function(x, ...) {
  ef <- x
  if ( is.matrix(ef) ) {
    ef <- as.data.frame(ef)
    for(i in 1:ncol(ef)){
      ef[,i] <- ef[,i]*units::parse_unit("g km-1")
    }
    class(ef) <- c("EmissionFactors",class(ef))
    efx <- ef
  } else if ( is.data.frame(ef) ) {
    for(i in 1:ncol(ef)){
      ef[,i] <- ef[,i]*units::parse_unit("g km-1")
    }
    class(ef) <- c("EmissionFactors",class(ef))
    efx <- ef
  } else if ( class(ef) == "units" ) {
    warning("Check units are g/km")
    class(e) <- c("EmissionFactors",class(x))
  } else if( class(ef) == "numeric" | class(e) == "integer" ) {
    ef <- ef*units::parse_unit("g km-1")
    class(ef) <- c("EmissionFactors",class(x))
    efx <- f
  }
  return(efx)
}

#' @rdname EmissionFactors
#' @method print EmissionFactors
#' @export
print.EmissionFactors <- function(x, ...) {
  cat("EmissionFactors:\n")
  NextMethod("print", x)
}


#' @rdname EmissionFactors
#' @method summary EmissionFactors
#' @export
summary.EmissionFactors <- function(object, ...) {
  cat("Mean EmissionFactors in study area = \n")
  print(summary.data.frame(object))
}

#' @rdname EmissionFactors
#' @method plot EmissionFactors
#' @export
plot.EmissionFactors <- function(x,  ...) {
  ef <- x
  if (mode(ef)=="numeric" || ncol(ef) > 12) {
    graphics::plot(ef, ...)
  } else if (ncol(ef) >= 2 & ncol(ef) <= 3) {
    graphics::par(mfrow=c(1, ncol(ef)), tcl = -0.5)
  } else if (ncol(ef) == 4) {
    graphics::par(mfrow=c(2, 2), tcl = -0.5)
  } else if (ncol(ef) >= 5 && ncol(ef) >= 6 ) {
    graphics::par(mfrow=c(2, 3), tcl = -0.5)
  } else if (ncol(ef) >= 7 && ncol(ef) >= 9 ) {
    graphics::par(mfrow=c(3, 3), tcl = -0.5)
  } else if (ncol(ef) >= 10 && ncol(ef) >= 12 ) {
    graphics::par(mfrow=c(3, 4), tcl = -0.5)
  }
  for (i in 1:ncol(ef)) {
    graphics::plot(ef[,i], type = "l", ...)
  }
  graphics::par(mfrow=c(1,1))
}
