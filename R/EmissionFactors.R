#' Construction function for class "EmissionFactors"
#'
#' @description Returns a tranformed object with class "EmissionFactors" and
#' units g/km. This functions has arguments to change
#' the units.
#'
#' @return Objects of class "EmissionFactors" or "units"
#'
#' @param x Object with class "data.frame", "matrix" or "numeric"
#' @param object Object with class "EmissionFactors"
#' @param ... ignored
#' @rdname EmissionFactors
#' @aliases EmissionFactors print.EmissionFactors summary.EmissionFactors
#' plot.EmissionFactors
#' @note If the class ob the object is functions, as.EmissionFactors won't
#' append another class
#' @examples \dontrun{
#' data(fe2015)
#' names(fe2015)
#' class(fe2015)
#' df <- fe2015[fe2015$Pollutant=="CO", c(ncol(fe2015)-1,ncol(fe2015))]
#' ef1 <- as.EmissionFactors(df)
#' class(ef1)
#' head(ef1)
#' }
#' @export
EmissionFactors <- function(x, ...) {
  ef <- x
  if ( is.matrix(ef) ) {
    ef <- as.data.frame(ef)
    for(i in 1:ncol(ef)){
      ef[,i] <- set_units(ef[,i],  g/km)
    }
    class(ef) <- c("EmissionFactors",class(ef))
    efx <- ef
  } else if ( is.data.frame(ef) ) {
    for(i in 1:ncol(ef)){
      ef[,i] <- set_units(ef[,i],  g/km)
    }
    class(ef) <- c("EmissionFactors",class(ef))
    efx <- ef
  } else if ( is.numeric(ef) ) {
    ef <- set_units(ef,  g/km)
    class(ef) <- c("EmissionFactors",class(ef))
    efx <- ef
  }
  return(efx)
}

#' @rdname EmissionFactors
#' @method print EmissionFactors
#' @export
print.EmissionFactors <- function(x, ...) {
  cat("Result for EmissionFactors")
  print(unclass(x),  ...)
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
