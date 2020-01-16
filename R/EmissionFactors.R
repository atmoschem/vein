#' Construction function for class "EmissionFactors"
#'
#' @description \code{EmissionFactors} returns a tranformed object with class
#' "EmissionFactors" and units g/km.
#'
#' @return Objects of class "EmissionFactors" or "units"
#'
#' @param x Object with class "data.frame", "matrix" or "numeric"
#' @param object Object with class "EmissionFactors"
#' @importFrom units as_units
#' @param ... ignored
#' @rdname EmissionFactors
#' @aliases EmissionFactors print.EmissionFactors summary.EmissionFactors
#' plot.EmissionFactors
#' @examples {
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
  if ( is.matrix(x) ) {
    ef <- as.data.frame(x)
    for(i in 1:ncol(ef)){
      ef[,i] <- ef[,i]*units::as_units("g km-1")
    }
    class(ef) <- c("EmissionFactors",class(ef))
    efx <- ef
  } else if ( is.data.frame(x) ) {
    ef <- x
    for(i in 1:ncol(ef)){
      ef[,i] <- ef[,i]*units::as_units("g km-1")
    }
    class(ef) <- c("EmissionFactors",class(ef))
  } else if ( class(x) == "units" ) {
    ef <- x
    warning("Check units are g/km")
  } else if( class(x) == "numeric" | class(x) == "integer" ) {
    ef <- x*units::as_units("g km-1")
  }
  return(ef)
}

#' @rdname EmissionFactors
#' @method print EmissionFactors
#' @export
print.EmissionFactors <- function(x, ...) {
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
  if (ncol(ef) >= 1 & ncol(ef) <= 3) {
    graphics::par(mfrow=c(1, ncol(ef)), tcl = -0.5)
  } else if (ncol(ef) == 4) {
    graphics::par(mfrow=c(2, 2), tcl = -0.5)
  } else if (ncol(ef) >= 5 & ncol(ef) <= 6 ) {
    graphics::par(mfrow=c(2, 3), tcl = -0.5)
  } else if (ncol(ef) >= 7 & ncol(ef) <= 9 ) {
    graphics::par(mfrow=c(3, 3), tcl = -0.5)
  } else {
    message("Plotting first 9 plots")
    graphics::par(mfrow=c(3, 3), tcl = -0.5)
  }
  nc <- ifelse(ncol(ef) <= 9, ncol(ef), 9)
  for (i in 1:nc) {
    graphics::plot(ef[,i], type = "l", ...)
  }
  graphics::par(mfrow=c(1,1))
}
