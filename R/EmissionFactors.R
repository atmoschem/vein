#' Construction function for class "EmissionFactors"
#'
#' @description \code{EmissionFactors} returns a tranformed object with class
#' "EmissionFactors" and units g/km.
#'
#' @return Objects of class "EmissionFactors" or "units"
#'
#' @param x Object with class "data.frame", "matrix" or "numeric"
#' @param pal Palette of colors available or the number of the position
#' @param object object with class "EmissionFactors'
#' @param rev Logical; to internally revert order of rgb color vectors.
#' @param fig1 par parameters for fig, \code{\link{par}}.
#' @param mai1 par parameters for mai, \code{\link{par}}.
#' @param fig2 par parameters for fig, \code{\link{par}}.
#' @param mai2 par parameters for mai, \code{\link{par}}.
#' @param fig3 par parameters for fig, \code{\link{par}}.
#' @param mai3 par parameters for mai, \code{\link{par}}.
#' @param ... ignored
#' @importFrom units as_units
#' @importFrom graphics par plot abline
#' @importFrom fields image.plot
#' @importFrom cptcity cpt
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
plot.EmissionFactors <- function(x,
                                 pal = "mpl_viridis",
                                 rev = TRUE,
                                 fig1 = c(0,0.8,0,0.8),
                                 fig2 = c(0,0.8,0.55,1),
                                 fig3 = c(0.7,1,0,0.8),
                                 mai1 = c(0.2, 0.82, 0.82, 0.42),
                                 mai2 = c(1.3, 0.82, 0.82, 0.42),
                                 mai3 = c(0.7, 0.62, 0.82, 0.42),
                                 ...) {
  # ef <- x
  # if (ncol(ef) >= 1 & ncol(ef) <= 3) {
  #   graphics::par(mfrow=c(1, ncol(ef)), tcl = -0.5)
  # } else if (ncol(ef) == 4) {
  #   graphics::par(mfrow=c(2, 2), tcl = -0.5)
  # } else if (ncol(ef) >= 5 & ncol(ef) <= 6 ) {
  #   graphics::par(mfrow=c(2, 3), tcl = -0.5)
  # } else if (ncol(ef) >= 7 & ncol(ef) <= 9 ) {
  #   graphics::par(mfrow=c(3, 3), tcl = -0.5)
  # } else {
  #   message("Plotting first 9 plots")
  #   graphics::par(mfrow=c(3, 3), tcl = -0.5)
  # }
  # nc <- ifelse(ncol(ef) <= 9, ncol(ef), 9)
  # for (i in 1:nc) {
  #   graphics::plot(ef[,i], type = "l", ...)
  # }
  # graphics::par(mfrow=c(1,1))
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
