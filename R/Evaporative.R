#' Construction function for class "Evaporative"
#'
#' @description \code{Evaporative} returns a tranformed object with class "Evaporative" and
#' units g/day. This class represents the daily emissions presented by
#' Mellios G and Ntziachristos (2016) Gasoline evaporation, Tier 2. Eventually
#' it will be incorporated the techniques of Tier 3.
#'
#'
#' @return Objects of class "Evaporative" or "units"
#'
#' @param x Object with class "numeric"
#' @param object Object with class "Evaporative"
#' @param ... ignored
#' @rdname Evaporative
#' @aliases Evaporative print.Evaporative summary.Evaporative
#' plot.Evaporative
#' @examples {
#' ef1 <- ef_evap(ef = "erhotc",v = "PC", cc = "<=1400", dt = "0_15", ca = "no")
#' ef1
#' }
#' @export
Evaporative <- function(x, ...) {
  ev <- x
if ( is.numeric(ev) ) {
    ev <- ev*units::as_units("g d-1")
    class(ev) <- c("Evaporative",class(ev))
  }
  return(ev)
}

#' @rdname Evaporative
#' @method print Evaporative
#' @export
print.Evaporative <- function(x, ...) {
  cat("Result for Evaporative")
  print(x,  ...)
}

#' @rdname Evaporative
#' @method summary Evaporative
#' @export
summary.Evaporative <- function(object, ...) {
  cat("Evaporative emissions in study area = \n")
  print(summary(unclass(object)))
}

#' @rdname Evaporative
#' @method plot Evaporative
#' @export
plot.Evaporative <- function(x,  ...) {
  ev <- x
    NextMethod("plot", ev, ...)
}
