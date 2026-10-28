#' Construction function for Celsius temperature
#'
#' @description \code{celsius} just  add unit celsius to different R objects
#'
#' @return Objects of class "data.frame" or "units"
#'
#' @param x Object with class "data.frame", "matrix",  "numeric", "integer"
#' or "units". Objects (or columns) with class "units" are converted to
#' 'degC'.
#' @importFrom units as_units
#'
#' @examples {
#' a <- celsius(rnorm(100)*10)
#' plot(a)
#' b <- celsius(matrix(rnorm(100)*10, ncol = 10))
#' print(head(b))
#' }
#' @export
celsius <- function(x) {
  to_u <- function(v) {
    if(inherits(v, "units")) {
      units::set_units(v, "degC", mode = "standard")
    } else {
      v*units::as_units("degC")
    }
  }
  if ( is.matrix(x) ) {
    e <- as.data.frame(x)
    for(i in 1:ncol(e)){
      e[,i] <- to_u(e[,i])
    }
  } else if ( is.data.frame(x) ) {
    e <- x
    for(i in 1:ncol(x)){
      e[,i] <- to_u(e[,i])
    }
  } else if( is.numeric(x) ) {
    e <- to_u(x)
  } else {
    stop("'x' must be of class 'data.frame', 'matrix', 'units', 'numeric' or 'integer'")
  }
  return(e)
}
