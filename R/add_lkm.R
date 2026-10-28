#' Construction function to add unit km
#'
#' @family Add distance unitts
#'
#' @description \code{add_lkm} just  add unit 'km' to different R objects
#'
#' @return Objects of class "data.frame" or "units"
#'
#' @param x Object with class "data.frame", "matrix",  "numeric", "integer"
#' or "units". Objects (or columns) with class "units" are converted to
#' 'km'.
#' @importFrom units as_units
#'
#' @examples \dontrun{
#' a <- add_lkm(rnorm(100)*10)
#' plot(a)
#' b <- add_lkm(matrix(rnorm(100)*10, ncol = 10))
#' print(head(b))
#' }
#' @export
add_lkm <- function(x) {
  to_u <- function(v) {
    if(inherits(v, "units")) {
      units::set_units(v, "km", mode = "standard")
    } else {
      v*units::as_units("km")
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

#' Construction function to add unit miles
#'
#' @family Add distance unitts
#'
#' @description \code{add_miles} just  add unit 'miles' to different R objects
#'
#' @return Objects of class "data.frame" or "units"
#'
#' @param x Object with class "data.frame", "matrix",  "numeric", "integer"
#' or "units". Objects (or columns) with class "units" are converted to
#' 'miles'.
#' @importFrom units as_units
#'
#' @examples \dontrun{
#' a <- add_miles(rnorm(100)*10)
#' plot(a)
#' b <- add_miles(matrix(rnorm(100)*10, ncol = 10))
#' print(head(b))
#' }
#' @export
add_miles <- function(x) {
  to_u <- function(v) {
    if(inherits(v, "units")) {
      units::set_units(v, "miles", mode = "standard")
    } else {
      v*units::as_units("miles")
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
