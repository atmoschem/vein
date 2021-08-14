#' Construction function to add unit km
#'
#' @family Add distance unitts
#'
#' @description \code{add_lkm} just  add unit 'km' to different R objects
#'
#' @return Objects of class "data.frame" or "units"
#'
#' @param x Object with class "data.frame", "matrix",  "numeric" or "integer"
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
  if ( is.matrix(x) ) {
    e <- as.data.frame(x)
    for(i in 1:ncol(e)){
      e[,i] <- e[,i]*units::as_units("km")
    }
  } else if ( is.data.frame(x) ) {
    e <- x
    for(i in 1:ncol(x)){
      e[,i] <- e[,i]*units::as_units("km")
    }
  } else if( class(x) == "numeric" | class(x) == "integer") {
    e <- x*units::as_units("km")
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
#' @param x Object with class "data.frame", "matrix",  "numeric" or "integer"
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
  if ( is.matrix(x) ) {
    e <- as.data.frame(x)
    for(i in 1:ncol(e)){
      e[,i] <- e[,i]*units::as_units("miles")
    }
  } else if ( is.data.frame(x) ) {
    e <- x
    for(i in 1:ncol(x)){
      e[,i] <- e[,i]*units::as_units("miles")
    }
  } else if( class(x) == "numeric" | class(x) == "integer") {
    e <- x*units::as_units("miles")
  }
  return(e)
}
