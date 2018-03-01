#' Construction function for class "EmissionFactorsList"
#'
#' @description \code{EmissionFactorsList} returns a tranformed object with
#' class"EmissionsFactorsList".
#'
#' @return Objects of class "EmissionFactorsList"
#'
#' @param x Object with class "list"
#' @param object Object with class "EmissionFactorsList"
#' @param default Logical value. When TRUE prints default list, when FALSE
#' prints messages with description of list
#' @param ... ignored
#' @rdname EmissionFactorsList
#' @aliases EmissionFactorsList print.EmissionFactorsList
#' summary.EmissionFactorsList plot.EmissionFactorsList
#' @examples {
#' data(fe2015)
#' names(fe2015)
#' class(fe2015)
#' df <- fe2015[fe2015$Pollutant=="CO", c(ncol(fe2015)-1,ncol(fe2015))]
#' ef1 <- EmissionFactorsList(df)
#' class(ef1)
#' length(ef1)
#' length(ef1[[1]])
#' summary(ef1)
#' ef1
#' }
#' @export
EmissionFactorsList <- function(x, ...) {
  if ( is.matrix(x) || is.data.frame(x) ) {
    efx <- lapply(1:ncol(x), function(i) {
      lapply(1:nrow(x), function(j) {
        function(V) x[j,i]
      })
    })
    class(efx) <- c("EmissionFactorsList",class(x))
  } else  if ( is.numeric(x) ) {
    efx <- lapply(1:length(x), function(i) {function(V) x[i] })
    class(efx) <- c("EmissionFactorsList",class(efx))
  } else if ( is.list(x) && is.function(x[[1]]) ) {
    efx <- x
    class(efx) <- c("EmissionFactorsList",class(x))
  }
  return(efx)
}

#' @rdname EmissionFactorsList
#' @method print EmissionFactorsList
#' @export
print.EmissionFactorsList <- function(x, ..., default = FALSE) {
  if ( default ) {
    print.listof(x)
  } else if ( is.function( x[[1]] ) ){
    cat("This EmissionFactorsList has ", length(x),
        " functions")
  } else if ( is.list(x) && is.list(x[[1]]) ) {
    cat("This EmissionFactorsList has ", length(x), " lists\n")
    cat("First has ",length(x[[1]]), " functions\n")
    cat("Last has ", length(x[[length(x)]]), " functions")
  }
}

#' @rdname EmissionFactorsList
#' @method summary EmissionFactorsList
#' @export
summary.EmissionFactorsList <- function(object, ...) {
  ef <- object
  if ( is.function( ef[[1]] ) ){
    cat("This EmissionFactorsList has", length(ef),
        "functions")
    summary(ef[[1]])
  } else if ( is.list(ef) && is.list(ef[[1]]) ) {
    cat("This EmissionFactorsList has ", length(ef), "lists\n")
    cat("First has",length(ef[[1]]), "functions\n")
    cat("Last has", length(ef[[length(ef)]]), "functions")
  }
}

#' @rdname EmissionFactorsList
#' @method plot EmissionFactorsList
#' @export
plot.EmissionFactorsList <- function(x, ...) {
  ef <- x
  if ( is.function( ef[[1]] ) ){
    cat("This EmissionFactorsList has", length(ef),
        "functions")
    graphics::plot(unlist(lapply(1:length(ef), function(i) ef[[i]](34) )))
  } else  {
    cat("Try other methods")
  }
}


