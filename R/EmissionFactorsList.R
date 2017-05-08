#' Construction function for class "EmissionFactorsList"
#'
#' @description Returns a tranformed object with class"EmissionsFactorsList".
#'
#' @return Objects of class "EmissionFactorsList"
#'
#' @param x Object with class "list"
#' @param object Object with class "EmissionFactorsList"
#' @param ... ignored
#' @rdname EmissionFactorsList
#' @aliases EmissionFactorsList print.EmissionFactorsList
#' summary.EmissionFactorsList plot.EmissionFactorsList
#' @examples \dontrun{
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
  ef <- x
  if ( is.matrix(ef) || is.data.frame(ef) ) {
    efx <- lapply(1:ncol(ef), function(i) {
      lapply(1:nrow(ef), function(j) {
        function(V) ef[j,i]
      })
    })
    class(efx) <- c("EmissionFactorsList",class(efx))
  } else  if ( is.numeric(ef) ) {
    efx <- lapply(1:length(ef), function(i) {function(V) ef[i] })
    class(efx) <- c("EmissionFactorsList",class(efx))
  } else if ( is.list(ef) && is.function(ef[[1]]) ) {
    class(ef) <- c("EmissionFactorsList",class(ef))
    efx <- ef
  }
  return(efx)
}

#' @rdname EmissionFactorsList
#' @method print EmissionFactorsList
#' @export
print.EmissionFactorsList <- function(x, ...) {
  ef <- x
  if ( is.function( ef[[1]] ) ){
    cat("This EmissionFactorsList has", length(ef),
        "functions\n")
    print(ef[[1]])
    cat("... ")
  } else if ( is.list(ef) && is.list(ef[[1]]) ) {
    cat("This EmissionFactorsList has ", length(ef), "lists\n")
    cat("First has",length(ef[[1]]), "functions\n")
    cat("Last has", length(ef[[length(ef)]]), "functions\n")
    print(ef[[1]][[1]])
    cat("\n ... ")
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


