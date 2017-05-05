#' Construction function for class "EmissionFactorsList"
#'
#' @description Returns a tranformed object with class"EmissionsFactorsList".
#' This functions has arguments to change of the numeric elements of the list.
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
#' ef1 <- as.EmissionFactors(df)
#' class(ef1)
#' head(ef1)
#' ef2 <- as.EmissionFactors(df, lfx = T)
#' class(ef2)
#' class(ef2[[1]])
#' }
#' @export
EmissionFactorsList <- function(x, ...) {
  ef <- x
  if ((is.matrix(ef) || is.data.frame(ef))) {
    efx <- lapply(1:ncol(ef), function(i) {
      lapply(1:nrow(ef), function(j) {
        function(V) ef[j,i]
      })
    })
    for (i in 1:length(efx)) {
      class(efx[[i]]) <- c("EmissionFactorsList",class(efx))
    }
    class(efx) <- c("EmissionFactorsList",class(efx))
  } else  if (is.numeric(ef)) {
    ef <- ef * units::parse_unit(paste0("g"," ", "km", "-1"))
    efx <- lapply(1:length(ef), function(i) {function(V) ef[i] })
    class(ef) <- c("EmissionFactorsList",class(ef))
    efx <- ef
  } else if (is.list(ef) && is.function(ef[[1]])) {
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
        "functions")
  } else if ( is.list(ef) && is.list(ef[[1]]) ) {
    cat("This EmissionFactorsList has ", length(ef), "lists\n")
    cat("First has",length(ef[[1]]), "functions\n")
    cat("Last has", length(ef[[length(ef)]]), "functions")
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
    summary(ef[[1]][[1]])
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
    plot(unlist(lapply(1:length(ef), function(i) ef[[i]](34) )))
  } else  {
    cat("Try other methods")
  }
}


