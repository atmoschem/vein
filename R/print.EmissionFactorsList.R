#' Printing class "EmissionFactors"
#'
#' @description A simple method for printing objects with class
#' "EmissionFactors".
#'
#' @param ef Object with class "EmissionFactors"
#' @param all when T call method print.data.frame \code{\link{print.data.frame}}.
#' When F applies a  default print to each column
#' @return Print method
#' @export
#' @examples \dontrun{
#' # Do not run
#' data(fe2015)
#' names(fe2015)
#' class(fe2015)
#' df <- fe2015[fe2015$Pollutant=="CO", c(ncol(fe2015)-1,ncol(fe2015))]
#' ef1 <- as.EmissionFactors(df)
#' print(ef1)
#' print(ef1, all = F)

#' }
print.EmissionFactorsList <- function(ef, default=F,  ...) {
  if ( default == TRUE ) {
    print.listof(ef)
  } else if ( is.function( ef[[1]] ) ){ #dont work
    cat("This EmissionFactorsList has", length(ef),
        "functions")
  } else if ( is.list(ef) && is.list(ef[[1]]) ) {
    cat("This EmissionFactorsList has ", length(ef), "lists\n")
    cat("First has",length(ef[[1]]), "functions\n")
    cat("Last has", length(ef[[length(ef)]]), "functions")
  }
}
