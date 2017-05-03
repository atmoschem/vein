#' Printing class "EmissionFactorsList"
#'
#' @description A simple method for printing objects with class
#' "EmissionFactorsList".
#'
#' @param ef Object with class "EmissionFactors"
#' @param default when T call method print.listof \code{\link{print.listof}}.
#' When F applies returns messages with type of nested lists. In vein it is
#' assumed that "EmissionFactorsList" are list of functions. The number
#' of elements in the list is the number of function that apply to a specific
#' type of vehicle. For example, an "EmissionFactorsList" with 30 functions
#' applies to a "Vehicles" data.frame with 30 categories. The function
#' \code{\link{emis}} relates both type of objects.
#' @seealso \code{\link{emis}}
#' @return Print method
#' @export
#' @examples \dontrun{
#' # Do not run
#' data(fe2015)
#' names(fe2015)
#' class(fe2015)
#' df <- fe2015[fe2015$Pollutant=="CO", c(ncol(fe2015)-1,ncol(fe2015))]
#' ef2 <- as.EmissionFactors(df, lfx = T)
#' ef2
#' ef2[[1]]
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
