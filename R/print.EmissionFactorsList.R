#' @export
print.EmissionFactorsList <- function(ef, default = F, ...) {
  if ( default == TRUE ) {
    print.listof(ef)
  } else if ( is.function( ef[[1]] ) ){
    cat("This EmissionFactorsList has", length(ef),
        "functions")
  } else if ( is.list(ef) && is.list(ef[[1]]) ) {
    cat("This EmissionFactorsList has ", length(ef), "lists\n")
    cat("First has",length(ef[[1]]), "functions\n")
    cat("Last has", length(ef[[length(ef)]]), "functions")
  }
}
