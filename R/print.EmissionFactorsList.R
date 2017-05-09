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
