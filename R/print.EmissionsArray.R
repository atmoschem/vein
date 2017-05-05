#' @export
print.EmissionsArray <- function(e, default = F, ...) {
  if ( default == TRUE ) {
    print.default(e)
  } else if (is.array(e)) {
    cat("This EmissionsArray has\n", dim(e)[1], "streets\n",
        dim(e)[2], "vehicle categories\n", dim(e)[3], "hours\n",
        dim(e)[4], "days\n")
    print(utils::head(e))
  }
}
