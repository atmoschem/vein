#' @export
print.Emissions <- function(e, ...) {
  cat("Result for Emissions ")
  print(unclass(e),  ...)
}
