#' @export
print.EmissionFactors <- function(ef, ...) {
  cat("Result for EmissionFactors")
  print(unclass(ef),  ...)
}
