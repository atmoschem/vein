#' @export
#' @name print.EmissionFactors
print.EmissionFactors <- function(ef, all = TRUE) {
  if(all ==TRUE) {
    print.data.frame(ef)
  } else {
    sapply(ef, print, zero.print=".")
  }
}
