#' @export
print.Emissions <- function(e, all = TRUE) {
  if(all ==TRUE) {
    print.data.frame(e)
  } else {
    sapply(e, print, zero.print=".")
  }
}
