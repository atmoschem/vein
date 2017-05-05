#' @export
print.Speed <- function(spd, all=TRUE) {
  if(all ==TRUE) {
    print.data.frame(spd)
  } else {
    sapply(spd, print, zero.print=".")
  }
}
