#' @export
#' @name print.Vehicles
print.Vehicles <- function(veh, all=TRUE, ...) {
  if(all ==TRUE) {
    print.data.frame(veh,  ...)
  } else {
    sapply(veh, print, zero.print=".", ...)
  }
}

