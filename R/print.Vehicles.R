#' Printing class "Vehicles"
#'
#' @description A simple method for printing objects with class "Vehicles".
#'
#'
#' @param veh Objecto with class "Vehicles"
#' @param all when T call method print.data.frame \code{\link{print.data.frame}}.
#' @param ... ignored
#' @return Print method
#' When F applies a  default print to each column
#' @rdname print.Vehicles
#' @export
Vehicles <- function(veh, ...) {
  UseMethod("Vehicles", veh)
}
#' @examples \dontrun{
#' # Do not run
#' data(net)
#' LT_B5 <- age_hdv(x = lt,name = "LT_B5")
#' LT_B5
#' }
print.Vehicles <- function(veh, all=TRUE) {
  if(all ==TRUE) {
    print.data.frame(veh)
  } else {
    sapply(veh, print, zero.print=".")
  }
}

