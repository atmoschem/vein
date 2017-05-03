#' Printing class "Vehicles"
#'
#' @description A simple method for printing objects with class "vehicles".
#'
#'
#' @param q traffic data per each link
#' @param pro expansion factors data-frames
#' @return Print method
#' @param all when T call method print.data.frame \code{\link{print.data.frame}}.
#' When F applies a  default print to each column
#' @export
#' @examples \dontrun{
#' # Do not run
#' data(net)
#' LT_B5 <- age_hdv(x = lt,name = "LT_B5")
#' LT_B5
#' }
print.Vehicles <- function(veh, all=TRUE,  ...) {
  if(all ==TRUE) {
    print.data.frame(veh)
  } else {
    sapply(veh, print, zero.print=".")
  }
}

