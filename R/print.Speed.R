#' Printing class "Speed"
#'
#' @description A simple method for printing objects with class "Speed".
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
#' df <- netspeed(pcw, net$ps,net$ffs, net$capacity, net$lkm, alpha = 1,isList=F)
#' class(df)
#' df
#' }
print.Speed <- function(spd, all=TRUE,  ...) {
  if(all ==TRUE) {
    print.data.frame(spd)
  } else {
    sapply(spd, print, zero.print=".")
  }
}
