#' Printing class "Speed"
#'
#' @description A simple method for printing objects with class "Speed".
#'
#'
#' @param spd Object with class "Speed"
#' @param all when T call method print.data.frame \code{\link{print.data.frame}}.
#' When F applies a  default print to each column
#' @param ... ignored
#' @return Print method
#' @rdname print.Speed
#' @export
Speed <- function(spd, ...) {
  UseMethod("Speed", spd)
}
#' @examples \dontrun{
#' # Do not run
#' data(net)
#' data(pc_profile)
#' pcw <- temp_fact(net$ldv+net$hdv, pc_profile)
#' class(pcw)
#' df <- netspeed(pcw, net$ps,net$ffs, net$capacity, net$lkm, alpha = 1,isList=F)
#' class(df)
#' df
#' }
print.Speed <- function(spd, all=TRUE) {
  if(all ==TRUE) {
    print.data.frame(spd)
  } else {
    sapply(spd, print, zero.print=".")
  }
}
