#' Expansion of hourly traffic data
#'
#' @description \code{temp_fact} is a matrix multiplication between traffic and
#' hourly expansion data-frames to obtain a data-frame of traffic
#' at each link to every hour
#'
#' @param q Numeric; traffic data per each link
#' @param pro Numeric; expansion factors data-frames
#' @param net SpatialLinesDataFrame or Spatial Feature of "LINESTRING"
#' @param time Character to be the time units as denominator, eg "1/h"
#' @return data-frames of expanded traffic or sf.
#' @importFrom sf st_sf st_as_sf
#' @export
#' @examples \dontrun{
#' # Do not run
#' data(net)
#' data(pc_profile)
#' pc_week <- temp_fact(net$ldv+net$hdv, pc_profile)
#' plot(pc_week)
#' pc_weeksf <- temp_fact(net$ldv+net$hdv, pc_profile, net = net)
#' plot(pc_weeksf)
#' }
temp_fact <- function(q, pro, net, time) {
  if (missing(q) | is.null(q)) {
    stop("No traffic data")
  }
  if(!missing(time)){
    df <- Vehicles(as.data.frame(as.matrix(q) %*% matrix(unlist(pro), nrow=1)), time = time)
  } else {
    df <- Vehicles(as.data.frame(as.matrix(q) %*% matrix(unlist(pro), nrow=1)))

  }

   if(!missing(net)){
  netsf <- sf::st_as_sf(net)
  speed <- sf::st_sf(df, geometry = sf::st_geometry(netsf))
  return(speed)
  } else {
    return(df)
  }
}
