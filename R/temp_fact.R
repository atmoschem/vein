#' Expansion of hourly traffic data
#'
#' @description Matrix multiplication between traffic and
#' hourly expansion data-frames to obtain a data-frame of traffic
#' at each link to every hour
#'
#' @param q traffic data per each link
#' @param pro expansion factors data-frames
#' @return data-frames of expanded traffic
#' @export
#' @examples \dontrun{
#' # Do not run
#' data(net)
#' data(pc_profile)
#' pc_week <- temp_fact(net$ldv+net$hdv, pc_profile)
#' plot(pc_week)
#' }
temp_fact <- function(q, pro) {
  if (missing(q) | is.null(q)) {
    stop("No traffic data")
  } else {
   df <- Vehicles.default(as.matrix(q) %*% matrix(unlist(pro), nrow=1))
  }
  return(df)
}
