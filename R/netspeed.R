#' Calculate speeds of traffic network
#'
#' @description \code{netspeed} Creates a dataframe of speeds for different hours
#' and each link based on morning rush traffic data
#'
#' @param q Data-frame (or numeric vector, for one hour) of traffic flow to
#' each hour (veh/h)
#' @param ps Peak speed (km/h). Only used when \code{scheme = TRUE}.
#' @param ffs Free flow speed (km/h)
#' @param cap Capacity of link (veh/h). Links with \code{cap <= 0} return NA
#' with a warning.
#' @param lkm Distance of link (km). Links with \code{lkm <= 0} return NA
#' with a warning.
#' @param alpha Parameter of BPR curves
#' @param beta Parameter of BPR curves
#' @param net SpatialLinesDataFrame or Spatial Feature of "LINESTRING"
#' @param scheme Logical to create a Speed data-frame with 24 hours and a
#' default  profile. It needs ffs and ps:
#' @param dist String indicating the units of the resulting distance in speed.
#' Default is "km". When \code{ps} and \code{ffs} have class "units", they
#' are converted to \code{dist}/h before the estimation. When they are
#' numeric, they are assumed to be in \code{dist}/h.
#' The hours of \code{scheme = TRUE} are:
#' \tabular{rl}{
#'   00:00-06:00 \tab ffs\cr
#'   06:00-07:00 \tab average between ffs and ps\cr
#'   07:00-10:00 \tab ps\cr
#'   10:00-17:00 \tab average between ffs and ps\cr
#'   17:00-20:00 \tab ps\cr
#'   20:00-22:00 \tab average between ffs and ps\cr
#'   22:00-00:00 \tab ffs\cr
#' }
#' @return dataframe speeds with units or sf. The speed is estimated with the
#' BPR curve: ffs / (1 + alpha * (q / cap)^beta). The names of the columns
#' are "S1", "S2", ..., one for each hour.
#' @importFrom sf st_sf st_as_sf
#' @export
#' @examples {
#' data(net)
#' data(pc_profile)
#' pc_week <- temp_fact(net$ldv+net$hdv, pc_profile)
#' df <- netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' class(df)
#' plot(df) #plot of the average speed at each hour, +- sd
#' # net$ps <- units::set_units(net$ps, "miles/h")
#' # net$ffs <- units::set_units(net$ffs, "miles/h")
#' # df <- netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' # class(df)
#' # plot(df) #plot of the average speed at each hour, +- sd
#' # df <- netspeed(ps = net$ps, ffs = net$ffs, scheme = TRUE)
#' # class(df)
#' # plot(df) #plot of the average speed at each hour, +- sd
#' # dfsf <- netspeed(ps = net$ps, ffs = net$ffs, scheme = TRUE, net = net)
#' # class(dfsf)
#' # head(dfsf)
#' # plot(dfsf, pal = cptcity::lucky(colorRampPalette = TRUE, rev = TRUE),
#' # key.pos = 1, max.plot = 9)
#' }
netspeed <- function (q = 1,
                      ps,
                      ffs,
                      cap,
                      lkm,
                      alpha = 0.15,
                      beta = 4,
                      net,
                      scheme = FALSE,
                      dist = "km"){
  to_dist <- function(v) {
    if(inherits(v, "units")) {
      v <- units::set_units(v, paste0(dist, "/h"), mode = "standard")
    }
    as.numeric(v)
  }

  if (scheme == FALSE & missing(q)){
    stop("No vehicles on 'q'")
  } else if (scheme == FALSE){
    qq <- as.data.frame(remove_units(q))
    for (i  in 1:ncol(qq) ) {
      qq[,i] <- as.numeric(qq[,i])
    }
    nl <- nrow(qq)
    ffs <- rep_len(to_dist(ffs), nl)
    cap <- rep_len(as.numeric(cap), nl)

    bad <- !is.na(cap) & cap <= 0
    if(any(bad)) {
      warning(sum(bad), " link(s) with 'cap' <= 0, returning NA speed")
    }
    if(!missing(lkm)) {
      lkm <- rep_len(as.numeric(lkm), nl)
      badl <- !is.na(lkm) & lkm <= 0
      if(any(badl)) {
        warning(sum(badl), " link(s) with 'lkm' <= 0, returning NA speed")
      }
      bad <- bad | badl
    }
    cap[bad] <- NA

    # BPR: lkm / (lkm/ffs*(1 + alpha*(q/cap)^beta)) = ffs/(1 + alpha*(q/cap)^beta)
    dfv <- as.data.frame(do.call("cbind",(lapply(1:ncol(qq), function(i) {
      sp <- ffs/(1 + alpha*(qq[,i]/cap)^beta)
      sp[bad] <- NA
      sp
    }))))
    names(dfv) <- paste0("S", 1:ncol(qq))

    df_speed <- Speed(dfv, dist = dist)

  } else {
    ps <- to_dist(ps)
    ffs <- to_dist(ffs)
    n <- max(length(ps), length(ffs))
    ps <- rep_len(ps, n)
    ffs <- rep_len(ffs, n)
    avs <- 0.5*(ps + ffs)

    # hours 00-06 ffs, 06-07 avg, 07-10 ps, 10-17 avg, 17-20 ps, 20-22 avg,
    # 22-24 ffs
    hs <- c(rep("ffs", 6), "avs", rep("ps", 3), rep("avs", 7), rep("ps", 3),
            rep("avs", 2), rep("ffs", 2))
    lv <- list(ffs = ffs, avs = avs, ps = ps)

    dfv <- as.data.frame(matrix(unlist(lv[hs]), nrow = n, ncol = 24))
    names(dfv) <- paste0("S", 1:24)

    df_speed <- Speed(dfv, dist = dist)

  }
  if(!missing(net)){
    netsf <- sf::st_as_sf(net)
    df_speedsf <- sf::st_sf(df_speed,
                            geometry = sf::st_geometry(netsf))
    return(df_speedsf)
  } else {
    return(df_speed)
  }
}
