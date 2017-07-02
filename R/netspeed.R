#' Calculate speeds of traffic network
#'
#' Creates a dataframe of speeds fir diferent hours and each link based on
#' morning rush traffic data
#'
#' @param q Data-frame of traffic flow to each hour (veh/h)
#' @param ps Peak speed (km/h)
#' @param ffs Free flow speed (km/h)
#' @param cap Capacity of link (veh/h)
#' @param lkm Distance of link (km)
#' @param alpha Parameter of BPR curves
#' @param beta Parameter of BPR curves
#' @param isList Logical to specify type of return, list or data-frame
#' @param distance Character specifying the units for distance. Default is "km"
#' @param time Character specifying the units for time Default is "h"
#' @return dataframe or list of speeds with units
#' @export
#' @examples \dontrun{
#' # Do not run
#' data(net)
#' data(pc_profile)
#' pc_week <- temp_fact(net$ldv+net$hdv, pc_profile)
#' df <- netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm)
#' class(df)
#' plot(df) #plot of the average speed at each hour, +- sd
#' }
netspeed <- function (q, ps, ffs, cap, lkm, alpha=0.15, beta=4, isList=FALSE,
                      distance = "km", time="h"){
  if(missing(q) | is.null(q)){
    stop(print("No vehicles"))
  } else if (isList==FALSE){
    qq <- as.data.frame(q)
    for (i  in 1:ncol(qq) ) {
      qq[,i] <- as.numeric(qq[,i])
    }
    ps <- as.numeric(ps)
    ffs <- as.numeric(ffs)
    cap <- as.numeric(cap)
    lkm <- as.numeric(lkm)
    dfv <- as.data.frame(do.call("cbind",(lapply(1:ncol(qq), function(i) {
      lkm/(lkm/ffs*(1 + alpha*(qq[,i]/cap)^beta))
    }))))
    # dfv[,8] <- ps
    names(dfv) <- unlist(lapply(1:ncol(q), function(i) paste0("S",i)))
    dfv <- Speed(dfv, distance = distance, time = time)
    return(dfv)
  } else if (isList==TRUE){
    qq <- as.data.frame(q)
    for (i  in 1:ncol(qq) ) {
      qq[,i] <- as.numeric(qq[,i])
    }
    ps <- as.numeric(ps)
    ffs <- as.numeric(ffs)
    cap <- as.numeric(cap)
    lkm <- as.numeric(lkm)
    dfv <- as.data.frame(do.call("cbind",(lapply(1:ncol(qq), function(i) {
      lkm/(lkm/ffs*(1 + alpha*(qq[,i]/cap)^beta))
    }))))
    # dfv[,8] <- ps
    names(dfv) <- unlist(lapply(1:ncol(q), function(i) paste0("S",i)))
    ldfv <- lapply(0:(ncol(dfv)/24-1),function(i) {
      as.list(dfv[,(1:24)+i*24])
    })
    return(ldfv)
  }
}
