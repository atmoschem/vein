#' @title Estimation with long format
#' @family China
#' @name emis_long
#' @description Emissions estimates
#' @param x Vehicles data.frame. x repeats down for each hour
#' @param lkm Length of each link in km. lkm repeats down for each hour
#' @param ef data.frame. ef repeats down for each hour
#' @param tfs temporal factor
#' @param speed Speed data.frame (nrow x)
#' @param verbose Logical to show more info
#' @param array Logical to return EmissionsArray or not
#' @return long data.frame
#' @importFrom data.table rbindlist
#' @export
#' @examples {
#' data(net)
#' net <- net[1:100, ]
#' data(pc_profile)
#' x <- age_veh(net$ldv)
#' pc_week <- temp_fact(net$ldv+net$hdv, pc_profile[[1]])
#' df <- netspeed(pc_week,
#'                net$ps,
#'                net$ffs,
#'                net$capacity,
#'                net$lkm,
#'                alpha = 1)
#'
#' s  <- do.call("rbind",lapply(1:ncol(df), function(i) {
#'  as.data.frame(replicate(ncol(x), df[, i]))
#' }))
#'
#' ef <- ef_wear(wear = "tyre",
#'               type = "PC",
#'               pol = "PM10",
#'               speed = as.data.frame(s))
#'
#' e <- emis_long(x = x,
#'                lkm = net$lkm,
#'                ef = ef,
#'                tfs = pc_profile[[1]],
#'                speed = df)
#'
#' ae <- emis_long(x = x,
#'                lkm = net$lkm,
#'                ef = ef,
#'                tfs = pc_profile[[1]],
#'                speed = df,
#'                array = TRUE)
#' }
emis_long <- function(x,
                      lkm,
                      ef,
                      tfs,
                      speed,
                      verbose = TRUE,
                      array = FALSE){

  # checks
  if(nrow(x) != length(lkm)) stop("length lkm and nrow x must be equal")
  if(length(tfs) != ncol(speed)) stop("length tfs and ncol speed must be equal")
  if(ncol(ef) != ncol(x)) stop("ncol of ef and x must be equal")
  LKM <- rep(lkm, length(tfs))
  if(nrow(ef) != length(lkm)*length(tfs)) stop("nrow of ef must be equal with length lkm time length tfs")

  # Vehicle
  if(verbose) cat("\nProcessing Vehicles\n")
  nr <- nrow(x)
  nc <- ncol(x)
  xx <- temp_veh(x = x, tfs = tfs)

  if(verbose) cat("Estimating Base EF\n")

  # speed
  s  <- data.table::rbindlist(lapply(1:ncol(speed), function(i) {
    as.data.frame(replicate(ncol(x), speed[, i]))
  }))


  if(verbose) cat("Estimating emissions\n")
  E <- Emissions(do.call("cbind", lapply(1:nc, function(i) {
    as.data.frame(ef)[, i] * as.data.frame(xx)[, i] * LKM
  })))
  # return(E)
  E$Hour <- rep(seq_along(tfs), each = nr)

  if(array) {
    lx <- split(E, E$Hour)
    lxx <- unlist(lapply(seq_along(lx), function(i) {
      unlist(lx[[i]][, 1:nc])
    }))
    a <- EmissionsArray(array(data = lxx,
                              dim = c(nr,
                                      nc,
                                      length(tfs))))
    return(a)
  } else {
    return(E)

  }


  return(x)
}
