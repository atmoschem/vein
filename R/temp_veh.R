#' Expanded Vehicles data.frame by hour
#'
#' @description \code{\link{temp_veh}} multiplies
#' vehicles with temporal factor
#'
#' @param x Vehicles data.frame
#' @param tfs temporal factor
#' @param array Logical, to return an array
#' @seealso \code{\link{temp_fact}}
#' @return data.table
#' @importFrom data.table rbindlist
#' @export
#' @examples \dontrun{
#' data(net)
#' data(pc_profile)
#' x <- age_ldv(x = net$ldv)
#' dx <- temp_veh(x = x, tfs = pc_profile[[1]])
#' plot(Vehicles(as.data.frame(dx[, 1:50])))
#' dx2 <- temp_veh(x = x,
#'                 tfs = pc_profile[[1]],
#'                 array = TRUE)
#' plot(EmissionsArray(dx2))
#' }
temp_veh <- function(x,
                     tfs,
                     array = FALSE){
  lapply(seq_along(tfs), function(i) {
    x <- x*tfs[i]
    if(!array) x$Hour <- seq_along(tfs)[i]
    x
  }) -> xx

  if(array) {
    lx <- unlist(lapply(xx, unlist))

    a <- array(data = lx,
               dim = c(nrow(x),
                       ncol(x),
                       length(tfs)))

    return(a)
  } else {
    return(data.table::rbindlist(xx))

  }

}
