#' Emissions estimation hourly for the of the week
#'
#' The vehicular emissions are estimated as the product of the vehicles on a
#' road, length of the road (units depend on the emission factor), emission
#' factor avaliated at the respective speed. \eqn{E = VEH*LENGTH*EF(speed)}
#'
#' @param veh Numeric vector with length of elements equals to number of streets
#' @param lkm Length of each link
#' @param ef List of functions of emission factors
#' @param speed List of speeds
#' @return emission estimation g/h
#' @examples \dontrun{
#' # Do not run
#' pc <- rnorm(n = 100, mean = 500, sd = 100)
#' lkm <-  rnorm(n = 100, mean = 10, sd = 2)
#' ef <- ef_ldv_speed(v = "PC",t = "PRE_ECE", cc = "ALL", f = "G",
#'                     eu = "PRE", p = "CO")
#'
#' emi  <- emissions_24(veh = pc, lkm = lkm, ef = ef, speed = 30 )
#' }
emissions_24 <- function(veh, lkm, ef, speed) {
  if(!is.list(veh) |  !is.list(ef) | !is.list(speed) ) {
    stop("veh, ef and speed must be lists")
    } else if (length(veh)!= 24 | length(speed)!= 24  ) {
      stop("length veh and speed should be 24")
      } else if ( length(ef) != ncol(veh[[1]]) ){
        stop("length of ef should be the same as nrow of veh elements (df)")
        } else {
          lapply(1:24, function(j) {
            as.data.frame(lapply(1:length(ef),function(i) {
              veh[[j]][,i]*lkm*ef[[i]](speed[[j]])
              })
            )
            }
          )
        }
  }
