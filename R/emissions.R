#' Emissions estimation hourly and daily
#'
#' The vehicular emissions are estimated as the product of the vehicles on a
#' road, length of the road (units depend on the emission factor), emission
#' factor avaliated at the respective speed. \eqn{E = VEH*LENGTH*EF(speed)}
#'
#' @param veh Numeric vector with length of elements equals to number of streets
#' @param lkm Length of each link
#' @param ef Emission factor function
#' @param speed Speed. It can be a numeric vector or a list.
#' @param agemax Age of oldest vehicles for that category
#' @param day When false returns a numeric factor with one hour emissions, when
#' TRUE expects emission factor as a list with number of elements equals to the
#' number of respective vehicular category and speed as list with length equals
#' to 24, one per hour, returning an array
#' @param k Factor to extrapolate the vehicular flux to other hours
#' @return emissions estimation g/h
#' @examples  \dontrun{
#' # Do not run
#' pc <- rnorm(n = 100, mean = 500, sd = 100)
#' lkm <-  rnorm(n = 100, mean = 10, sd = 2)
#' ef <- ef_ldv_speed(v = "PC",t = "PRE_ECE", cc = "ALL", f = "G",
#'                     eu = "PRE", p = "CO")
#'
#' emi  <- emissions(veh = pc, lkm = lkm, ef = ef, speed = 30 )
#' }
emissions <- function (veh, lkm, ef, speed, agemax = 40, day = F,  k = 1) {
  if (day == T) {
    arr  <- simplify2array(
      lapply(1:24, function(j){
        simplify2array(
          lapply(1:agemax, function(i){
            veh[, i]*k*lkm*ef[[i]](speed[[j]])
            })
          )
        })
      )
    return(arr)
  } else if (day == F) {
    emi <- veh*k*lkm*ef(speed)
    return(emi)
    }
}
