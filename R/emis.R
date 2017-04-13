#' Emissions estimation hourly for the of the week
#'
#' The vehicular emissions are estimated as the product of the vehicles on a
#' road, length of the road, emission factor avaliated at the respective speed.
#'  \eqn{E = VEH*LENGTH*EF(speed)}
#'
#' @param veh Numeric vector with length of elements equals to number of streets
#' @param lkm Length of each link
#' @param ef List of functions of emission factors
#' @param speed List of speeds
#' @param agemax Age of oldest vehicles for that category
#' @param profile Numerical or dataframe with nrows equal to 24 and ncol 7 day of the week
#' @param hour Number of considered hours in estimation
#' @param day Number of considered days in estimation
#' @param array When FALSE produces a dataframe of the estimation. When TRUE expects a
#' profile as a dataframe producing an array with dimensions (streets x columns x hours x days)
#' @return emission estimation  g/h
#' @export
#' @examples \dontrun{
#' # Do not run
#' pc <- rnorm(n = 100, mean = 500, sd = 100)
#' lkm <-  rnorm(n = 100, mean = 10, sd = 2)
#' ef <- ef_ldv_speed(v = "PC",t = "PRE_ECE", cc = "ALL", f = "G",
#'                     eu = "PRE", p = "CO")
#'
#' emi  <- emis(veh = pc, lkm = lkm, ef = ef, speed = 30 )
#' }
emis <- function (veh, lkm, ef, speed, agemax, profile, hour = 1, day = 1,
                  array = F) {
  if(array == F){
  lista <- lapply(1:day,function(j){
    lapply(1:hour,function(i){
      lapply(1:agemax, function(k){
        veh[, k]*profile[i,j]*lkm*ef[[k]](speed[[j]][[i]])
        })
      })
    })
    return(lista)
  } else {
  d <-  simplify2array(
    lapply(1:day,function(j){
      simplify2array(
        lapply(1:hour,function(i){
          simplify2array(
            lapply(1:agemax, function(k){
              veh[, k]*profile[i,j]*lkm*ef[[k]](speed[[j]][[i]])
              })
            )
          })
        )
      })
    )
  return(d)
  }
}

