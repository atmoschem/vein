#' Estimation of VKM
#'
#' @description \code{vkm} consists in the product of the number of vehicles and
#' the distance driven by these vehicles in km. This function reads hourly
#' vehiles and then extrapolates the vehicles
#'
#' @param veh Numeric vector with number of vehicles per street
#' @param lkm Length of each link (km)
#' @param profile Numerical or dataframe with nrows equal to 24 and ncol 7 day of the week
#' @param hour Number of considered hours in estimation
#' @param day Number of considered days in estimation
#' @param array When FALSE produces a dataframe of the estimation. When TRUE expects a
#' profile as a dataframe producing an array with dimensions (streets x hours x days)
#' @return emission estimation  of vkm
#' @export
#' @examples {
#' # Do not run
#' pc <- lkm <- abs(rnorm(10,1,1))*100
#' pro <- matrix(abs(rnorm(24*7,0.5,1)), ncol=7, nrow=24)
#' vkms  <- vkm(veh = pc, lkm = lkm, profile = pro)
#' class(vkms)
#' dim(vkms)
#' }
vkm <- function (veh,
                 lkm,
                 profile,
                 hour = nrow(profile),
                 day = ncol(profile),
                 array = T) {
  veh <- as.numeric(veh)
  lkm <- as.numeric(lkm)
  if(array == F){
    lista <- lapply(1:day,function(j){
      lapply(1:hour,function(i){
        veh*profile[i,j]*lkm
      })
    })
    return(lista)
  } else {
    d <- simplify2array(lapply(1:day, function(j) {
      simplify2array(lapply(1:hour, function(i) {
        veh* profile[i, j]*lkm
      }))
    }))
    return(d)
  }
}

