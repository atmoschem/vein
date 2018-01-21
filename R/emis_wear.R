#' Emission estimation from tyre, break and road surface wear
#'
#' @description \code{emis_wear} estimates wear emissions. The sources are tyres,
#' breaks and road surface.
#'
#' @param veh Object of class "Vehicles"
#' @param lkm Length of the road
#' @param ef list of emission factor functions class "EmissionFactorsList",
#' length equals to hours.
#' @param agemax Age of oldest vehicles for that category
#' @param profile Numerical or dataframe with nrows equal to 24 and ncol
#' 7 day of the week
#' @param hour Number of considered hours in estimation
#' @param day Number of considered days in estimation
#' @return emission estimation  g/h
#' @references Ntziachristos and Boulter 2016. Automobile tyre and break wear
#' and road abrasion. In: EEA, EMEP. EEA air pollutant emission inventory
#' guidebook-2009. European Environment Agency, Copenhagen, 2016
#' @export
#' @examples \dontrun{
#' # Do not run
#' }
emis_wear <- function (veh, lkm, ef, agemax = ncol(veh), profile, hour = 1, day = 1) {
  veh <- as.data.frame(veh)
  lkm <- as.numeric(lkm)
    d <-  simplify2array(
    lapply(1:day,function(j){
      simplify2array(
        lapply(1:hour,function(i){
          simplify2array(
            lapply(1:agemax, function(k){
              veh[, k]*profile[i,j]*lkm*ef[[i]]
            })
          )
        })
      )
    })
  )
  return(EmissionsArray(d))
}
