#' Estimation of average running losses evaporative emissions
#'
#' @description Estimation of evaporative emissions from EMEP/EEA emisison
#' guidelines
#'
#' @param x Mean number of trips per vehicle per day
#' @param carb fraction of gasoline vehicles with carburator or fuel return system
#' @param p Fraction of trips finished with hot engine
#' @param erhotc average daily running losses evaporative factor for vehicles with
#' carburator or fuel return system
#' @param erwarmc average daily cold and warm running losses evaporative factor
#' for vehicles with carburator or fuel return system
#' @param erhotfi average daily hot running losses evaporative factor
#' for vehicles with fuel injection and returnless fuel systems
#' @return numeric vector of emission estimation in grams
#' @references Mellios G and Ntziachristos 2016. Gasoline evaporation. In:
#' EEA, EMEP. EEA air pollutant emission inventory guidebook-2009. European
#' Environment Agency, Copenhagen, 2009
#' @export
#' @examples \dontrun{
#' # Do not run
#' ev <- running_losses(x = 1:10, carb = 0, p = 1, erhot = 1, erwarmc =1,
#' erhotfi = 1)
#' }
running_losses <- function(x,carb,p,erhotc,erwarmc,erhotfi) {
  evap <- x*(carb*(p*erhotc+(1-p)*erwarmc)+(1-carb)*erhotfi)
  Evaporative(evap)
}

