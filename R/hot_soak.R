#' Estimation of average daily  hot-soak evaporative emissions
#'
#' @description \code{hot_soak} estimates of evaporative emissions from EMEP/EEA
#' emisison guidelines
#'
#' @param x Mean number of trips per vehicle per day
#' @param carb fraction of gasoline vehicles with carburator or fuel return system
#' @param p Fraction of trips finished with hot engine
#' @param eshotc average daily hot-soak evaporative factor for vehicles with
#' carburator or fuel return system
#' @param eswarmc average daily cold-warm-soak evaporative factor for vehicles
#' with carburator or fuel return system
#' @param eshotfi average daily hot-soak evaporative factor for vehicles
#' with fuel injection and returnless fuel systems
#' @return numeric vector of emission estimation in grams
#' @name running_losses-deprecated
#' @seealso \code{\link{vein-deprecated}}
#' @keywords internal
NULL

#' @rdname vein-deprecated
#' @section \code{Evaporative}:
#' For \code{running_losses}, use \code{\link{emis_evap}}.
#'
#' @references Mellios G and Ntziachristos 2016. Gasoline evaporation. In:
#' EEA, EMEP. EEA air pollutant emission inventory guidebook-2009. European
#' Environment Agency, Copenhagen, 2009
#' @export
#' @examples {
#' # Do not run
#' }
hot_soak <- function(x,carb,p,eshotc,eswarmc,eshotfi) {
  .Deprecated("emis_evap")
  "Evaporative emissions"
}
