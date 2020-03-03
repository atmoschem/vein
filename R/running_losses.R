#' Estimation of average daily running losses evaporative emissions
#'
#' @description \code{running_losses} estimates evaporative emissions from
#' EMEP/EEA emisison guidelines
#'
#' @param x Numeric or data.frame; Mean number of trips per vehicle per day or Vehicles.
#' @param erhotfi average daily hot running losses evaporative factor
#' for vehicles with fuel injection and returnless fuel systems
#' @param lkm Numeric with class untis and unit km.
#' @param carb fraction of gasoline vehicles with carburator or fuel return system.
#' @param p Fraction of trips finished with hot engine
#' @param erhotc average daily running losses evaporative factor for vehicles with
#' carburator or fuel return system
#' @param erwarmc average daily cold and warm running losses evaporative factor
#' for vehicles with carburator or fuel return system
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
#'
running_losses <- function(x, erhotfi, lkm, carb, p, erhotc, erwarmc) {
  .Deprecated("emis_evap")
  "Evaporative emissions"
}

