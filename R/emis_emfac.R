#' Emission calculation based on EMFAC emission factors
#'
#' @description \code{\link{emis_emfac}} estimates emissions based on
#' an emission factors database from EMFAC.You must download the
#' emission factors from EMFAC website.
#'
#' @param ef Character path to EMFAC ef (g/miles)
#' @param veh Vehicles data.frame
#' @param lkm Distance per street-link in miles
#' @param speed Speed data.frame in niles/hour
#' @param vehname numeric vector for heavy good vehicles or trucks
#' @param pol character, "CO_RUNEX"
#' @param modelyear numeric vector, 2021:1982
#' @param noyear newest numeric year to take out from ef
#' @param hours Character, name of hours in speed, paste0("S", 1:24) data-frame profile for passenger cars, 24 hours only.
#' @param vkm logical, to return vkm
#' @param verbose logical, to show more information
#' @return data.table with emission estimation in long format
#' @export
#' @examples \dontrun{
#' # do not run
#' }
emis_emfac <- function(ef,
                       veh,
                       lkm,
                       speed,
                       vehname,
                       pol = "CO_RUNEX",
                       modelyear = 2021:1982,
                       noyear = 2022,
                       hours = paste0("S", 1:24),
                       vkm = TRUE,
                       verbose = TRUE){

  # ef
if(is.character(ef)) {
  ef <- ef_emfac(ef)
}
  ModelYear <- NULL

  # estimation
  if(verbose) cat("Estimating emissions of", pol, " for", vehname, "\n")
  data.table::rbindlist(lapply(seq_along(hours), function(l) {

    data.table::rbindlist(lapply(seq_along(modelyear), function(k) {

      efx <- ef[ModelYear == modelyear[k]]

      breaks = unique(efx$Speed)

      interval <- findInterval(as.numeric(speed[[hours[l]]]),
                               breaks)

      dfspeed <- data.table::data.table(Speed = breaks[interval])

      # match Speed with real speed
      dfspeed <- merge(dfspeed,
                       efx[, c("Speed", "gmiles"),
                           with = F],
                       by = "Speed",
                       all.x = T)

      eeff <- EmissionFactors(dfspeed$gmiles,
                              mass = "g",
                              dist = "miles")

      vv <- Vehicles(as.numeric(veh[[k]]),
                     time = "1/h")

      dx <- data.table::data.table(id = 1:length(eeff),
                                   emi = eeff*lkm* vv,
                                   age = k,
                                   vehicles = vehname,
                                   pollutant = pol,
                                   hour = l)
      if(vkm) {
        vkm <- data.table::data.table(id = 1:length(eeff),
                                      emi = lkm* vv,
                                      age = k,
                                      vehicles = vehname,
                                      pollutant = "vkm",
                                      hour = l)
        dx <- rbind(dx, vkm)
      }
      dx
    }))
  }))   -> emii
  return(emii)
}
