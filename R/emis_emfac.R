#' Emission calculation based on EMFAC emission factors
#'
#' @description \code{\link{emis_emfac}} estimates emissions based on
#' an emission factors database from EMFAC.You must download the
#' emission factors from EMFAC website.
#'
#' @param efpath Character path to EMFAC ef (g/miles)
#' @param veh Vehicles data.frame
#' @param lkm Distance per street-link in miles
#' @param speed Speed data.frame in niles/hour
#' @param vehname numeric vector for heavy good vehicles or trucks
#' @param pol character, "CO_RUNEX"
#' @param modelyear numeric vector, 2021:1982
#' @param noyear newest numeric year to take out from ef
#' @param hours Character, name of hours in speed, paste0("S", 1:24) data-frame profile for passenger cars, 24 hours only.
#' @param verbose, logical, to show more information
#' @return data.table with emission estimation in long format
#' @export
#' @examples \dontrun{
#' # do not run
#' }
emis_emfac <- function(efpath,
                       veh,
                       lkm,
                       speed,
                       vehname,
                       pol = "CO_RUNEX",
                       modelyear = 2021:1982,
                       noyear = 2022,
                       hours = paste0("S", 1:24),
                       verbose = TRUE){

  # ef

  # ef ####
  # if(verbose) cat("Reading ef\n")
  ef <- data.table::fread(efpath)
  names(ef) <- gsub(" ", "", names(ef))
  if(grep(pattern = "FuelConsumption", names(ef)) > 0) {
    names(ef)[grep("FuelConsumption",
                   names(ef))] <- "FC"
  }
  ModelYear <- NULL
  ef <- ef[ModelYear < noyear]

  ef$fuel <- ifelse(
    ef$Fuel == "Gasoline", "G",
    ifelse(
      ef$Fuel == "Diesel", "D",
      ifelse(
        ef$Fuel == "Natural Gas", "CNG",
        ifelse(
          ef$Fuel == "Electricity", "ELEC",
          ifelse(
            ef$Fuel == "Plug-in Hybrid", "HY",
            "caca"
          )))))

  unique(ef$fuel)

  ef$vehicles <- paste0(ef$VehicleCategory,  "_", ef$fuel)


  runex <- grep(pattern = "RUNEX",
                x = names(ef),
                value = T)

  pmbw <- grep(pattern = "PMBW",
               x = names(ef),
               value = T)

  eff <- ef[ModelYear < 2022,
            c("ModelYear",
              "VehicleCategory",
              "Speed",
              "vehicles",
              "fuel",
              runex,
              pmbw,
              "FC"),
            with = F]

  def <- data.table::melt.data.table(data = ef,
                                     id.vars = c("ModelYear",
                                                 "VehicleCategory",
                                                 "Speed",
                                                 "vehicles",
                                                 "fuel"),
                                     measure.vars = c(runex,
                                                      pmbw,
                                                      "FC"),
                                     variable.name = "pollutant",
                                     value.name = "gmiles")

  data.table::setorderv(def, "ModelYear", -1)
  vehicles <- pollutant <- i <- j <- k <- l <- NULL
  efx <- def[vehicles == vehname &
               pollutant == pol]


  # estimation
  if(verbose) cat("Estimating emissions of", pol, " for", vehname, "\n")
  data.table::rbindlist(lapply(seq_along(hours), function(l) {

    data.table::rbindlist(lapply(seq_along(modelyear), function(k) {

      efx <- efx[ModelYear == modelyear[k]]

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

      data.table::data.table(id = 1:length(eeff),
                             emi = eeff*lkm* vv,
                             age = k,
                             vehicles = vehname,
                             pollutant = pol,
                             hour = l)
    }))
  }))   -> emii
  return(emii)
}
