#' Emission Factors from EMFAC emission factors
#'
#' @description \code{\link{ef_emfac}} reads path to ef EMFAC.
#' You must download the
#' emission factors from EMFAC website.
#'
#' @param efpath Character path to EMFAC ef (g/miles)
#' @return data.table with emission estimation in long format
#' @export
#' @examples \dontrun{
#' # do not run
#' }
ef_emfac <- function(efpath){

  # ef
  ModelYear <- NULL

  # ef ####
  # if(verbose) cat("Reading ef\n")
  ef <- data.table::fread(efpath)
  names(ef) <- gsub(" ", "", names(ef))
  if(grep(pattern = "FuelConsumption", names(ef)) > 0) {
    names(ef)[grep("FuelConsumption",
                   names(ef))] <- "FC"
  }

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
  return(def)
}
