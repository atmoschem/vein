#' Emission Factors from EMFAC emission factors
#'
#' @description \code{\link{ef_emfac}} reads path to ef EMFAC.
#' You must download the
#' emission factors from EMFAC website.
#'
#' @param efpath Character path to EMFAC ef (g/miles)
#' @param dg Numeric density of gasoline, default 750 kg/m3
#' @param dd Numeric density of diesel, default 850 kg/m3
#' @param dhy Numeric density of hybrids, default 750 kg/m3
#' @param dcng Numeric density of CNG, default 0.8 kg/m3
#' @param fill_missing Logical to fill and correct ef = 0
#' @param verbose Logical, to show more information
#' @return data.table with emission estimation in long format
#' @note Fuel consumption must be present
#' @export
#' @examples \dontrun{
#' # do not run
#' }
ef_emfac <- function(efpath,
                     dg = 750,
                     dd = 850,
                     dhy = 750,
                     dcng = 0.8,
                     fill_missing = TRUE,
                     verbose = TRUE){

  # ef
  ModelYear <- NULL

  # ef ####
  # if(verbose) cat("Reading ef\n")
  ef <- data.table::fread(efpath)
  names(ef) <- gsub(" ", "", names(ef))

  process <- c("RUNEX",   # Running Exhaust Emissions
               "STREX",   # Idle Exhaust Emissions
               "IDLEX",   # Start Exhaust Tailpipe Emissions
               "DIURN",   # Diurnal Evaporative HC Emissions
               "RESTLOSS",# Resting Evaporative Losses (permeation through rubber and plastic)
               "HOTSOAK", # Hot Soak Evaporative HC Emissions
               "RUNLOSS", # Running Loss Evaporative HC Emissions
               "PMTW",    # Tire Wear
               "PMBW",    # Brake Wear
               "FuelConsumption")  # FC

  mval <- unlist(lapply(process, grep, names(ef), value = T))

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

  def <- data.table::melt.data.table(data = ef,
                                     id.vars = c("ModelYear",
                                                 "VehicleCategory",
                                                 "Speed",
                                                 "vehicles",
                                                 "fuel"),
                                     measure.vars = mval,
                                     variable.name = "pollutant",
                                     value.name = "gmiles")

  pollutant <- NULL
  def[ , pollutant := ifelse(
    pollutant == "FuelConsumption",
    "FC",
    pollutant
  )]

  ef_nofc <- def[pollutant != "FC"]
  ef_fc <- def[pollutant == "FC"]

  lt <- ef_fc$gmiles*3.79

  m3 <- lt/1000

  d_kg_m3 <- ifelse(
    ef_fc$fuel == "G", dg,
    ifelse(
      ef_fc$fuel == "D", dd,
      ifelse(
        ef_fc$fuel == "CNG", dcng,
        ifelse(
          ef_fc$fuel == "HY", dhy,
          1)))) #kg/m3

  ef_fc$gmiles <- m3*d_kg_m3*1000

  def <- rbind(ef_nofc,
               ef_fc)
  # fix for missiing values for some speeds

  if(fill_missing) {

    gmiles <- vehicles <- NULL
    . <- pollutant <- ModelYear <- NULL

    full <- def[,
                mean(gmiles, na.rm = TRUE),
                by = .(vehicles,
                       pollutant,
                       ModelYear)]

    empty <- def[gmiles == 0]

    if(verbose) {
      cat("Identified ", nrow(empty), " 0 gmiles\n")
    }
    df_em <- merge(empty,
                   full,
                   by = c("vehicles",
                          "pollutant",
                          "ModelYear"),
                   all.x = TRUE,
                   allow.cartesian = TRUE)
    df_em$gmiles <- NULL # 0
    names(df_em)[ncol(df_em)] <- "gmiles"

    gmiles <- NULL
    def <- rbind(def[gmiles > 0],
                 df_em)
    # this is a fix for real missing EF
    # if 0 is for SO2 CNG, will be 0
  }

  data.table::setorderv(def, "ModelYear", -1)

  # vehicles <- pollutant <- i <- j <- k <- l <- NULL
  return(def)
}
