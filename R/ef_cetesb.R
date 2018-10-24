#' Emissions factors for Environment Company of Sao Paulo, Brazil (CETESB) 2016
#'
#' \code{\link{ef_cetesb}} returns a vector or data.frame of Brazilian emission factors.
#' @param p Character; Pollutants: "COd", "HCd", "NMHCd", "CH4", "NOxd", "CO2"
#' "PM", "N2O", "KML", "FC", "NO2d", "NOd", "gD/KWH", "gCO2/KWH", "RCHOd",
#' "CO", "HC", "NMHC", "NOx", "NO2" ,"NO", "RCHO" (g/km). The letter 'd' means deteriorated
#' factor. Also, evaporative emissions at average temperature ranges:
#' "D_20_35", "S_20_35", "R_20_35", "D_10_25", "S_10_25", "R_10_25", "D_0_15"
#' "S_0_15" and "R_0_15" where D means diurnal (g/day), S hot/warm soak (g/trip)
#' and R hot/warm running losses (g/trip).
#' @param veh Character; Vehicle categories: "PC_G", "PC_FG", "PC_FE", "PC_E"
#' "LCV_G", "LCV_FG", "LCV_FE", "LCV_E", "LCV_D", "SLT", "LT", "MT", "SHT"
#' "HT", "UB", "SUB", "COACH", "ARTIC", "M_G_150", "M_G_150_500", "M_G_500"
#' "M_FG_150", "M_FG_150_500", "M_FG_500". "M_FE_150", "M_FE_150_500",
#' "M_FE_500", "CICLOMOTOR", "GNV
#' @param full Logical; To return a data.frame instead or a vector adding
#' Age, Year, Brazilian emissions standards and its euro equivalents.
#' @return A vector of Emission Factor or a data.frame
#' @keywords  emission factors
#' @note This emission factors are not exactly the same as the report of CETESB.
#'
#' 1) In this emission factors, there is also NO and NO2 based on split by
#' published in the EMEP/EEA air pollutant emission inventory guidebook.
#'
#' 2) Also, the emission factors were extended till 50 years of use, repeating
#' the oldest value.
#'
#' 3) CNG emission factors were expanded to other pollutants by comparison
#' of US.EPA-AP42 emission factor: Section 1.4 Natural Gas Combustion.
#'
#' @references Emissoes Veiculares no Estado de Sao Paulo 2016. Technical Report.
#' url: https://cetesb.sp.gov.br/veicular/relatorios-e-publicacoes/.
#' @export
#' @examples {
#' a <- ef_cetesb("CO", "PC_G")
#' b <- ef_cetesb("R_10_25", "PC_G")
#' }
ef_cetesb <- function(p, veh, full = FALSE){
  ef <- sysdata[["cetesb"]]
  ef <-  ef[ef$Age <= 50, ]
  evapd <- c("D_20_35","D_10_25","D_0_15")
  evap <- c("S_20_35", "R_20_35", "S_10_25", "R_10_25", "S_0_15", "R_0_15")
  pols <- as.character(unique(ef$Pollutant))
  if(!p %in% pols){
    stop(paste("Please, choose one of the following pollutants:", pols))
  }
  if(p %in% evapd){
    cat("Units: [g/day]\n")
  }
  if(p %in% evap){
    cat("Units: [g/trip]\n")
  }
  nveh <- names(ef)[12:ncol(ef)]
  if(!veh %in% nveh){
    stop(paste("Please, choose on of the following categories:", nveh))
  }
  if(full) {
    if(p %in% c(evapd, evap)){
      df <- cbind(ef[ef$Pollutant == p, 1:11],
                  ef[ef$Pollutant == p, veh])
      names(df)[ncol(df)] <- p

    } else {
      df <- cbind(ef[ef$Pollutant == p, 1:11],
                  EmissionFactors(ef[ef$Pollutant == p, veh]))
      names(df)[ncol(df)] <- p

    }
  } else{
    if(p %in% c(evapd, evap)){
      df <- ef[ef$Pollutant == p, veh]
    } else {
      df <- vein::EmissionFactors(ef[ef$Pollutant == p, veh])
    }

  }
  return(df)
}
