#' Emissions factors for Environment Company of Sao Paulo, Brazil (CETESB) 2017
#'
#' \code{\link{ef_cetesb}} returns a vector or data.frame of Brazilian emission factors.
#' @param p Character;
#'
#' Pollutants: "CO", "HC", "NMHC", "CH4", "NOx", "CO2","RCHO", "ETOH",
#' "PM", "N2O", "KML", "FC", "NO2d", "NOd", "gD/KWH", "gCO2/KWH", "RCHOd",
#' "CO_0km", "HC_0km", "NMHC_0km", "NOx_0km", "NO2_0km" ,"NO_0km",
#' "RCHO_0km" and "ETOH_0km",
#' (g/km).  Evaporative emissions at average temperature ranges:
#' "D_20_35", "S_20_35", "R_20_35", "D_10_25", "S_10_25", "R_10_25", "D_0_15",
#' "S_0_15" and "R_0_15" where D means diurnal (g/day), S hot/warm soak (g/trip)
#' and R hot/warm running losses (g/trip).
#' @param veh Character; Vehicle categories: "PC_G", "PC_FG", "PC_FE", "PC_E",
#' "LCV_G", "LCV_FG", "LCV_FE", "LCV_E", "LCV_D", "SLT", "LT", "MT", "SHT",
#' "HT", "UB", "SUB", "COACH", "ARTIC", "M_G_150", "M_G_150_500", "M_G_500",
#' "M_FG_150", "M_FG_150_500", "M_FG_500", "M_FE_150", "M_FE_150_500",
#' "M_FE_500", "CICLOMOTOR", "GNV"
#' @param year Numeric; Filter the emission factor to start from a specific base year.
#' If project is 'constant' values above 2017 and below 1980 will be repeated
#' @param agemax Integer; age of oldest vehicles for that category
#' @param full Logical; To return a data.frame instead or a vector adding
#' Age, Year, Brazilian emissions standards and its euro equivalents.
#' @param project haracter showing the method for projecting emission factors in
#' future. Currently the only value is "constant"
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
#' In the previous versions I used the letter 'd' for deteriorated. I removed the
#' letter 'd' internally to not break older code.
#'
#' @references Emissoes Veiculares no Estado de Sao Paulo 2016. Technical Report.
#' url: https://cetesb.sp.gov.br/veicular/relatorios-e-publicacoes/.
#' @export
#' @examples {
#' a <- ef_cetesb("CO", "PC_G")
#' a <- ef_cetesb("R_10_25", "PC_G")
#' a <- ef_cetesb("CO", c("PC_G", "PC_FE"))
#' ef_cetesb(p = "CO", veh = "PC_G", year = 2018, agemax = 40)
#' ef_cetesb(p = "CO", veh = "PC_G", year = 1970, agemax = 40)
#' ef_cetesb(p = "CO", veh = "PC_G", year = 2030, agemax = 40)
#' }
ef_cetesb <- function(p, veh, year = 2017, agemax = 40, full = FALSE, project = "constant"){
  ef <- sysdata$cetesb
  ef[is.na(ef)] <- 0

  year1 <- ef$Year[1]

  p <- gsub(pattern = "d", replacement = "", x = p) #not break old code

  if(year < 1956) stop("Choose a newer year")
    # Selecting
    ef <- ef[ef$Year <= year, ]

  evapd <- c("D_20_35","D_10_25","D_0_15")
  evap <- c("S_20_35", "R_20_35", "S_10_25", "R_10_25", "S_0_15", "R_0_15")
  pols <- as.character(unique(ef$Pollutant))
  if(!p %in% pols){
    stop(paste("Please, choose one of the following pollutants:", pols))
  }
  if(p %in% evapd){
    message("Units: [g/day]\n")
  }
  if(p %in% evap){
    message("Units: [g/trip]\n")
  }
  nveh <- names(ef)[12:ncol(ef)]
  if(any(!veh %in% nveh)){
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
  if(is.data.frame(df)){
    # project future EF
    if(project == "constant"){
      if(year > year1){
        dif <- year - year1

        eff <- do.call("rbind",(lapply(1:dif, function(i){
          df[1, ]
        })))
        edff <- rbind(eff, df[1:(agemax - dif), ])
      }
    }

    #Filling older ef
    if(!missing(agemax)){
      if(nrow(df) < agemax){
        dif <- agemax - nrow(df)
        df[nrow(df):(nrow(df)+dif), ] <- df[nrow(df), ]
      }
      df <-  df[1:agemax, ]
    }

  } else {
    # project future EF
    if(project == "constant"){
      if(year > year1){
        dif <- year - year1
        eff <- rep(df[1], dif)
        df <- c(eff, df[1:(agemax - dif)])
      }
    }

    #Filling older ef
    if(!missing(agemax)){
      if(length(df) < agemax){
        dif <- agemax - length(df)
        df[length(df):(length(df)+dif)] <- df[length(df)]
      }
      df <-  df[1:agemax]
    }

  }
  return(df)
}
