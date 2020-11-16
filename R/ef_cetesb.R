#' Emissions factors for Environment Company of Sao Paulo, Brazil (CETESB) 2017
#'
#' \code{\link{ef_cetesb}} returns a vector or data.frame of Brazilian emission factors.
#' @param p Character;
#'
#' Pollutants: "CO", "HC", "NMHC", "CH4", "NOx", "CO2",
#' "RCHO" (aldehydes + formaldehyde), "ETOH",
#' "PM", "N2O", "KML", "FC", "NO2", "NO",
#' "gD/KWH", "gCO2/KWH", "RCHO_0km" (aldehydes + formaldehyde),
#' "CO_0km", "HC_0km", "NMHC_0km", "NOx_0km", "NO2_0km" ,"NO_0km",
#' "RCHO_0km" and "ETOH_0km", "FS" (fuel sales) (g/km). If scale = "tunnel" is
#' used, there is also "ALD" for aldehydes and "HCHO" for  formaldehydes
#' Evaporative emissions at average temperature ranges:
#' "D_20_35", "S_20_35", "R_20_35", "D_10_25", "S_10_25", "R_10_25", "D_0_15",
#' "S_0_15" and "R_0_15" where D means diurnal (g/day), S hot/warm soak (g/trip)
#' and R hot/warm running losses (g/trip).
#' @param veh Character; Vehicle categories:
#' "PC_G", "PC_FG", "PC_FE", "PC_E",
#' "LCV_G", "LCV_FG", "LCV_FE", "LCV_E", "LCV_D",
#' "TRUCKS_SL", "TRUCKS_L", "TRUCKS_M", "TRUCKS_SH", "TRUCKS_H",
#' "BUS_URBAN", "BUS_MICRO", "BUS_COACH", "BUS_ARTIC",
#' "MC_G_150", "MC_G_150_500", "MC_G_500",
#' "MC_FG_150", "MC_FG_150_500", "MC_FG_500",
#' "MC_FE_150", "MC_FE_150_500", "MC_FE_500"
#' "CICLOMOTOR", "GNV"
#' @param year Numeric; Filter the emission factor to start from a specific base year.
#' If project is 'constant' values above 2017 and below 1980 will be repeated
#' @param agemax Integer; age of oldest vehicles for that category
#' @param scale Character; values "default" or "tunnel". If "tunnel", emission
#' factors are scaled to represent EF measurements in tunnels in Sao Paulo
#' @param sppm Numeric, sulfur (sulphur) in ppm in fuel.
#' @param full Logical; To return a data.frame instead or a vector adding
#' Age, Year, Brazilian emissions standards and its euro equivalents.
#' @param project Character showing the method for projecting emission factors in
#' future. Currently the only value is "constant"
#' @param verbose Logical; To show more information
#' @param csv String with the path to download the ef in a .csv file. For instance,
#' ef.csv
#' @return A vector of Emission Factor or a data.frame
#' @keywords  emission factors
#' @note The new convention for vehicles names are translated from CETESB report:
#' \tabular{ll}{
#'   veh    \tab description \cr
#'   PC_G   \tab Passenger Car Gasohol (Gasoline + 27perc of anhydrous ethanol)  \cr
#'   PC_E   \tab Passenger Car Ethanol (hydrous ethanol) \cr
#'   PC_FG   \tab Passenger Car Flex Gasohol (Gasoline + 27perc of anhydrous ethanol)  \cr
#'   PC_FE  \tab Passenger Car Flex Ethanol (hydrous ethanol) \cr
#'   LCV_G   \tab Light Commercial Vehicle Gasohol (Gasoline + 27perc of anhydrous ethanol)  \cr
#'   LCV_E   \tab Light Commercial Vehicle Ethanol (hydrous ethanol) \cr
#'   LCV_FG   \tab Light Commercial Vehicle Flex Gasohol (Gasoline + 27perc of anhydrous ethanol)  \cr
#'   LCV_FE  \tab Light Commercial Vehicle Flex Ethanol (hydrous ethanol) \cr
#'   LCV_D  \tab Light Commercial Vehicle Diesel (5perc bio-diesel) \cr
#'   TRUCKS_SL_D  \tab Trucks Semi Light Diesel (5perc bio-diesel) \cr
#'   TRUCKS_L_D  \tab Trucks Light Diesel (5perc bio-diesel) \cr
#'   TRUCKS_M_D  \tab Trucks Medium Diesel (5perc bio-diesel) \cr
#'   TRUCKS_SH_D  \tab Trucks Semi Heavy Diesel (5perc bio-diesel) \cr
#'   TRUCKS_H_D  \tab Trucks Heavy Diesel (5perc bio-diesel) \cr
#'   BUS_URBAN_D  \tab Urban Bus Diesel (5perc bio-diesel) \cr
#'   BUS_MICRO_D  \tab Micro Urban Bus Diesel (5perc bio-diesel) \cr
#'   BUS_COACH_D  \tab Coach (inter-state) Bus Diesel (5perc bio-diesel) \cr
#'   BUS_ARTIC_D  \tab Articulated Urban Bus Diesel (5perc bio-diesel) \cr
#'   MC_150_G   \tab Motorcycle engine less than 150cc Gasohol (Gasoline + 27perc of anhydrous ethanol)  \cr
#'   MC_150_500_G   \tab Motorcycle engine 150-500cc Gasohol (Gasoline + 27perc of anhydrous ethanol)  \cr
#'   MC_500_G   \tab Motorcycle greater than 500cc Gasohol (Gasoline + 27perc of anhydrous ethanol)  \cr
#'   MC_150_FG   \tab Flex Motorcycle engine less than 150cc Gasohol (Gasoline + 27perc of anhydrous ethanol)  \cr
#'   MC_150_500_FG   \tab Flex Motorcycle engine 150-500cc Gasohol (Gasoline + 27perc of anhydrous ethanol)  \cr
#'   MC_500_FG   \tab Flex Motorcycle greater than 500cc Gasohol (Gasoline + 27perc of anhydrous ethanol)  \cr
#'   MC_150_FE   \tab Flex Motorcycle engine less than 150cc Ethanol (hydrous ethanol) \cr
#'   MC_150_500_FE   \tab Flex Motorcycle engine 150-500cc Ethanol (hydrous ethanol) \cr
#'   MC_500_FE   \tab Flex Motorcycle greater than 500cc Ethanol (hydrous ethanol) \cr
#'   PC_ELEC   \tab Passenger Car Electric  \cr
#'   LCV_ELEC   \tab Light Commercial Vehicle Electric \cr
#' }
#'
#' The percentage varies of biofuels varies by law.
#'
#' This emission factors are not exactly the same as the report of CETESB.
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
#' If by mistake, the user inputs one of veh names from the old convention,
#' they are internally changed to the new convention:
#' "SLT", "LT", "MT", "SHT","HT", "UB", "SUB", "COACH", "ARTIC", "M_G_150",
#' "M_G_150_500", "M_G_500", "M_FG_150", "M_FG_150_500", "M_FG_500",
#' "M_FE_150", "M_FE_150_500","M_FE_500",
#'
#' If pollutant is "SO2", it needs sppm. It is designed when veh has length 1, if it has length 2 or more,
#' it will show a warning
#'
#' \strong{Emission factor for vehicles older than the reported by CETESB were filled as the moving average of 2:}
#'
#' \itemize{
#' \item Range EF from PC and LCV otto: 2018 - 1982. EF for 1981 and older as moving average.
#' \item Range LCV diesel : 2018 - 2006. EF for 2005 and older as moving average.
#' \item Range Trucks and Buse: 2018 - 1998. EF for 1997 and older as moving average.
#' \item Range MC Gasoline: 2018 - 2003.  EF for 2002 and older as moving average.
#' \item Range MC Flex 150-500cc and >500cc: 2018 - 2012.  EF for 2011 and older as moving average.
#'}
#' @references Emissoes Veiculares no Estado de Sao Paulo 2016. Technical Report.
#' url: https://cetesb.sp.gov.br/veicular/relatorios-e-publicacoes/.
#'
#' @note Currently, 2020, there are not any system for recovery of fuel vapors in Brazil. Hence,
#' the FS takes into account the vapour that comes from the fuel tank inside the car and
#' released into the atmosphere when injecting new fuel. There are discussions about
#' incrementing implementing stage I and II and/or ORVR thesedays. The ef FS is calculated
#' by transforming g FC/km into  (L/KM)*g/L with g/L 1.14 fgor gasoline and 0.37
#' for ethanol (CETESB, 2016). The density considered is 0.75425 for gasoline and
#' 0.809 for ethanol (t/m^3)
#'
#' CETESB emission factors did not cover evaporative emissions from motorcycles,
#' which occure. Therefore, in the abscence of better data, it was assumed the
#' same ratio from passenger cars.
#'
#' Li, Lan, et al. "Exhaust and evaporative emissions from motorcycles fueled
#' with ethanol gasoline blends." Science of the Total Environment 502 (2015): 627-631.
#'
#' If scale is used with tunnel, the references are:
#' \itemize{
#' \item Pérez-Martinez, P. J., Miranda, R. M., Nogueira, T., Guardani, M. L.,
#' Fornaro, A., Ynoue, R., and Andrade, M. F. (2014). Emission
#' factors of air pollutants from vehicles measured inside road tunnels in
#' Sao Paulo: case study comparison. International Journal of
#' Environmental Science and Technology, 11(8), 2155-2168.
#' \item Nogueira, T., de Souza, K. F., Fornaro, A., de Fatima Andrade, M., and
#'  de Carvalho, L. R. F. (2015). On-road emissions of carbonyls
#'  from vehicles powered by biofuel blends in traffic tunnels in the
#'  Metropolitan Area of Sao Paulo, Brazil. Atmospheric Environment, 108, 88-97.
#'}
#'
#'
#' @export
#' @examples \dontrun{
#' a <- ef_cetesb("CO", "PC_G")
#' a <- ef_cetesb("R_10_25", "PC_G")
#' a <- ef_cetesb("CO", c("PC_G", "PC_FE"))
#' ef_cetesb(p = "CO", veh = "PC_G", year = 1970, agemax = 40)
#' ef_cetesb(p = "CO", veh = "PC_G", year = 2030, agemax = 40)
#' ef_cetesb(p = "CO", veh = "TRUCKS_L_D", year = 2018)
#' ef_cetesb(p = "CO", veh = "SLT", year = 2018) #  olds names
#' ef_cetesb(p = "SO2", veh = "PC_G", year = 2030, agemax = 40, sppm = 300)
#' ef_cetesb(p = "SO2", veh = "PC_FE", year = 2030, agemax = 40, sppm = 300)
#' a <- ef_cetesb(p = "NMHC", veh = c("PC_G", "PC_FG", "PC_FE", "PC_E"), year = 2018, agemax = 20)
#' colplot(a, main = "NMHC EF", ylab = "[g/km]", xlab = "Years of use")
#' ef_cetesb(p = "CO", veh = "PC_ELEC", year = 1970, agemax = 40)
#' }
ef_cetesb <- function(p,
                      veh,
                      year = 2017,
                      agemax = 40,
                      scale = "default",
                      sppm,
                      full = FALSE,
                      project = "constant",
                      verbose = FALSE,
                      csv){
  ef <- sysdata$cetesb
  print(names(ef))
  ef$PC_ELEC <- ef$PC_G*0
  ef$LCV_ELEC <- ef$LCV_G*0
  ef$BUS_ELEC <- ef$BUS_URBAN_D*0
  ef$MC_ELEC_150 <- ef$MC_150_G*0
  ef$MC_ELEC_150_500 <- ef$MC_150_500_G*0
  ef$MC_ELEC_500 <- ef$MC_500_G*0

  # tunel
  if(scale == "tunnel") {
    ef <- sysdata$cetesb
    names(ef)
    LDV <- c(grep(pattern = "PC_", x = names(ef), value = TRUE),
             grep(pattern = "LCV_", x = names(ef), value = TRUE)[1:4],
             grep(pattern = "MC_", x = names(ef), value = TRUE))

    HDV <- c(grep(pattern = "TRUCKS_", x = names(ef), value = TRUE),
             grep(pattern = "BUS_", x = names(ef), value = TRUE),
             "LCV_D")
    # CO HC NMHC
    COHC <- c("CO", "CO_0km","NMHC", "NMHC_0km","HC", "HC_0km")
    ef[ef$Pollutant %in% COHC, LDV] <- ef[ef$Pollutant %in% COHC, LDV]*1.842674

    ef[ef$Pollutant %in% COHC, HDV] <- ef[ef$Pollutant %in% COHC, HDV]*2.905313

    # NONO2 NOx
    NONO<- c("NOx", "NO2", "NO", "NOx_0km", "NO2_0km", "NO_0km")
    ef[ef$Pollutant %in% NONO, HDV] <- ef[ef$Pollutant %in% NONO, HDV]*1.459931

    # PM
    ef[ef$Pollutant %in% "PM", HDV] <- ef[ef$Pollutant %in% "PM", HDV]*1.487109

    # ALD # ja tem NMHC incrementado
    efALD <- ef[ef$Pollutant %in% "NMHC", ]
    efALD$Pollutant <- "ALD"
    efALD[, LDV] <- efALD[, LDV]*0.02420901
    efALD[, HDV] <- efALD[, HDV]*0.08494866
    ef <- rbind(ef, efALD)

    # HCHO # ja tem NMHC incrementado
    efHCHO <- ef[ef$Pollutant %in% "NMHC", ]
    efHCHO$Pollutant <- "HCHO"
    efHCHO[, LDV] <- efHCHO[, LDV]*0.01864748
    efHCHO[, HDV] <- efHCHO[, HDV]*0.08494866
    ef <- rbind(ef, efHCHO)

  }
  ef[is.na(ef)] <- 0

  oldt <- c("SLT", "LT", "MT", "SHT", "HT",
            "UB", "SUB", "COACH", "ARTIC",
            "M_G_150", "M_G_150_500", "M_G_500",
            "M_FG_150", "M_FG_150_500", "M_FG_500",
            "M_FE_150", "M_FE_150_500", "M_FE_500")
  if(any(veh %in% oldt)) {
    message("I guess you wanted this:")
    veh <- switch(veh,
                  "SLT" = "TRUCKS_SL_D",
                  "LT" = "TRUCKS_L_D",
                  "MT" = "TRUCKS_M_D",
                  "SHT" = "TRUCKS_SH_D",
                  "HT" = "TRUCKS_H_D",
                  "UB" = "BUS_URBAN_D",
                  "SUB" = "BUS_MICRO_D",
                  "COACH" = "BUS_COACH_D",
                  "ARTIC" = "BUS_ARTIC_D",
                  "M_G_150" = "MC_150_G",
                  "M_G_150_500" = "MC_150_500_G",
                  "M_G_500" = "MC_500_G",
                  "M_FG_150" = "MC_150_FG",
                  "M_FG_150_500" = "MC_150_500_FG",
                  "M_FG_500" = "MC_500_FG",
                  "M_FE_150" = "MC_150_FE",
                  "M_FE_150_500" = "MC_150_500_FE",
                  "M_FE_500" = "MC_500_FE")
    message(veh)
  }
  year1 <- ef$Year[1]

  p <- gsub(pattern = "d", replacement = "", x = p) #not break old code

  s0 <- c("PC_E", "PC_FE", "LCV_E", "LCV_FE",
          "MC_150_FE", "MC_150_500_FE", "MC_500_FE")

  if(p == "SO2"){
    if(missing(sppm)) stop("if p is 'SO2', sppm must be present")
    if(length(veh) != length(sppm)) {
      stop("sppm must has the same length as veh")
    }
  }

  if(year < 1956) stop("Choose a newer year")
  # Selecting
  ef <- ef[ef$Year <= year, ]

  evapd <- c("D_20_35","D_10_25","D_0_15")
  evap <- c("S_20_35", "R_20_35", "S_10_25", "R_10_25", "S_0_15", "R_0_15")
  pols <- as.character(unique(ef$Pollutant))

  if(!p %in% c(pols, "SO2")){
    stop(cat("Please, choose one of the following pollutants:\n", pols, "\n"))
  }

  if(p %in% evapd){
    if(verbose) message("Units: [g/day]\n")
  }
  if(p %in% evap){
    if(verbose) message("Units: [g/trip]\n")
  }
  nveh <- names(ef)[12:ncol(ef)]
  if(any(!veh %in% nveh)){
    stop(cat("Please, choose on of the following categories:\n", nveh, "\n"))
  }



  pol <- p
  k <- ifelse(p == "SO2", sppm*2*1e-06, 1)
  p <- ifelse(p == "SO2", "FC", p)

  if(full) {
    if(p %in% c(evapd, evap)){
      df1 <- ef[ef$Pollutant == p, 1:11]
      df2 <- ef[ef$Pollutant == p, veh]
      if(length(veh) == 1) {
        df2 <- units::as_units(df2, "g")
      } else {
        for(i in 1:ncol(df2)) df2[, i] <- units::as_units(df2[, i], "g")
      }

      df <- cbind(df1, df2)

      names(df)[ncol(df)] <- p

    } else {
      if(pol == "SO2" & length(veh) == 1){
        if(veh %in% s0) k = 0
      }
      df <- cbind(ef[ef$Pollutant == p, 1:11],
                  EmissionFactors(ef[ef$Pollutant == p, veh]*k)  )
      names(df)[ncol(df)] <- p
    }
  } else {
    if(p %in% c(evapd, evap)){
      df <- ef[ef$Pollutant == p, veh]

      if(length(veh) == 1) {
        df <- units::as_units(df, "g")
      } else {
        for(i in 1:ncol(df)) df[, i] <- units::as_units(df[, i], "g")
      }

    } else {
      if(pol == "SO2" & length(veh) == 1){
        if(veh %in% s0) k = 0
      }
      df <- vein::EmissionFactors(ef[ef$Pollutant == p, veh]*k)
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
  if(!missing(csv)) {
    data.table::fwrite(x = df, file = csv)
  }
  return(df)
}
