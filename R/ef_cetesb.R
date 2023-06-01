#' Emissions factors for Environment Company of Sao Paulo, Brazil (CETESB)
#'
#' \code{\link{ef_cetesb}} returns a vector or data.frame of Brazilian emission factors.
#' @param p Character;
#'
#' Pollutants: "CO", "HC", "NMHC", "CH4", "NOx", "CO2",
#' "RCHO" (aldehydes + formaldehyde), "ETOH",
#' "PM", "N2O", "KML", "FC", "NO2", "NO", "NH3",
#' "gD/KWH", "gCO2/KWH", "RCHO_0km" (aldehydes + formaldehyde), "PM25RES", "PM10RES",
#' "CO_0km", "HC_0km", "NMHC_0km", "NOx_0km", "NO2_0km" ,"NO_0km",
#' "RCHO_0km" and "ETOH_0km", "FS" (fuel sales) (g/km). If scale = "tunnel" is
#' used, there is also "ALD" for aldehydes and "HCHO" for  formaldehydes
#' Evaporative emissions at average temperature ranges:
#' "D_20_35", "S_20_35", "R_20_35", "D_10_25", "S_10_25", "R_10_25", "D_0_15",
#' "S_0_15" and "R_0_15" where D means diurnal (g/day), S hot/warm soak (g/trip)
#' and R hot/warm running losses (g/trip). THe deteriorated emission factors are calculated inside this function.
#' @param veh Character; Vehicle categories:
#' "PC_G", "PC_FG", "PC_FE", "PC_E",
#' "LCV_G", "LCV_FG", "LCV_FE", "LCV_E", "LCV_D",
#' "TRUCKS_SL", "TRUCKS_L", "TRUCKS_M", "TRUCKS_SH", "TRUCKS_H",
#' "BUS_URBAN", "BUS_MICRO", "BUS_COACH", "BUS_ARTIC",
#' "MC_150_G", "MC_150_500_G", "MC_500_G",
#' "MC_150_FG", "MC_150_500_FG", "MC_500_FG",
#' "MC_150_FE", "MC_150_500_FE", "MC_500_FE",
#' "CICLOMOTOR", "GNV"
#' @param year Numeric; Filter the emission factor to start from a specific base year.
#' If project is 'constant' values above 2017 and below 1980 will be repeated
#' @param agemax Integer; age of oldest vehicles for that category
#' @param scale Character; values "default","tunnel" o  "tunnel2018". If "tunnel", emission
#' factors are scaled to represent EF measurements in tunnels in Sao Paulo
#' @param sppm Numeric, sulfur (sulphur) in ppm in fuel.
#' @param full Logical; To return a data.frame instead or a vector adding
#' Age, Year, Brazilian emissions standards and its euro equivalents.
#' @param efinput data.frame with efinput structure of sysdata cetesb. Allow
#'  apply deterioration for future emission factors
#' @param verbose Logical; To show more information
#' @param csv String with the path to download the ef in a .csv file. For instance,
#' ef.csv
#' @return A vector of Emission Factor or a data.frame
#' @note new emission factors ar projects as the lates available,
#' @importFrom data.table melt rbindlist ":="
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
#' PC_ELEC, LCV_ELEC, TRUCKS_ELEC, BUS_ELEC,
#' MC_150_ELEC, MC_150_500_ELEC, MC_500_ELEC
#'
#' If pollutant is "SO2", it needs sppm. It is designed when veh has length 1, if it has length 2 or more,
#' it will show a warning
#'
#' \strong{Emission factor for vehicles older than the reported by CETESB were filled with las highest EF}
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
#' increasing implementing stage I and II and/or ORVR these days. The ef FS is calculated
#' by transforming g FC/km into  (L/KM)*g/L with g/L 1.14 fgor gasoline and 0.37
#' for ethanol (CETESB, 2016). The density considered is 0.75425 for gasoline and
#' 0.809 for ethanol (t/m^3)
#'
#' CETESB emission factors did not cover evaporative emissions from motorcycles,
#' which occur. Therefore, in the absence of better data, it was assumed the
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
#' \item Nogueira, T., et al (2021). In preparation (for tunnel 2018)
#'}
#'
#' Emission factors for resuspension applies \strong{only} with top-down approach
#' as a experimental feature. Units are g/(streets*veh)/day. These values were
#' derived form a bottom-up resuspension emissions from metropolitan area
#' of Sao Paulo 2018, assuming 50000 streets
#'
#' NH3 from EEA Tier 2
#'
#' @export
#' @examples {
#' a <- ef_cetesb(p = "CO", veh = "PC_G")
#' a <- ef_cetesb(p = "NOx", veh = "TRUCKS_M_D")
#' a <- ef_cetesb("R_10_25", "PC_G")
#' a <- ef_cetesb("CO", c("PC_G", "PC_FE"))
#' ef_cetesb(p = "CO", veh = "PC_G", year = 1970, agemax = 40)
#' ef_cetesb(p = "CO", veh = "TRUCKS_L_D", year = 2018)
#' ef_cetesb(p = "CO", veh = "SLT", year = 2018) #  olds names
#' a <- ef_cetesb(p = "NMHC", veh = c("PC_G", "PC_FG", "PC_FE", "PC_E"), year = 2018, agemax = 20)
#' colplot(a, main = "NMHC EF", ylab = "[g/km]", xlab = "Years of use")
#' ef_cetesb(p = "PM25RES", veh = "PC_ELEC", year = 1970, agemax = 40)
#' ef_cetesb(p = "PM25RES", veh = "BUS_ELEC", year = 1970, agemax = 40)
#' }
ef_cetesb <- function(p,
                      veh,
                      year = 2017,
                      agemax = 40,
                      scale = "default",
                      sppm,
                      full = FALSE,
                      efinput,
                      verbose = FALSE,
                      csv){
  # load("R/sysdata.rda")
  if(!missing(efinput)) {
    # Deterioration must be applied to any base year
    #  therefore, project the emission factors constant into the future was wrong
    # instead, efinput allow input any ef with the same structure of sysdata$cetesb
    # and apply deterioration for future emission factors
    ef <- efinput
  } else {
    ef <- sysdata$cetesb
  }

  ymax <- max(ef$Year)
  if(year > ymax) {
    if(verbose) message("Projecting new EF constant ONLY")
    efmax <- ef[ef$Year == max(ef$Year), ]

    dify <- year - ymax

    efmm <- data.table::rbindlist(replicate(
      dify, efmax, simplify = FALSE))

    Year <- Pollutant <- NULL
    efmm[,
         Year := year:(ymax + 1),
         by = Pollutant]

    ef <- rbind(ef, efmm)
    data.table::setorderv(ef,
                          cols = c("Pollutant", "Year"),
                          order = c(1, -1))
  }


  if(year < 1949) stop("Choose a newer year at least in 1949")

  if(length(p) > 1) stop("One pollutant each time please")


  #extend ef until 120 years in past ####
  # rep nrow ef
  lef <- split(ef, ef$Pollutant)
  nx <- 120 - nrow(lef[[1]])
  ef <- data.table::rbindlist(
    lapply(
      1:length(lef),
      function(i){
        rbind(
          lef[[i]],
          lef[[i]][rep(nrow(lef[[i]]), nx),  ]
        )}))

  Age <- Year <- .N <- Pollutant <- NULL
  ef[, Age := 1:.N, by= Pollutant]
  ef[, Year := max(ef$Year):(max(ef$Year)-max(ef$Age) + 1), by= Pollutant]

  # deterioration ####
  ef <- ef[ef$Year <= year, ]


  det <- sysdata$cetesbdet
  # 1 year - det ####
  endyear <- (year - max(det$Age)+1)
  det$Year <- year:endyear
  det$mil <- ifelse(
    det$veh %in% c("PC_FG", "PC_FE", "LCV_FG", "LCV_FE") &
      det$Year <2003, 0,
    ifelse(
      det$veh %in% c("PC_E", "LCV_E") &
        (det$Year > 2006 | det$Year < 1979), 0,
      det$mil))
  # 2 year - fe 0km ####
  ef <- ef[ef$Year %in% year:endyear, ]
  ef[is.na(ef)] <- 0
  vehs <-  c("PC_G","PC_E", "PC_FG","PC_FE",
             "LCV_G","LCV_E", "LCV_FG","LCV_FE")

  d0 <- data.table::melt(data = ef[ef$Pollutant %in% c("CO", "NMHC", "NOx", "RCHO",
                                                       "NO", "NO2", "HC"),
                                   c("Year", "Pollutant",
                                     "PC_G","PC_E", "PC_FG","PC_FE",
                                     "LCV_G","LCV_E", "LCV_FG","LCV_FE")],
                         id.vars = c("Year", "Pollutant"),
                         measure.vars = vehs,
                         variable.name = "veh",
                         value.name = "ef")
  # 3 adding parameter for mileage ####
  d0$xmil <- 0

  det$xmil <-
    ifelse(
      det$pol == "CO" & det$veh %in% c("PC_G", "PC_FG", "LCV_G", "LCV_FG") & det$Year > 1994, 0.0000032,
      ifelse(
        det$pol == "CO" & det$veh %in% c("PC_G", "PC_FG", "LCV_G", "LCV_FG") & det$Year <= 1994 & det$Year > 1985, 0.0000275,
        ifelse(
          det$pol == "CO" & det$veh %in% c("PC_G", "PC_FG", "LCV_G", "LCV_FG") & det$Year <= 1985, 0.0000413,
          ifelse(
            det$pol == "CO" & det$veh %in% c("PC_E", "PC_FE", "LCV_E", "LCV_FE") & det$Year > 1994, 0.0000028,
            ifelse(
              det$pol == "CO" & det$veh %in% c("PC_E", "PC_FE", "LCV_E", "LCV_FE") & det$Year <= 1994 & det$Year > 1985, 0.000016,
              ifelse(
                det$pol == "CO" & det$veh %in% c("PC_E", "PC_FE", "LCV_E", "LCV_FE") & det$Year <= 1985, 0.0000225,0))))))

  det$xmil <-
    ifelse(
      det$pol %in% c("NMHC", "HC") & det$veh %in% c("PC_G", "PC_FG", "LCV_G", "LCV_FG") & det$Year > 1994, 0.000000288,
      ifelse(
        det$pol %in% c("NMHC", "HC") & det$veh %in% c("PC_G", "PC_FG", "LCV_G", "LCV_FG") & det$Year <= 1994 & det$Year > 1985, 0.0000017,
        ifelse(
          det$pol %in% c("NMHC", "HC") & det$veh %in% c("PC_G", "PC_FG", "LCV_G", "LCV_FG") & det$Year <= 1985, 0.00000319,
          ifelse(
            det$pol %in% c("NMHC", "HC") & det$veh %in% c("PC_E", "PC_FE", "LCV_E", "LCV_FE") & det$Year > 1994, 0.0000003,
            ifelse(
              det$pol %in% c("NMHC", "HC") & det$veh %in% c("PC_E", "PC_FE", "LCV_E", "LCV_FE") & det$Year <= 1994 & det$Year > 1985, 0.0000017,
              ifelse(
                det$pol %in% c("NMHC", "HC") & det$veh %in% c("PC_E", "PC_FE", "LCV_E", "LCV_FE") & det$Year <= 1985, 0.0000017, det$xmil))))))

  det$xmil <-
    ifelse(
      det$pol %in% c("RCHO") & det$veh %in% c("PC_G", "PC_FG", "LCV_G", "LCV_FG") & det$Year > 1994, 0.000000008,
      ifelse(
        det$pol %in% c("RCHO") & det$veh %in% c("PC_G", "PC_FG", "LCV_G", "LCV_FG") & det$Year <= 1994 & det$Year > 1985, 0.00000005,
        ifelse(
          det$pol %in% c("RCHO") & det$veh %in% c("PC_G", "PC_FG", "LCV_G", "LCV_FG") & det$Year <= 1985, 0.00000003,
          ifelse(
            det$pol %in% c("RCHO") & det$veh %in% c("PC_E", "PC_FE", "LCV_E", "LCV_FE") & det$Year > 1994, 0.00000003,
            ifelse(
              det$pol %in% c("RCHO") & det$veh %in% c("PC_E", "PC_FE", "LCV_E", "LCV_FE") & det$Year <= 1994 & det$Year > 1985, 0.0000001,
              ifelse(
                det$pol %in% c("RCHO") & det$veh %in% c("PC_E", "PC_FE", "LCV_E", "LCV_FE") & det$Year <= 1985, 0.0000001, det$xmil))))))

  det$xmil <-
    ifelse(
      det$pol %in% c("NOx", "NO", "NO2") & det$veh %in% c("PC_G", "PC_FG", "LCV_G", "LCV_FG") & det$Year > 1994, 0.000000375,
      ifelse(
        det$pol %in% c("NOx", "NO", "NO2") & det$veh %in% c("PC_G", "PC_FG", "LCV_G", "LCV_FG") & det$Year <= 1994, 1,
        ifelse(
          det$pol %in% c("NOx", "NO", "NO2") & det$veh %in% c("PC_E", "PC_FE", "LCV_E", "LCV_FE") & det$Year > 1994, 0.00000025,
          ifelse(
            det$pol %in% c("NOx", "NO", "NO2") & det$veh %in% c("PC_E", "PC_FE", "LCV_E", "LCV_FE") & det$Year <= 1994, 1, det$xmil))))
  # 5 merging ####
  d0$ID <- paste(d0$Year, d0$Pollutant,  d0$veh)
  det$ID <- paste(det$Year, det$pol, det$veh)
  det2 <- merge(det, d0[, c("ID", "ef")], by ="ID", all.x = T)
  # 6 fe mileages ####
  det2$ef80 <- ifelse(det2$Year < 1994,0, det2$x + det2$ef)
  det2$ef160 <- ifelse(det2$Year < 1994, det2$ef + det2$ef*0.2, det2$x + det2$ef80)
  # 7 constrain fe160 ####
  xx <- det2[det2$Year == 1984 &
               det2$pol == "CO" &
               det2$veh == "PC_G", ]$ef160
  det2$ef160 <- ifelse(
    det2$veh == "PC_G" &
      det2$pol == "CO" &
      det2$Year < 1984,
    xx,
    det2$ef160)
  det2[is.na(det2)]<- 0
  # 8 ef det ####
  det2$efd <- ifelse(
    det2$pol %in% c("CO", "NMHC", "RCHO", "HC") & det2$Year > 1994,
    det2$xmil*det2$mil + det2$ef,
    ifelse(
      det2$pol %in% c("CO", "NMHC", "RCHO", "HC") & det2$Year <= 1994 & det2$mil > 160000,
      det2$ef160,
      ifelse(
        det2$pol %in% c("CO", "NMHC", "RCHO", "HC") & det2$Year <= 1994 & det2$mil <= 160000,
        det2$xmil*det2$mil + det2$ef,
        ifelse(
          det2$pol %in% c("NO", "NO2", "NOx") & det2$Year > 1994,
          det2$xmil*det2$mil + det2$ef,
          det2$ef))))

  data.table::setorderv(x = det2, cols = "Year", order = -1)

  # 9 efd ####
  efd <- ef[!ef$Pollutant %in% c("NO", "NOx", "NO2", "HC", "NMHC", "RCHO", "CO"), ]
  ve4s <- c("PC_G","PC_E", "PC_FG","PC_FE",
            "LCV_G","LCV_E", "LCV_FG","LCV_FE")

  efnoxd <- efnox <- ef[ef$Pollutant == "NOx", ]
  efnox$Pollutant <-  "NOx_0km"
  for(i in seq_along(ve4s)) efnoxd[[ve4s[i]]] <- det2[det2$veh == ve4s[i] & det2$pol == "NOx", ]$efd

  efnod <- efno <- ef[ef$Pollutant == "NO", ]
  efno$Pollutant <-  "NO_0km"
  for(i in seq_along(ve4s)) efnox[[ve4s[i]]] <- det2[det2$veh == ve4s[i] & det2$pol == "NO", ]$efd

  efno2d <- efno2 <- ef[ef$Pollutant == "NO2", ]
  efno2$Pollutant <-  "NO2_0km"
  for(i in seq_along(ve4s)) efno2d[[ve4s[i]]] <- det2[det2$veh == ve4s[i] & det2$pol == "NO2", ]$efd

  efcod <- efco <- ef[ef$Pollutant == "CO", ]
  efcod$Pollutant <-  "CO_0km"
  for(i in seq_along(ve4s)) efcod[[ve4s[i]]] <- det2[det2$veh == ve4s[i] & det2$pol == "CO", ]$efd

  efnmhcd <- efnmhc <- ef[ef$Pollutant == "NMHC", ]
  efnmhcd$Pollutant <-  "NMHC_0km"
  for(i in seq_along(ve4s)) efnmhcd[[ve4s[i]]] <- det2[det2$veh == ve4s[i] & det2$pol == "NMHC", ]$efd

  efhcd <- efhc <- ef[ef$Pollutant == "HC", ]
  efhcd$Pollutant <-  "HC_0km"
  for(i in seq_along(ve4s)) efhcd[[ve4s[i]]] <- det2[det2$veh == ve4s[i] & det2$pol == "HC", ]$efd

  efrchod <- efrcho <- ef[ef$Pollutant == "RCHO", ]
  efrcho$Pollutant <-  "RCHO_0km"
  for(i in seq_along(ve4s)) efrchod[[ve4s[i]]] <- det2[det2$veh == ve4s[i] & det2$pol == "RCHO", ]$efd


  ef <- rbind(efd,
              efcod, efco,
              efnoxd, efnox,
              efnod, efno,
              efno2d, efno2,
              efhcd, efhc,
              efnmhcd, efnmhc,
              efrchod, efrcho)
  ef <- as.data.frame(ef)

  nLDV <- c(grep(pattern = "PC_", x = names(ef), value = TRUE),
            grep(pattern = "LCV_", x = names(ef), value = TRUE),
            "G_BEFORE_GNV", "G_AFTER_GNV", "GNV_AFTER_GNV")
  nHDV <- c(grep(pattern = "TRUCKS_", x = names(ef), value = TRUE))
  nBUS <- c(grep(pattern = "BUS_", x = names(ef), value = TRUE))
  nMC <- c(grep(pattern = "MC_", x = names(ef), value = TRUE),
           "CICLOMOTOR")

  pmveh <-  c("BUS", "LDV", "MC", "TRUCKS", "BUS", "LDV", "MC", "TRUCKS")
  pmpol <- c("PM", "PM", "PM", "PM", "PM10", "PM10", "PM10", "PM10")
  pmg <- c(0.6148492,  0.2065113, 0.1840867, 2.2728183,
           0.5413765, 0.535802, 0.7608918, 9.3943155)/2
  pmdf <- data.frame(veh = pmveh, pol = pmpol, gst = pmg)
  pmef10 <-  pmef2 <-  ef[ef$Pollutant == "CO", ]
  pmef2$Pollutant <- "PM25RES"
  pmef2[, nLDV] <- ifelse(pmef2[, nLDV]>0,
                          1*pmdf[pmdf$pol == "PM" & pmdf$veh == "LDV", ]$gst,
                          NA)
  pmef2[, nHDV] <- ifelse(pmef2[, nHDV]>0,
                          1*pmdf[pmdf$pol == "PM" & pmdf$veh == "HDV", ]$gst,
                          NA)
  pmef2[, nBUS] <- ifelse(pmef2[, nBUS]>0,
                          1*pmdf[pmdf$pol == "PM" & pmdf$veh == "BUS", ]$gst,
                          NA)
  pmef2[, nMC] <- ifelse(pmef2[, nMC]>0,
                         1*pmdf[pmdf$pol == "PM" & pmdf$veh == "MC", ]$gst,
                         NA)

  pmef10$Pollutant <- "PM10RES"
  pmef10[, nLDV] <- ifelse(pmef10[, nLDV]>0,
                           1*pmdf[pmdf$pol == "PM10" & pmdf$veh == "LDV", ]$gst,
                           NA)
  pmef10[, nHDV] <- ifelse(pmef10[, nHDV]>0,
                           1*pmdf[pmdf$pol == "PM10" & pmdf$veh == "HDV", ]$gst,
                           NA)
  pmef10[, nBUS] <- ifelse(pmef10[, nBUS]>0,
                           1*pmdf[pmdf$pol == "PM10" & pmdf$veh == "BUS", ]$gst,
                           NA)
  pmef10[, nMC] <- ifelse(pmef10[, nMC]>0,
                          1*pmdf[pmdf$pol == "PM10" & pmdf$veh == "MC", ]$gst,
                          NA)
  pmef2$PC_ELEC <-  pmef2$PC_G
  pmef2$LCV_ELEC <-  pmef2$LCV_G
  pmef2$TRUCKS_ELEC <-  pmef2$TRUCKS_SL_D
  pmef2$BUS_ELEC <-  pmef2$BUS_URBAN_D
  pmef2$MC_150_ELEC <- pmef2$MC_150_500_ELEC <- pmef2$MC_500_ELEC <- pmef2$MC_150_G

  pmef10$PC_ELEC <-  pmef10$PC_G
  pmef10$LCV_ELEC <-  pmef10$LCV_G
  pmef10$TRUCKS_ELEC <-  pmef10$TRUCKS_SL_D
  pmef10$BUS_ELEC <-  pmef10$BUS_URBAN_D
  pmef10$MC_150_ELEC <- pmef10$MC_150_500_ELEC <- pmef10$MC_500_ELEC <- pmef10$MC_150_G

  ef <- rbind(ef, pmef2, pmef10)

  if(p %in% c("PM25RES", "PM10RES")) {
    if(verbose) message("Experimental: Use only with top-down approach. Units: g/km")
  }
  # tunnel ####
  if(scale %in% c("tunnel","tunnel2014")) {
    if(verbose) cat("scale = tunnel2014\n")
    # conferir valor do tunnel
    # I recalculated the values for tunnel 2014
    # fleet 2014 from CETESB
    # This fleet was used by Daniel as well.- SIE
    # ef <- sysdata$cetesb

    LDV <- c(grep(pattern = "PC_", x = names(ef), value = TRUE),
             grep(pattern = "LCV_", x = names(ef), value = TRUE)[1:4],
             grep(pattern = "MC_", x = names(ef), value = TRUE))

    # Tunnel measurements of heavy trucks covered only big trucks
    HDV <- c(grep(pattern = "TRUCKS_", x = names(ef), value = TRUE))

    # CO HC NMHC (including running losses)

    COHC <- c("CO", "CO_0km","NMHC", "NMHC_0km","HC", "HC_0km",
              "R_0_15", "R_10_25", "R_0_25")

    ef[ef$Pollutant %in% COHC, LDV] <- ef[ef$Pollutant %in% COHC, LDV]*3.770729
    ef[ef$Pollutant %in% COHC, HDV] <- ef[ef$Pollutant %in% COHC, HDV]*2.600306


    # NONO2NOx
    NONO<- c("NOx", "NO2", "NO", "NOx_0km", "NO2_0km", "NO_0km")
    ef[ef$Pollutant %in% NONO, LDV] <- ef[ef$Pollutant %in% NONO, LDV]*1.003158
    ef[ef$Pollutant %in% NONO, HDV] <- ef[ef$Pollutant %in% NONO, HDV]*1.25692

    # PM
    ef[ef$Pollutant %in% "PM", LDV] <- ef[ef$Pollutant %in% "PM", LDV]*1.702909
    ef[ef$Pollutant %in% "PM", HDV] <- ef[ef$Pollutant %in% "PM", HDV]*1.093566

    # ALD # ja tem NMHC incrementado
    # these values i did not change
    efALD <- ef[ef$Pollutant %in% "NMHC", ]
    efALD$Pollutant <- "ALD"
    efALD[, LDV] <- efALD[, LDV]*0.05013932
    efALD[, HDV] <- efALD[, HDV]*0.07298411
    ef <- rbind(ef, efALD)

    # HCHO # ja tem NMHC incrementado
    efHCHO <- ef[ef$Pollutant %in% "NMHC", ]
    efHCHO$Pollutant <- "HCHO"
    efHCHO[, LDV] <- efHCHO[, LDV]*0.03862083
    efHCHO[, HDV] <- efHCHO[, HDV]*0.07298411
    ef <- rbind(ef, efHCHO)

  } else if(scale == "tunnel2018") {
    # Values updated by Mario
    if(verbose) cat("scale = tunnel2018\n")
    # ef <- sysdata$cetesb
    LDV <- c(grep(pattern = "PC_", x = names(ef), value = TRUE),
             grep(pattern = "LCV_", x = names(ef), value = TRUE)[1:4],
             grep(pattern = "MC_", x = names(ef), value = TRUE))

    # Tunnel measurements of heavy trucks covered only big trucks
    HDV <- c(grep(pattern = "TRUCKS_", x = names(ef), value = TRUE))

    # CO HC NMHC (including running losses)
    COHC <- c("CO", "CO_0km","NMHC", "NMHC_0km","HC", "HC_0km",
              "R_0_15", "R_10_25", "R_0_25")

    # CO2
    ef[ef$Pollutant %in% "CO2", LDV] <- ef[ef$Pollutant %in% "CO2", LDV]*0.9623089
    ef[ef$Pollutant %in% "CO2", HDV] <- ef[ef$Pollutant %in% "CO2", HDV]*0.8961876

    ef[ef$Pollutant %in% COHC, LDV] <- ef[ef$Pollutant %in% COHC, LDV]*2.618311
    ef[ef$Pollutant %in% COHC, HDV] <- ef[ef$Pollutant %in% COHC, HDV]*4.777128

    # NOx
    NOx <- c("NOx", "NOx_0km")
    ef[ef$Pollutant %in% NOx, LDV] <- ef[ef$Pollutant %in% NOx, LDV]*0.7839094
    ef[ef$Pollutant %in% NOx, HDV] <- ef[ef$Pollutant %in% NOx, HDV]*1.047166

    # NO
    NO <- c("NO", "NO_0km")
    ef[ef$Pollutant %in% NO, LDV] <- ef[ef$Pollutant %in% NO, LDV]*0.5361807
    ef[ef$Pollutant %in% NO, HDV] <- ef[ef$Pollutant %in% NO, HDV]*1.048273

    # NO2
    NO2 <- c("NO2", "NO2_0km")
    ef[ef$Pollutant %in% NO2, LDV] <- ef[ef$Pollutant %in% NO2, LDV]*1.338307
    ef[ef$Pollutant %in% NO2, HDV] <- ef[ef$Pollutant %in% NO2, HDV]*1.039307

    # # The following adjustments made for tunnel study for 2018 - MEGC
    # PM (EF_LDV = 0.0326574 g/m3, EF_HDV = 0.2749503  mg/m3, agemax = 40)
    ef[ef$Pollutant %in% "PM", LDV] <- ef[ef$Pollutant %in% "PM", LDV]*4.128006
    ef[ef$Pollutant %in% "PM", HDV] <- ef[ef$Pollutant %in% "PM", HDV]*1.626788

    # ALD # ja tem NMHC incrementado
    efALD <- ef[ef$Pollutant %in% "NMHC", ]
    efALD$Pollutant <- "ALD"
    efALD[, LDV] <- efALD[, LDV]*0.05013932
    efALD[, HDV] <- efALD[, HDV]*0.07298411
    ef <- rbind(ef, efALD)

    # HCHO # ja tem NMHC incrementado
    efHCHO <- ef[ef$Pollutant %in% "NMHC", ]
    efHCHO$Pollutant <- "HCHO"
    efHCHO[, LDV] <- efHCHO[, LDV]*0.03862083
    efHCHO[, HDV] <- efHCHO[, HDV]*0.07298411
    ef <- rbind(ef, efHCHO)

  } else {
    if(verbose) cat("scale = default\n")
  }

  ef[is.na(ef)] <- 0
  # vehicle category ####
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

  # Selecting
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

  # agemax
  if(is.data.frame(df)) {
    if(!is.null(agemax)) df <- df[1:agemax, ]
  } else {
    if(!is.null(agemax)) df <- df[1:agemax]
  }


  if(!missing(csv)) {
    data.table::fwrite(x = df, file = csv)
  }
  return(df)
}
