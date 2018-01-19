#' Speciation of emissions
#'
#' @description The separation of emissions in different compunds. It includes
#' black carbon and organic matter from particulate matter. Soon it will be
#' added more speciations
#'
#' @param x Emissions estimation
#' @param spec speciation: The speciations are: "bcom", tyre", "break", "road",
#' "iag", "nox" and "nmhc". 'iag' now includes a speciation for use of industrial and
#' building paintings. "bcom" stands for black carbon and organic matter.
#' @param veh Type of vehicle:
#' When spec is "bcom" or "nox" veh can be "PC", "LCV", HDV" or "Motorcycle".
#' When spec is "iag" veh can take two values depending:
#' when the speciation is for vehicles veh accepts "veh", eu "Evaporative",
#' "Liquid" or "Exhaust" and fuel "G", "E" or "D",
#' when the speciation is for painting, veh is "paint" fuel or eu can be
#' "industrial" or "building"
#' when spec is "nmhc", veh can be "LDV" with fuel "G" or "D" and eu "PRE", "I",
#' "II", "III", "IV", "V", or "VI".
#' when spec is "nmhc", veh can be "HDV" with fuel "D" and eu  "PRE", "I",
#' "II", "III", "IV", "V", or "VI".
#' when spec is "nmhc" and fuel is "LPG", veh and eu must be "ALL"
#' @param fuel Fuel. When spec is "bcom" fuel can be "G" or "D".
#' When spec is "iag" fuel can be "G", "E" or "D". When spec is "nox" fuel can
#' be "G", "D", "LPG", "E85" or "CNG". Not required for "tyre", "break" or "road".
#' When spec is "nmhc" fuel can be G, D or LPG.
#' @param eu Euro emission standard: "PRE", "ECE_1501", "ECE_1502", "ECE_1503",
#'  "I", "II", "III", "IV",  "V", "III-CDFP","IV-CDFP","V-CDFP", "III-ADFP",
#'  "IV-ADFP","V-ADFP" and "OPEN_LOOP". When spec is "iag" accept the values
#'  "Exhaust" "Evaporative" and "Liquid". When spec is "nox" eu can be
#'  "PRE", "I", "II", "III", "IV", "V", "VI", "VIc", "III-DPF" or "III+CRT".
#'  Not required for "tyre", "break" or "road"
#' @param show when TRUE shows row of table with respective speciation
#' @param list when TRUE returns a list with number of elements of the list as
#' the number species of pollutants
#' @importFrom units parse_unit
#' @return dataframe of speciation in grams or mols
#' @references "bcom": Ntziachristos and Zamaras. 2016. Passneger cars, light
#' commercial trucks, heavy-duty vehicles including buses and motor cycles. In:
#' EEA, EMEP. EEA air pollutant emission inventory guidebook-2009. European
#' Environment Agency, Copenhagen, 2016
#' @references "tyre", "break" and "road": Ntziachristos and Boulter 2016.
#' Automobile tyre and break wear and road abrasion. In: EEA, EMEP. EEA air
#' pollutant emission inventory
#' guidebook-2009. European Environment Agency, Copenhagen, 2016
#' @references "iag": Ibarra-Espinosa S. Air pollution modeling in Sao Paulo
#' using bottom-up vehicular emissions inventories. 2017. PhD thesis. Instituto de
#' Astronomia, Geofisica e Ciencias Atmosfericas, Universidade de Sao Paulo,
#' Sao Paulo, page 88.
#' Speciate EPA: https://cfpub.epa.gov/speciate/. :
#' K. Sexton, H. Westberg, "Ambient hydrocarbon and ozone measurements downwind
#' of a large automotive painting plant" Environ. Sci. Tchnol. 14:329 (1980).P.A.
#' Scheff, R.A. Schauer, James J., Kleeman, Mike J., Cass, Glen R., Characterization
#' and Control of Organic Compounds Emitted from Air Pollution Sources, Final Report,
#' Contract 93-329, prepared for California Air Resources Board Research Division,
#' Sacramento, CA, April 1998.
#' 2004 NPRI National Databases as of April 25, 2006,
#' http://www.ec.gc.ca/pdb/npri/npri_dat_rep_e.cfm. Memorandum
#' Proposed procedures for preparing composite speciation profiles using
#' Environment Canada s National Pollutant Release Inventory (NPRI) for
#' stationary sources, prepared by Ying Hsu and Randy Strait of E.H. Pechan
#' Associates, Inc. for David Niemi, Marc Deslauriers, and Lisa Graham of
#' Environment Canada, September 26, 2006.
#'
#' @note when spec = "iag", veh is only "VEH", STANDARD is "Evaporative",
#' "Liquid" or "Exhaust", FUEL is "G" for gasoline (blended with 25\% ethanol),
#'  "E" for Ethanol and "D" for diesel (blended with 5\% of biodiesel).
#'  When spec = "bcom", veh can be "PC", "LCV", "Motorcycle" or "HDV"
#'   VEH", STANDARD is "Evaporative",
#' "Liquid" or "Exhaust", FUEL is "G" for gasoline (blended with 25\% ethanol),
#'  "E" for Ethanol and "D" for diesel (blended with 5\% of biodiesel).
#' @export
#' @examples \dontrun{
#' # Do not run
#' pm <- rnorm(n = 100, mean = 400, sd = 2)
#' df <- speciate(pm, veh="PC", fuel="G", eu="I")
#' }
speciate <- function (x, spec = "bcom", veh, fuel, eu, show = FALSE, list = FALSE) {
  if(x$id){
    stop("Remove 'id'")
  }
  # black carbon and organic matter
  if (spec=="bcom") {
    bcom <- sysdata[[4]]
    df <- bcom[bcom$VEH == veh & bcom$FUEL == fuel & bcom$STANDARD == eu , ]
    dfb <- Emissions(data.frame(BC = x*df$BC/100,
                                OM = (df$OM/100)*(x*df$BC/100)))
    if (show == TRUE) {print(df) } else if (list == TRUE){
      dfb <- as.list(dfb) }
    # tyre ####
  } else if (spec=="tyre") {
    df <- data.frame(PM10 = 0.6, PM2.5 = 0.42,PM1 = 0.06,
                     PM0.1 = 0.048)
    dfb <- Emissions(data.frame(PM10 = x*0.6, PM2.5 = x*0.42,PM1 = x*0.06,
                                PM0.1 = x*0.048))
    if (show == TRUE) {
      print(df)
    } else if (list == TRUE){
      dfb <- as.list(dfb)
    }
    # break ####
  } else if (spec=="break") {
    df <- data.frame(PM10 = 0.98, PM2.5 = 0.39,PM1 = 0.1,
                     PM0.1 = 0.08)
    dfb <- Emissions(data.frame(PM10 = x*0.98, PM2.5 = x*0.39,PM1 = x*0.1,
                                PM0.1 = x*0.08))
    if (show == TRUE) {
      print(df)
    } else if (list == TRUE){
      dfb <- as.list(dfb)
    }
    # road ####
  } else if (spec=="road") {
    df <- data.frame(PM10 = 0.5, PM2.5 = 0.27)
    dfb <- Emissions(data.frame(PM10 = x*0.5, PM2.5 = x*0.27))
    if (show == TRUE) {print(df) } else if (list == TRUE){
      dfb <- as.list(dfb)
    }
    # iag ####
  } else if (spec=="iag") {
    iag <- sysdata[[6]]
    df <- iag[iag$VEH == veh & iag$FUEL == fuel & iag$STANDARD == eu , ]
    if (is.data.frame(x)) {
      for (i in 1:ncol(x)) {
        x[ , i] <- as.numeric(x[ , i])
      }
    }
    if (list == T) {
      dfx <- df[, 4:ncol(df)]
      dfb <- lapply(1:ncol(dfx), function(i){
        dfx[, i]*x/100
      })
      names(dfb) <- names(dfx)
      for (j in 1:length(dfb)) {
        for (i in 1:ncol(x)) {
          dfb[[j]][ , i] <- dfb[[j]][ , i] * units::parse_unit("mol h-1")
        }
      }
      if (show == TRUE) { print(df) }
    } else {
      dfx <- df[, 4:ncol(df)]
      dfb <- as.data.frame(lapply(1:ncol(dfx), function(i){
        dfx[, i]*x/100
      }))
      names(dfb) <- names(dfx)
      # e_eth, e_hc3, e_hc5, e_hc8, e_ol2, e_olt, e_oli, e_iso, e_tol,
      # e_xyl, e_c2h5oh, e_hcho /100 because it is based on 100g of fuel
      # e_ch3oh and e_ket /100 because it is percentage
    }
    if (show == TRUE) {
      print(df)
    }
    # nmhc ####
  } else if (spec=="nmhc") {
    iag <- sysdata[[9]]
    df <- iag[iag$VEH == veh & iag$FUEL == fuel & iag$STANDARD == eu , ]
    if (is.data.frame(x)) {
      for (i in 1:ncol(x)) {
        x[ , i] <- as.numeric(x[ , i])
      }

    }
    if (list == T) {
      dfx <- df[, 4:ncol(df)]
      dfb <- lapply(1:ncol(dfx), function(i){
        dfx[, i]*x/100 #percentage
      })
      names(dfb) <- names(dfx)

      for (j in 1:length(dfb)) {
        for (i in 1:ncol(x)) {
          dfb[[j]][ , i] <- dfb[[j]][ , i] * units::parse_unit("g h-1")
        }
      }

      if (show == TRUE) { print(df) }
    } else {
      dfx <- df[, 4:ncol(df)]
      dfb <- as.data.frame(lapply(1:ncol(dfx), function(i){
        dfx[, i]*x/100
      }))
      names(dfb) <- names(dfx)

    }
    if (show == TRUE) {
      print(df)
    }
    # nox ####
  } else if (spec=="nox") {
    bcom <- sysdata[[7]]
    df <- bcom[bcom$VEH == veh & bcom$FUEL == fuel & bcom$STANDARD == eu , ]
    dfb <- Emissions(data.frame(NO2 = x*df$NO2,
                                NO =  x*df$NO))
    if (show == TRUE) {print(df) } else if (list == TRUE){
      dfb <- as.list(dfb)
    }
  }
  return(dfb)
}
