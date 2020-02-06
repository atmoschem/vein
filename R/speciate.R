#' Speciation of emissions
#'
#' @description \code{speciate} separates emissions in different compounds.
#' It covers black carbon and organic matter from particulate matter. Soon it
#' will be added more speciations
#'
#' @param x Emissions estimation
#' @param spec speciation: The speciations are: "bcom", tyre" (or "tire"), "brake", "road",
#' "iag", "nox" and "nmhc". 'iag' now includes a speciation for use of industrial and
#' building paintings. "bcom" stands for black carbon and organic matter. "pmiag"
#' speciates PM2.5 and requires only argument x of PM2.5 emissions in g/h/km^2 as
#' gridded emissions (flux). It also accepts one of the following pollutants:
#' 'e_eth', 'e_hc3', 'e_hc5', 'e_hc8', 'e_ol2', 'e_olt', 'e_oli',
#' 'e_iso', 'e_tol', 'e_xyl', 'e_c2h5oh', 'e_hcho', 'e_ch3oh', 'e_ket',
#' "e_so4i", "e_so4j", "e_no3i", "e_no3j", "e_pm2.5i",
#' "e_pm2.5j", "e_orgi", "e_orgj", "e_eci", "e_ecj". Also "h2o"
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
#' be "G", "D", "LPG", "E85" or "CNG". Not required for "tyre", "brake" or "road".
#' When spec is "nmhc" fuel can be G, D or LPG.
#' @param eu Euro emission standard: "PRE", "ECE_1501", "ECE_1502", "ECE_1503",
#'  "I", "II", "III", "IV",  "V", "III-CDFP","IV-CDFP","V-CDFP", "III-ADFP",
#'  "IV-ADFP","V-ADFP" and "OPEN_LOOP". When spec is "iag" accept the values
#'  "Exhaust" "Evaporative" and "Liquid". When spec is "nox" eu can be
#'  "PRE", "I", "II", "III", "IV", "V", "VI", "VIc", "III-DPF" or "III+CRT".
#'  Not required for "tyre", "brake" or "road"
#' @param show when TRUE shows row of table with respective speciation
#' @param list when TRUE returns a list with number of elements of the list as
#' the number species of pollutants
#' @param pmpar Numeric vector for PM speciation eg:
#' c(e_so4i = 0.0077,  e_so4j = 0.0623,  e_no3i = 0.00247,
#' e_no3j = 0.01053,  e_pm2.5i = 0.1,  e_pm2.5j = 0.3,
#' e_orgi = 0.0304,  e_orgj = 0.1296,  e_eci = 0.056,
#' e_ecj = 0.024,  h2o = 0.277) These are default values. however, when this
#' argument is preseent, new values are used.
#' @importFrom units as_units
#' @importFrom sf st_as_sf st_set_geometry
#' @return dataframe of speciation in grams or mols
#' @references "bcom": Ntziachristos and Zamaras. 2016. Passneger cars, light
#' commercial trucks, heavy-duty vehicles including buses and motor cycles. In:
#' EEA, EMEP. EEA air pollutant emission inventory guidebook-2009. European
#' Environment Agency, Copenhagen, 2016
#' @references "tyre", "brake" and "road": Ntziachristos and Boulter 2016.
#' Automobile tyre and brake wear and road abrasion. In: EEA, EMEP. EEA air
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
#' @note when spec = "iag":
#' veh is only "veh",
#' fuel is "G"  (blended with 25\% ethanol), "D" (blended with 5\% of biodiesel)
#' or "E" (Ethanol 100\%).
#' eu is "Evaporative", "Liquid" or "Exhaust",
#' @note emissions of "pmiag" speciate pm2.5 into e_so4i, e_so4j, e_no3i,
#' e_no3j, e_mp2.5i, e_mp2.5j, e_orgi, e_orgj, e_eci, e_ecj and h2o. Reference:
#' Rafee, S.: Estudo numerico do impacto das emissoes veiculares e fixas da
#' cidade de Manaus nas concentracoes de poluentes atmosfericos da regiao
#' amazonica, Master thesis, Londrina: Universidade Tecnologica Federal do
#' Parana, 2015.
#' @export
#' @examples {
#' # Do not run
#' pm <- rnorm(n = 100, mean = 400, sd = 2)
#' df <- speciate(pm, veh = "PC", fuel = "G", eu = "I")
#' dfa <- speciate(pm, spec = "e_eth", veh = "veh", fuel = "G", eu = "Exhaust")
#' dfb <- speciate(pm, spec = "e_tol", veh = "veh", fuel = "G", eu = "Exhaust")
#' dfc <- speciate(pm, spec = "e_so4i")
#' }
speciate <- function (x, spec = "bcom", veh, fuel, eu, show = FALSE,
                      list = FALSE, pmpar) {
nvoc <- c('e_eth', 'e_hc3', 'e_hc5', 'e_hc8', 'e_ol2', 'e_olt', 'e_oli',
            'e_iso', 'e_tol', 'e_xyl', 'e_c2h5oh', 'e_hcho', 'e_ch3oh', 'e_ket')
pmdf <- data.frame(c("e_so4i", "e_so4j", "e_no3i", "e_no3j", "e_pm2.5i",
                   "e_pm2.5j", "e_orgi", "e_orgj", "e_eci", "e_ecj", "h2o"))

  # black carbon and organic matter
  if (spec=="bcom") {
    bcom <- sysdata$bcom
    df <- bcom[bcom$VEH == veh & bcom$FUEL == fuel & bcom$STANDARD == eu , ]
    dfb <- Emissions(data.frame(BC = x*df$BC/100,
                                OM = (df$OM/100)*(x*df$BC/100)))
    if (show == TRUE) {print(df) } else if (list == TRUE){
      dfb <- as.list(dfb) }
    # tyre ####
  } else if (spec=="tyre" | spec=="tire") {
    df <- data.frame(PM10 = 0.6, PM2.5 = 0.42,PM1 = 0.06,
                     PM0.1 = 0.048)
    dfb <- Emissions(data.frame(PM10 = x*0.6, PM2.5 = x*0.42,PM1 = x*0.06,
                                PM0.1 = x*0.048))
    if (show == TRUE) {
      print(df)
    } else if (list == TRUE){
      dfb <- as.list(dfb)
    }
    # brake ####
  } else if (spec=="brake") {
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
    iag <- sysdata$iag
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
          dfb[[j]][ , i] <- dfb[[j]][ , i] * units::as_units("mol h-1")
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
    # names PMIAG ####
  } else if (spec %in% nvoc) {
    iag <- sysdata$iag
    df <- iag[iag$VEH == veh & iag$FUEL == fuel & iag$STANDARD == eu , spec]
    df <- data.frame(spec = df)
    names(df) <- spec
    if (is.data.frame(x)) {
      for (i in 1:ncol(x)) {
        x[ , i] <- as.numeric(x[ , i])
      }
    }
    if (list == T) {
      dfx <- df
      dfb <- lapply(1:ncol(dfx), function(i){
        dfx[, i]*x/100
      })
      names(dfb) <- names(dfx)
      for (j in 1:length(dfb)) {
        for (i in 1:ncol(x)) {
          dfb[[j]][ , i] <- dfb[[j]][ , i] * units::as_units("mol h-1")
        }
      }
      if (show == TRUE) { print(df) }
    } else {
      dfx <- df
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
    iag <- sysdata$nmhc
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

      if(is.data.frame(x)) {
        for (j in 1:length(dfb)) {
          for (i in 1:ncol(x)) {
            dfb[[j]][ , i] <- dfb[[j]][ , i] * units::as_units("g h-1")
          }
        }
      } else {
        dfb <- lapply(dfb, Emissions)
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
    bcom <- sysdata$nox
    df <- bcom[bcom$VEH == veh & bcom$FUEL == fuel & bcom$STANDARD == eu , ]
    dfb <- Emissions(data.frame(NO2 = x*df$NO2,
                                NO =  x*df$NO))
    if (show == TRUE) {print(df) } else if (list == TRUE){
      dfb <- as.list(dfb)
    }
    # PM2.5 IAG ####
  } else if(spec == "pmiag"){
    message("For emissions grid only, emissions must be in g/(km^2)/h\n")
    message("PM.2.5-10 must be calculated as substraction of PM10-PM2.5 to enter this variable into WRF")
    if(class(x)[1] == "sf"){
      x <- sf::st_set_geometry(x, NULL)
    } else if(class(x) == "Spatial"){
      x <- sf::st_as_sf(x)
      x <- sf::st_set_geometry(x, NULL)
    }
    x$id <- NULL
    # x (g / Xkm^2 / h)
    # x <- x*1000000 # g to micro grams
    # x <- x*(1/1000)^2 # km^2 to m^2
    x <- x/3600#*(dx)^-2  # h to seconds. Consider the DX
    # I think it is wrong to divide by dx^2. Need to reconfirm
    if(!missing(pmpar)) {
      if(length(pmpar) != 11) stop("length 'pmpar' must be 11")
      df <- as.data.frame(matrix(pmpar, ncol = length(pmpar)))
      names(df) <- names(pmpar)
    } else {
      df <- data.frame(e_so4i = 0.0077,
                       e_so4j = 0.0623,
                       e_no3i = 0.00247,
                       e_no3j = 0.01053,
                       e_pm2.5i = 0.1,
                       e_pm2.5j = 0.3,
                       e_orgi = 0.0304,
                       e_orgj = 0.1296,
                       e_eci = 0.056,
                       e_ecj = 0.024,
                       h2o = 0.277)

    }
    if (is.data.frame(x)) {
      for (i in 1:ncol(x)) {
        x[ , i] <- as.numeric(x[ , i])
      }
    }
    if (list == T) {
      dfx <- df
      dfb <- lapply(1:ncol(dfx), function(i){
        dfx[, i]*x
      })
      names(dfb) <- names(dfx)
      for (j in 1:length(dfb)) {
        for (i in 1:ncol(x)) {
          dfb[[j]][ , i] <- dfb[[j]][ , i] * units::as_units("g m-2 s-1")
        }
      }
      if (show == TRUE) { print(df) }
    } else {
      dfx <- df
      dfb <- as.data.frame(lapply(1:ncol(dfx), function(i){
        dfx[, i]*x
      }))
      names(dfb) <- names(dfx)
      for (i in 1:ncol(x)) {
        dfb[ , i] <- dfb[ , i] * units::as_units("g m-2 s-1")
      }
    }
    if (show == TRUE) {
      print(df)
    }
  } else if (spec %in% pmdf) {
    message("To be used in emissions grid only, emissions must be in g/(Xkm^2)/h\n")
    message("PM.2.5-10 must be calculated as substraction of PM10-PM2.5 to enter this variable into WRF")
    if(class(x)[1] == "sf"){
      x <- sf::st_set_geometry(x, NULL)
    } else if(class(x) == "Spatial"){
      x <- sf::st_as_sf(x)
      x <- sf::st_set_geometry(x, NULL)
    }
    x$id <- NULL
    # x (g / Xkm^2 / h)
    # x <- x*1000000 # g to micro grams
    # x <- x*(1/1000)^2 # km^2 to m^2
    x <- x/3600#*(dx)^-2  # h to seconds. Consider the DX
    if(!missing(pmpar)) {
      if(length(pmpar) != 11) stop("length 'pmpar' must be 11")
      df <- as.data.frame(matrix(pmpar, ncol = length(pmpar)))
      names(df) <- names(pmpar)
    } else {
      df <- data.frame(e_so4i = 0.0077,
                       e_so4j = 0.0623,
                       e_no3i = 0.00247,
                       e_no3j = 0.01053,
                       e_pm2.5i = 0.1,
                       e_pm2.5j = 0.3,
                       e_orgi = 0.0304,
                       e_orgj = 0.1296,
                       e_eci = 0.056,
                       e_ecj = 0.024,
                       h2o = 0.277)

    }

    names(df) <- spec
    if (is.data.frame(x)) {
      for (i in 1:ncol(x)) {
        x[ , i] <- as.numeric(x[ , i])
      }
    }
    if (list == T) {
      dfx <- df
      dfb <- lapply(1:ncol(dfx), function(i){
        dfx[, i]*x/100
      })
      names(dfb) <- names(dfx)
      for (j in 1:length(dfb)) {
        for (i in 1:ncol(x)) {
          dfb[[j]][ , i] <- dfb[[j]][ , i] * units::as_units("mol h-1")
        }
      }
      if (show == TRUE) { print(df) }
    } else {
      dfx <- df
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


  }
  return(dfb)
}
