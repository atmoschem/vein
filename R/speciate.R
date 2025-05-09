#' Speciation of emissions
#'
#' @description \code{speciate} separates emissions in different compounds.
#' It covers black carbon and organic matter from particulate matter. Soon it
#' will be added more speciations
#'
#' @param x Emissions estimation
#' @param spec The speciations are:
#' \itemize{
#' \item "bcom": Splits PM2.5 in black carbon and organic matter.
#' \item "tyre" or "tire": Splits PM in PM10, PM2.5, PM1 and PM0.1.
#' \item "brake": Splits PM in PM10, PM2.5, PM1 and PM0.1.
#' \item "road": Splits PM in PM10 and PM2.5.
#' \item "nox": Splits NOx in NO and NO2.
#' \item "nmhc": Splits NMHC in compounds, see \code{\link{ef_ldv_speed}}.
#' \item "voc": Splits NMHC in voc groups according EDGAR-CAMS.
#' \item "pmiag", "pmneu",  "pmneu2", "pm2023", "pm2025": Splits PM in groups, see note below.
#' }
#' @param veh Type of vehicle:
#' \itemize{
#' \item "bcom": veh can be "PC", "LCV", HDV" or "Motorcycle".
#' \item "tyre" or "tire": not necessary.
#' \item "brake": not necessary.
#' \item "road": not necessary.
#' \item "nox": veh can be "PC", "LCV", HDV" or "Motorcycle".
#' \item "nmhc":see below
#' \item "voc": read options while running.
#' \item ""pmiag", "pmneu",  "pmneu2", "pm2023": not necessary.
#' \item "pm2025": "LDV", "HDV"
#' }
#' @param fuel Fuel.
#' \itemize{
#' \item "bcom": "G" or "D".
#' \item "tyre" or "tire": not necessary.
#' \item "brake": not necessary.
#' \item "road": not necessary.
#' \item "nox": "G", "D", "LPG", "E85" or "CNG".
#' \item "nmhc":see below
#' \item "voc": read options while running.
#' \item "pmiag", "pmneu",  "pmneu2", "pm2023", "pm2025": not necessary.
#' }
#' @param eu Emission standard
#' \itemize{
#' \item "bcom": "G" or "D".
#' \item "tyre" or "tire": not necessary.
#' \item "brake": not necessary.
#' \item "road": not necessary.
#' \item "nox": "G", "D", "LPG", "E85" or "CNG".
#' \item "nmhc":see below
#' \item "voc": read options while running.
#' \item "pmiag", "pmneu",  "pmneu2", "pm2023", "pm2025": not necessary.
#' }
#' @param list when TRUE returns a list with number of elements of the list as
#' the number species of pollutants
#' @param pmpar Numeric vector for PM speciation eg:
#' c(e_so4i = 0.0077,  e_so4j = 0.0623,  e_no3i = 0.00247,
#' e_no3j = 0.01053,  e_pm25i = 0.1,  e_pm25j = 0.3,
#' e_orgi = 0.0304,  e_orgj = 0.1296,  e_eci = 0.056,
#' e_ecj = 0.024,  h2o = 0.277) These are default values. however, when this
#' argument is present, new values are used.
#' @param verbose Logical to show more information
#'
#' @note options for spec "nmhc":
#' \tabular{ccc}{
#'    veh \tab fuel \tab eu          \cr
#'    LDV \tab G    \tab PRE         \cr
#'    LDV \tab G    \tab I           \cr
#'    LDV \tab D    \tab all         \cr
#'    HDV \tab D    \tab all         \cr
#'    LDV \tab LPG  \tab all         \cr
#'    LDV \tab G    \tab Evaporative \cr
#'    LDV \tab E25  \tab Evaporative \cr
#'    LDV \tab E100 \tab Evaporative \cr
#'    LDV \tab E25  \tab Exhaust     \cr
#'    LDV \tab E100 \tab Exhaust     \cr
#'    HDV \tab B5   \tab Exhaust     \cr
#'    LDV \tab E85  \tab Exhaust     \cr
#'    LDV \tab E85  \tab Evaporative \cr
#'    LDV \tab CNG  \tab Exhaust     \cr
#'    ALL \tab E100 \tab Liquid      \cr
#'    ALL \tab G    \tab Liquid      \cr
#'    ALL \tab E25  \tab Liquid      \cr
#'    ALL \tab ALL  \tab OM          \cr
#'    LDV \tab G    \tab OM-001      \cr
#'    LDV \tab D    \tab OM-002      \cr
#'    HDV \tab D    \tab OM-003      \cr
#'    MC  \tab G    \tab OM-004      \cr
#'    ALL  \tab LPG \tab OM-005      \cr
#'    LDV \tab G    \tab OM-001-001  \cr
#'    LDV \tab G    \tab OM-001-002  \cr
#'    LDV \tab G    \tab OM-001-003  \cr
#'    LDV \tab G    \tab OM-001-004  \cr
#'    LDV \tab G    \tab OM-001-005  \cr
#'    LDV \tab G    \tab OM-001-006  \cr
#'    LDV \tab G    \tab OM-001-007  \cr
#'    LDV \tab D    \tab OM-002-001  \cr
#'    LDV \tab D    \tab OM-002-002  \cr
#'    LDV \tab D    \tab OM-002-003  \cr
#'    LDV \tab D    \tab OM-002-004  \cr
#'    LDV \tab D    \tab OM-002-005  \cr
#'    LDV \tab D    \tab OM-002-006  \cr
#'    HDV \tab D    \tab OM-003-001  \cr
#'    HDV \tab D    \tab OM-003-002  \cr
#'    HDV \tab D    \tab OM-003-003  \cr
#'    HDV \tab D    \tab OM-003-004  \cr
#'    HDV \tab D    \tab OM-003-005  \cr
#'    HDV \tab D    \tab OM-003-006  \cr
#'    MC  \tab G    \tab OM-004-001  \cr
#'    MC  \tab G    \tab OM-004-002  \cr
#'    MC  \tab G    \tab OM-004-003  \cr
#'    ALL  \tab ALL    \tab urban  \cr
#'    ALL  \tab ALL    \tab highway  \cr
#' }
#'
#' after eu = OM, all profiles are Chinese
#' # the following specs will be removed soon
#' \itemize{
#' \item"iag_racm": ethanol emissions added in hc3.
#' \item"iag" or "iag_cb05": Splits NMHC by CB05 (WRF exb05_opt1) group .
#' \item"petroiag_cb05": Splits NMHC by CB05 (WRF exb05_opt1) group .
#' \item"iag_cb05v2": Splits NMHC by CB05 (WRF exb05_opt2) group .
#' \item"neu_cb05": Splits NMHC by CB05 (WRF exb05_opt2) group alternative.
#' \item"petroiag_cb05v2": Splits NMHC by CB05 (WRF exb05_opt2) group alternative.
#' }
#' @importFrom units as_units
#' @importFrom sf st_as_sf st_set_geometry
#' @importFrom data.table as.data.table dcast.data.table
#' @return dataframe of speciation in grams or mols
#' @references "bcom": Ntziachristos and Zamaras. 2016. Passenger cars, light
#' commercial trucks, heavy-duty vehicles including buses and motorcycles. In:
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
#' @note spec \strong{"pmiag"} speciate pm2.5 into e_so4i, e_so4j, e_no3i,
#' e_no3j, e_mp2.5i, e_mp2.5j, e_orgi, e_orgj, e_eci, e_ecj and h2o. Reference:
#' Rafee, S.: Estudo numerico do impacto das emissoes veiculares e fixas da
#' cidade de Manaus nas concentracoes de poluentes atmosfericos da regiao
#' amazonica, Master thesis, Londrina: Universidade Tecnologica Federal do
#' Parana, 2015.
#'
#' specs: "neu_cb05", "pmneu" and "pmneu2" provided by Daniel Schuch,
#' from Northeastern University.
#' "pm2023" provided by Iara da Silva; Leila D. Martins
#'
#' Speciation with fuels \strong{"E25", "E100" and "B5"} made by Prof. Leila Martins (UTFPR),
#' represents BRAZILIAN fuel
#'
#'
#' pmiag2 pass the mass only on j fraction
#'
#' @note spec \strong{"voc"} splits nmhc into the 25 VOC
#' groups according: Huang et al 2019, "Speciation of anthropogenic
#' emissions of non-methane volatile
#' organic compounds: a global gridded data set for
#' 1970-2012" ACP. Speciation In development.
#' @export
#' @examples
#' \dontrun{
#' # Do not run
#' pm <- rnorm(n = 100, mean = 400, sd = 2)
#' (df <- speciate(pm, veh = "PC", fuel = "G", eu = "I"))
#' (df <- speciate(pm, spec = "brake", veh = "PC", fuel = "G", eu = "I"))
#' (dfa <- speciate(pm, spec = "iag", veh = "veh", fuel = "G", eu = "Exhaust"))
#' (dfb <- speciate(pm, spec = "iag_cb05v2", veh = "veh", fuel = "G", eu = "Exhaust"))
#' (dfb <- speciate(pm, spec = "neu_cb05", veh = "veh", fuel = "G", eu = "Exhaust"))
#' pm <- units::set_units(pm, "g/km^2/h")
#' #(dfb <- speciate(as.data.frame(pm), spec = "pmiag", veh = "veh", fuel = "G", eu = "Exhaust"))
#' for (i in 1:ncol(dfb)) {
#'     dfb[, i] <- units::set_units(dfb[, i], "ug/m^2/s")
#'   }
#' #(dfb <- speciate(as.data.frame(pm), spec = "pmneu", veh = "veh", fuel = "G", eu = "Exhaust"))
#' #(dfb <- speciate(as.data.frame(pm), spec = "pmneu2", veh = "veh", fuel = "G", eu = "Exhaust"))
#' (dfb <- speciate(as.data.frame(pm), spec = "pm2025", veh = "LDV"))
#' #(dfb <- speciate(as.data.frame(pm), spec = "pm2025", veh = "HDV"))
#' # new
#' (pah <- speciate(spec = "pah", veh = "LDV", fuel = "G", eu = "I"))
#' (xs <- speciate(spec = "pcdd", veh = "LDV", fuel = "G", eu = "I"))
#' (xs <- speciate(spec = "pmchar", veh = "LDV", fuel = "G", eu = "I"))
#' (xs <- speciate(spec = "metals", veh = "LDV", fuel = "G", eu = "all"))
#'  dx1 <- speciate(
#'  x = pm,
#'  spec = "voc",
#'  fuel = "E25",
#'  veh = "LDV",
#'  eu = "Exhaust")
#' }
speciate <- function(x = 1,
                     spec = "bcom",
                     veh,
                     fuel,
                     eu,
                     list = FALSE,
                     pmpar,
                     verbose = FALSE) {
  nvoc <- c(
    "e_eth", "e_hc3", "e_hc5", "e_hc8", "e_ol2", "e_olt", "e_oli",
    "e_iso", "e_tol", "e_xyl", "e_c2h5oh", "e_hcho", "e_ch3oh", "e_ket"
  )

  pmdf <- data.frame(c(
    "e_so4i", "e_so4j", "e_no3i", "e_no3j", "e_pm25i",
    "e_pm25j", "e_orgi", "e_orgj", "e_eci", "e_ecj", "h2o"
  ))

  # bcom black carbon and organic matter####
  if (spec == "bcom") {
    bcom <- sysdata$bcom
    df <- bcom[bcom$VEH == veh & bcom$FUEL == fuel & bcom$STANDARD == eu, ]
    dfb <- Emissions(data.frame(
      BC = x * df$BC / 100,
      OM = (df$OM / 100) * (x * df$BC / 100)
    ))
    if (list == TRUE) {
      dfb <- as.list(dfb)
    }
    # tyre ####
  } else if (spec == "tyre" | spec == "tire") {
    df <- data.frame(
      PM10 = 0.6,
      PM2.5 = 0.42,
      PM1 = 0.06,
      PM0.1 = 0.048
    )
    dfb <- Emissions(data.frame(
      PM10 = x * 0.6,
      PM2.5 = x * 0.42,
      PM1 = x * 0.06,
      PM0.1 = x * 0.048
    ))
    if (list == TRUE) {
      dfb <- as.list(dfb)
    }
    # brake ####
  } else if (spec == "brake") {
    df <- data.frame(
      PM10 = 0.98,
      PM2.5 = 0.39,
      PM1 = 0.1,
      PM0.1 = 0.08
    )
    dfb <- Emissions(data.frame(
      PM10 = x * 0.98,
      PM2.5 = x * 0.39,
      PM1 = x * 0.1,
      PM0.1 = x * 0.08
    ))
    if (list == TRUE) {
      dfb <- as.list(dfb)
    }
    # road ####
  } else if (spec == "road") {
    df <- data.frame(PM10 = 0.5,
                     PM2.5 = 0.27)
    dfb <- Emissions(data.frame(PM10 = x * 0.5,
                                PM2.5 = x * 0.27))
    if (list == TRUE) {
      dfb <- as.list(dfb)
    }
    # iag ####
  } else if (spec %in% c("iag", "iag_cb05", "iag_cb05v2", "neu_cb05", "iag_racm",
                         "petroiag_cb05", "petroiag_cb05v2")) {
    iag <- sysdata$iag

    spec <- ifelse(spec == "iag", "iag_cb05", spec)

    iag <- iag[iag$mech == spec, ]

    iag$VEH_FUEL_STANDARD <- paste(iag$VEH,
                                   iag$FUEL,
                                   iag$STANDARD,
                                   sep = "_")

    VEH_FUEL_STANDARD <- groups <- NULL

    iag2 <- data.table::dcast.data.table(
      data.table::as.data.table(iag),
      formula = VEH_FUEL_STANDARD ~ groups,
      value.var = "x")

    iag2 <- as.data.frame(iag2)

    iag2 <- cbind(
      iag2,
      do.call(
        "rbind",
        strsplit(
          x = iag2$VEH_FUEL_STANDARD,
          split = "_"
        )
      )
    )
    names(iag2)[ncol(iag2)] <- "STANDARD"
    names(iag2)[ncol(iag2) - 1] <- "FUEL"
    names(iag2)[ncol(iag2) - 2] <- "VEH"
    iag <- iag2
    iag$VEH_FUEL_STANDARD <- NULL

    df <- iag[iag$VEH == veh &
                iag$FUEL == fuel &
                iag$STANDARD == eu, ]

    df <- df[, 1:(ncol(df) - 3)]

    if (is.data.frame(x)) {
      for (i in 1:ncol(x)) {
        x[, i] <- as.numeric(x[, i])
      }
    }


    if (list == T) {
      dfx <- df[, 1:ncol(df)]

      dfb <- lapply(1:ncol(dfx), function(i) {
        dfx[, i] * x / 100
      })

      names(dfb) <- names(dfx)
      for (j in 1:length(dfb)) {
        for (i in 1:ncol(x)) {
          dfb[[j]][, i] <- dfb[[j]][, i] * units::as_units("mol h-1")
        }
      }

    } else {
      dfx <- df[, 1:ncol(df)]


      dfb <- as.data.frame(lapply(1:ncol(dfx), function(i) {
        dfx[, i] * x / 100
      }))
      names(dfb) <- names(dfx)
    }

    names(df) <- toupper(names(df))

    # nmhc ####
  } else if (spec == "nmhc") {
    nmhc <- sysdata$nmhc

    if(!veh %in% unique(nmhc$veh)) {
      choice <- utils::menu(unique(nmhc$veh), title="Choose veh")
      veh <- unique(nmhc$veh)[choice]
    }
    nmhc <- nmhc[nmhc$veh == veh , ]

    if(!fuel %in% unique(nmhc$fuel)) {
      choice <- utils::menu(unique(nmhc$fuel), title="Choose fuel")
      fuel <- unique(nmhc$fuel)[choice]
    }
    nmhc <- nmhc[nmhc$fuel == fuel , ]

    if(!eu %in% unique(nmhc$eu)) {
      choice <- utils::menu(unique(nmhc$eu), title="Choose eu")
      eu <- unique(nmhc$eu)[choice]
    }
    df <- nmhc[nmhc$eu == eu , ]


    if (list == T) {

      dfb <- lapply(1:nrow(df), function(i) {
        df[i, ]$x * x / 100
      })
      names(dfb) <- df$species

    } else {

      dfb <- data.table::rbindlist(lapply(1:nrow(df), function(i) {
        data.frame(x = df[i, ]$x * x / 100,
                   pol = df$species[i])
      }))
      if(!is.null(names(x))) names(dfb) <- c(names(x), "pol")
    }


    # voc ####
  } else if (spec == "voc") {
    nmhc <- sysdata$mech
    nmhc <- as.data.frame(nmhc)

    if(!veh %in% unique(nmhc$veh)) {
      choice <- utils::menu(unique(nmhc$veh),
                            title="Choose veh")
      veh <- unique(nmhc$veh)[choice]
    }
    nmhc <- nmhc[nmhc$veh == veh , ]

    if(!fuel %in% unique(nmhc$fuel)) {
      choice <- utils::menu(unique(nmhc$fuel),
                            title="Choose fuel")
      fuel <- unique(nmhc$fuel)[choice]
    }

    nmhc <- nmhc[nmhc$fuel == fuel , ]

    if(!eu %in% unique(nmhc$eu)) {
      choice <- utils::menu(unique(nmhc$eu),
                            title="Choose eu")
      eu <- unique(nmhc$eu)[choice]
    }

    df <- nmhc[nmhc$eu == eu , ]

    df <- data.table::as.data.table(df)
    voc <- NULL
    df <- df[, sum(x), by = voc]

    names(df)[2] <- "x"

    if (list == T) {

      dfb <- lapply(1:nrow(df), function(i) {
        df[i, ]$x * x / 100
      })
      names(dfb) <- df$voc

    } else {

      dfb <- data.table::rbindlist(lapply(1:nrow(df), function(i) {
        data.frame(x = df[i, ]$x * x / 100,
                     pol = df$voc[i])
      }))
      if(!is.null(names(x))) names(dfb) <- c(names(x), "pol")
    }



    # pah ####
  } else if (spec == "pah") {
    nmhc <- sysdata$pah

    if(!veh %in% unique(nmhc$veh)) {
      choice <- utils::menu(unique(nmhc$veh), title="Choose veh")
      veh <- unique(nmhc$veh)[choice]
    }
    nmhc <- nmhc[nmhc$veh == veh , ]

    if(!fuel %in% unique(nmhc$fuel)) {
      choice <- utils::menu(unique(nmhc$fuel), title="Choose fuel")
      fuel <- unique(nmhc$fuel)[choice]
    }
    nmhc <- nmhc[nmhc$fuel == fuel , ]

    if(!eu %in% unique(nmhc$eu)) {
      choice <- utils::menu(unique(nmhc$eu), title="Choose eu")
      eu <- unique(nmhc$eu)[choice]
    }
    df <- nmhc[nmhc$eu == eu , ]


    if (list == T) {

      dfb <- lapply(1:nrow(df), function(i) {
        df[i, ]$x
      })
      names(dfb) <- df$species

    } else {

      dfb <- data.table::rbindlist(lapply(1:nrow(df), function(i) {
        data.frame(x = df[i, ]$x,
                   pol = df$species[i])
      }))
      if(!is.null(names(x))) names(dfb) <- c(names(x), "pol")
    }

    # pcdd ####
  } else if (spec == "pcdd") {
    nmhc <- sysdata$pcdd
    nmhc$x <- nmhc$x*1e-12

    if(!veh %in% unique(nmhc$veh)) {
      choice <- utils::menu(unique(nmhc$veh), title="Choose veh")
      veh <- unique(nmhc$veh)[choice]
    }
    nmhc <- nmhc[nmhc$veh == veh , ]

    if(!fuel %in% unique(nmhc$fuel)) {
      choice <- utils::menu(unique(nmhc$fuel), title="Choose fuel")
      fuel <- unique(nmhc$fuel)[choice]
    }
    nmhc <- nmhc[nmhc$fuel == fuel , ]

    if(!eu %in% unique(nmhc$eu)) {
      choice <- utils::menu(unique(nmhc$eu), title="Choose eu")
      eu <- unique(nmhc$eu)[choice]
    }
    df <- nmhc[nmhc$eu == eu , ]


    if (list == T) {

      dfb <- lapply(1:nrow(df), function(i) {
        df[i, ]$x
      })
      names(dfb) <- df$species

    } else {

      dfb <- data.table::rbindlist(lapply(1:nrow(df), function(i) {
        data.frame(x = df[i, ]$x,
                   pol = df$species[i])
      }))
      if(!is.null(names(x))) names(dfb) <- c(names(x), "pol")
    }

    # metals ####
  } else if (spec %in% c("metal", "metals")) {
    message("multiplies fuel consumption (g/km)")
    nmhc <- sysdata$metals

    if(!veh %in% unique(nmhc$veh)) {
      choice <- utils::menu(unique(nmhc$veh), title="Choose veh")
      veh <- unique(nmhc$veh)[choice]
    }
    nmhc <- nmhc[nmhc$veh == veh , ]

    if(!fuel %in% unique(nmhc$fuel)) {
      choice <- utils::menu(unique(nmhc$fuel), title="Choose fuel")
      fuel <- unique(nmhc$fuel)[choice]
    }
    nmhc <- nmhc[nmhc$fuel == fuel , ]

    if(!eu %in% unique(nmhc$eu)) {
      choice <- utils::menu(unique(nmhc$eu), title="Choose eu")
      eu <- unique(nmhc$eu)[choice]
    }
    df <- nmhc[nmhc$eu == eu , ]


    if (list == T) {

      dfb <- lapply(1:nrow(df), function(i) {
        df[i, ]$x
      })
      names(dfb) <- df$species

    } else {

      dfb <- data.table::rbindlist(lapply(1:nrow(df), function(i) {
        data.frame(x = df[i, ]$x*x,
                   pol = df$species[i])
      }))
      if(!is.null(names(x))) names(dfb) <- c(names(x), "pol")
    }


    # pmchar ####
  } else if (spec %in% c("pmchar")) {
    nmhc <- sysdata$pmchar
    nmhc$units <- ifelse(nmhc$species %in% grep(pattern = "AS_",
                                                x = nmhc$species,
                                                value = TRUE),
                         "cm2/km"," N/km")

    if(!veh %in% unique(nmhc$veh)) {
      choice <- utils::menu(unique(nmhc$veh), title="Choose veh")
      veh <- unique(nmhc$veh)[choice]
    }
    nmhc <- nmhc[nmhc$veh == veh , ]

    if(!fuel %in% unique(nmhc$fuel)) {
      choice <- utils::menu(unique(nmhc$fuel), title="Choose fuel")
      fuel <- unique(nmhc$fuel)[choice]
    }
    nmhc <- nmhc[nmhc$fuel == fuel , ]

    if(!eu %in% unique(nmhc$eu)) {
      choice <- utils::menu(unique(nmhc$eu), title="Choose eu")
      eu <- unique(nmhc$eu)[choice]
    }
    df <- nmhc[nmhc$eu == eu , ]


    if (list == T) {

      dfb <- lapply(1:nrow(df), function(i) {
        df[i, ]$x
      })
      names(dfb) <- df$species

    } else {

      dfb <- data.table::rbindlist(lapply(1:nrow(df), function(i) {
        data.frame(x = df[i, ]$x*x,
                   pol = df$species[i],
                   unit = df$units[i])
      }))
      if(!is.null(names(x))) names(dfb) <- c(names(x), "pol")
    }

    # nox ####
  } else if (spec == "nox") {
    bcom <- sysdata$nox
    df <- bcom[bcom$VEH == veh & bcom$FUEL == fuel & bcom$STANDARD == eu, ]
    dfb <- Emissions(data.frame(
      NO2 = x * df$NO2,
      NO = x * df$NO
    ))
    if (list == TRUE) {
      dfb <- as.list(dfb)
    }
    # PM ####
  } else if (spec %in% c("pmiag", "pmneu", "pmneu2","pm2023", "pm2025")) {

    if (inherits(x, "sf")) {
      x <- sf::st_set_geometry(x, NULL)
    } else if (inherits(x, "Spatial")) {
      x <- sf::st_as_sf(x)
      x <- sf::st_set_geometry(x, NULL)
    }
    x$id <- NULL

    # x (g / Xkm^2 / h)
    # x <- x*1000000 # g to micro grams
    # x <- x*(1/1000)^2 # km^2 to m^2
    # x <- x/3600#*(dx)^-2  # h to seconds. Consider the DX
    if (!missing(pmpar)) {
      if (length(pmpar) != 11) stop("length 'pmpar' must be 11")
      df <- as.data.frame(matrix(pmpar, ncol = length(pmpar)))
      names(df) <- names(pmpar)
    } else {
      if (spec == "pmiag") {
        df <- data.frame(
          e_so4i = 0.0077,
          e_so4j = 0.0623,
          e_no3i = 0.00247,
          e_no3j = 0.01053,
          e_pm25i = 0.1,
          e_pm25j = 0.3,
          e_orgi = 0.0304,
          e_orgj = 0.1296,
          e_eci = 0.056,
          e_ecj = 0.024#,h2o = 0.277
        )
      } else if (spec == "pmneu") {
        df <- data.frame(
          e_so4i = 0,
          e_so4j = 0.0077 + 0.0623,
          e_no3i = 0,
          e_no3j = 0.00247 + 0.01053,
          e_pm25i = 0,
          e_pm25j = 0.1 + 0.3,
          e_orgi = 0,
          e_orgj = 0.0304 + 0.1296,
          e_eci = 0,
          e_ecj = 0.056 + 0.024#,h2o = 0.277
        )
      } else if (spec == "pm2023") {
        df <- data.frame(
          e_so4i = 0.00646,
          e_so4j = 0.04104,
          e_no3i = 0.004025,
          e_no3j = 0.013475,
          e_pm25i = 0.114,
          e_pm25j = 0.342,
          e_orgi = 0.041515,
          e_orgj = 0.176985,
          e_eci = 0.24487,
          e_ecj = 0.01563#,h2o = 0.277
        )
        #pm2025
      } else if (spec == "pm2025") {
        if(veh == "LDV") {

          df <- data.frame(
            e_so4i = 0.00087,
          e_so4j = 0.00553,
          e_no3i = 0.00000,
          e_no3j = 0.00000,
          e_orgi = 0.04535,
          e_orgj = 0.19335,
          e_eci = 0.22360,
          e_ecj = 0.01427,
          e_pm25i = 0.12925,
          e_pm25j = 0.38776
          )

        } else if(veh == "HDV") {
          df <- data.frame(
          e_so4i = 0.00796,
          e_so4j = 0.05056,
          e_no3i = 0.00876,
          e_no3j = 0.02933,
          e_orgi = 0.07032,
          e_orgj = 0.29976,
          e_eci = 0.50131,
          e_ecj = 0.03200,
          e_pm25i = 0.00000,
          e_pm25j = 0.00000
          )

        } else{
          stop("when spec = pm2025, veh can be LDV or HDV")
        }

      }  else if (spec == "pmneu2") {
        df <- data.frame(
          e_so4i = 0,
          e_so4j = 0.07,
          e_no3i = 0,
          e_no3j = 0.015,
          e_pm25i = 0,
          e_pm25j = 0.3,
          e_orgi = 0,
          e_orgj = 0.35,
          e_eci = 0,
          e_ecj = 0.18#,h2o = 0.277
        )
      }
    }

    names(df) <- toupper(names(df))
    if (list == T) {
      dfx <- df
      dfb <- lapply(1:ncol(dfx), function(i) {
        dfx[, i] * x
      })
      names(dfb) <- names(dfx)
      # for (j in 1:length(dfb)) {
      #   for (i in 1:ncol(x)) {
      #     dfb[[j]][ , i] <-   units::set_units(dfb[[j]][ , i], "g/m^2/s")
      #   }
      # }

    } else {
      dfx <- df
      dfb <- as.data.frame(lapply(1:ncol(dfx), function(i) {
        dfx[, i] * x
      }))
      names(dfb) <- names(dfx)
      # for (i in 1:ncol(x)) {
      #   dfb[ , i] <- dfb[ , i] * units::as_units("g m-2 s-1")
      # }
    }

  } else {
    stop("Selelect another `spec`")
  }
  return(dfb)
}
