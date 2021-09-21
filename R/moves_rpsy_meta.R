#' MOVES estimation of using rates per start by model year
#'
#' @description \code{\link{moves_rpsy_meta}} estimates running exhaust emissions
#' using MOVES emission factors.
#'
#' @param metadata data.frame with the metadata for a vein project for MOVES.
#' @param lkm Length of each link in miles
#' @param ef emission factors from EmissionRates_running exported from MOVES
#' @param fuel_type Data.frame of fuelSubtypeID exported by MOVES.
#' @param profile Data.frame or Matrix with nrows equal to 24 and ncol 7 day of
#' the week
#' @param agemax Integer; max age for the fleet, assuming the same for all vehicles.
#' @param net Road network class sf
#' @param simplify Logical, to return the whole object or processed by streets and veh
#' @param verbose Logical; To show more information. Not implemented yet
#' @param colk Character identifying a column in 'metadata' to multiply the emission factor
#' @param colkt Logical, TRUE if `colk` is used
#' @return a list with emissions at each street and data.base aggregated by categories.
#' @export
#' @importFrom data.table rbindlist as.data.table data.table dcast.data.table melt.data.table
#' @note The idea is the user enter with emissions factors by pollutant
#' @examples {
#' data(decoder)
#' decoder
#' }
moves_rpsy_meta <- function(metadata,
                            lkm,
                            ef,
                            fuel_type,
                            profile,
                            agemax = 31,
                            net,
                            simplify = TRUE,
                            verbose = FALSE,
                            colk,
                            colkt = F){

  profile$Hour <- sourceTypeID <- fuelTypeID <- pollutantID <- processID <- NULL

  `:` <- NULL

  age_total <- lxstart <- NULL

  # 3 fi;ter by sourceTypeID
  uni_source <- unique(ef$sourceTypeID)

  data.table::rbindlist(lapply(seq_along(uni_source), function(m) {

    if(verbose) cat(paste0("sourceTypeID: ", uni_source[m], "\n"))

    def <- ef[sourceTypeID == uni_source[m]]


    # 2 filter by fuelTypeID k
    uni_fuel <- unique(def$fuelTypeID)

    data.table::rbindlist(lapply(seq_along(uni_fuel), function(k) {

      if(verbose) cat(paste0("Fuel: ",  uni_fuel[k], "\n"))

      def <- def[fuelTypeID == uni_fuel[k]]

      # read vehicles
      nveh <- metadata[fuelTypeID == uni_fuel[k] & sourceTypeID == uni_source[m]]$vehicles

      if(verbose) cat(paste0("Reading: veh/", nveh, ".rds\n\n"))

      veh <- readRDS(paste0("veh/", nveh, ".rds"))

      # 3 filter by process
      uni_process <- unique(def$processID)

      data.table::rbindlist(lapply(seq_along(uni_process), function(l) {

        if(verbose) cat(paste0("Process: ", uni_process[l], "\n"))

        def <- def[processID == uni_process[l]]

        # 1 filter by pollutantID j
        uni_pol <- unique(ef$pollutantID)

        data.table::rbindlist(lapply(seq_along(uni_pol), function(j) {

          if(verbose) cat(paste0("Pollutant: ", uni_pol[j], "\n"))

          def <- def[pollutantID == uni_pol[j]]

          # hours
          data.table::rbindlist(lapply(1:nrow(profile), function(i) {

            seq_horas <- rep(1:24, nrow(profile)/24)

            hourID <- processID <- pollutantID <- sourceTypeID <- fuelTypeID <- roadTypeID <- NULL


            # no EF between 10:18
            # assuming that there should not exist EF in that hours
            #  need to compare with EF ratepervehicle


            # if (any(uni_pol[j] == 79 &
            #    uni_fuel[k] %in% 2:3 &
            #    uni_process[l] == c(16, 2) &
            #    uni_source[m] %in% 61:62)) {
            #
            #   def <- def[hourID == 9, ]
            #   def$EF <- def$EF*0
            # } else {
            def <- def[hourID == seq_horas[i], ]

            if(nrow(def) == 0) return()
            # }


            data.table::setorderv(def,
                                  cols = "modelYearID",
                                  order = -1)



            modelYearID <- .N <- NULL

            def[, `:=`(modelYearID, 1:.N)]


            . <- NULL
            def_veh <- dcast.data.table(data = def,
                                        formula = . ~ modelYearID,
                                        value.var = "EF")

            names(def_veh)[2:ncol(def_veh)] <- paste0("age_", names(def_veh)[2:ncol(def_veh)])


            la <- replicate(n = length(lkm),
                            expr = def_veh[, 2:ncol(def_veh)],
                            simplify = F)
            EF <- data.table::rbindlist(la)




            if(colkt){
              # to convert starts (trips) to km (EF g/start * start/km => g/km)
              # in metadata k should be "km_trip"
              nk <- metadata[fuelTypeID == uni_fuel[k] & sourceTypeID == uni_source[m]][[colk]]
              EF <- EF/as.numeric(nk)
            }


            if (uni_pol[j] == 91) {

              vehicles <- NULL

              df_fuel <- data.table::as.data.table(fuel_type)[fuelTypeID == metadata[vehicles == nveh]$fuelTypeID,
                                                              lapply(.SD, mean, na.rm = T),
                                                              .SDcols = c("carbonContent",
                                                                          "humidityCorrectionCoeff",
                                                                          "energyContent",
                                                                          "fuelDensity"),
                                                              by = fuelTypeID]

              EF <- EF/1000 * 1/df_fuel$energyContent * 1/df_fuel$fuelDensity

              for (kk in 1:ncol(EF)) {
                EF[[kk]] <- units::set_units(EF[[kk]], "gallons/miles/veh")
              }
            } else {
              EF <- EmissionFactors(x = as.data.frame(EF),
                                    mass = "g",
                                    dist = "miles")

              for (kk in 1:ncol(EF)) {
                EF[[kk]] <- EF[[kk]] * units::set_units(1, "1/veh")
              }
            }

            # estimation ####

            if (ncol(EF) < ncol(veh)) {

              if (verbose) message("Number of columns of EF is lower than number of columns of veh. Fixing")

              dif_col <- ncol(veh) - ncol(EF)

              for (kk in (ncol(EF) + 1):(ncol(EF) + dif_col)) {
                EF[[kk]] <- EF[[ncol(EF)]]
              }
            }
            lx <- data.table::rbindlist(
              lapply(1:agemax,
                     function(n) {
                       data.table::data.table(emi = EF[[n]] * veh[[n]] *  lkm * profile[i, n],
                                              id = 1:length(lkm),
                                              age = n,
                                              hour = i)
                     }))

            emi <- data.table::dcast.data.table(lx,
                                                formula = id + hour ~ age,
                                                value.var = "emi")

            names(emi)[3:ncol(emi)] <- paste0("age_", names(emi)[3:ncol(emi)])

            emi$veh <-  metadata[fuelTypeID == uni_fuel[k] & sourceTypeID == uni_source[m]]$vehicles

            emi$veh_type <-  metadata[fuelTypeID == uni_fuel[k] & sourceTypeID == uni_source[m]]$name

            emi$fuel <-  metadata[fuelTypeID == uni_fuel[k] & sourceTypeID == uni_source[m]]$fuel

            emi$fuelTypeID <-  metadata[fuelTypeID == uni_fuel[k] & sourceTypeID == uni_source[m]]$fuelTypeID

            emi$pollutantID <- uni_pol[j]

            emi$processID <- uni_process[l]

            emi$sourceTypeID <- uni_source[m]

            emi
          }))
        }))
      }))
    }))
  })) -> lxstart

  lxstart$age_total <- rowSums(lxstart[, paste0("age_", 1:agemax), with = F], na.rm = T)

  lxstart[, paste0("age_", 1:agemax):=NULL]

  if (!simplify) {
    message("The table has size ", format(object.size(lxstart), units = "Mb"))
    return(lxstart)
  } else {

    # To obtain NMHC by processID, fuelTypeID and sourceTypeID, !simplify must be used

    by_street <- lxstart[,
                         c("id", "hour", "age_total"),
                         with = F][,
                                   sum(age_total),
                                   by = c("id", "hour")]

    streets <- data.table::dcast.data.table(by_street,
                                            formula = id ~ hour,
                                            value.var = "V1")
    names(streets)[2:ncol(streets)] <- paste0("H", names(streets)[2:ncol(streets)])

    if (!missing(net)) {
      streets <- cbind(net, streets)
    }


    names(lxstart)

    by_veh <- lxstart[,
                      lapply(.SD, sum, na.rm = T),
                      .SDcols = paste0("age_", 1:agemax),
                      by = c("hour",
                             "veh",
                             "veh_type",
                             "fuel",
                             "fuelTypeID",
                             "pollutantID",
                             "processID",
                             "sourceTypeID")]

    veh <- data.table::melt.data.table(data = by_veh,
                                       id.vars = names(by_veh)[1:8],
                                       measure.vars = paste0("age_", 1:agemax))
    variable <- NULL

    veh[, `:=`(age, as.numeric(gsub("age_", "",  variable)))]

    rm(lxstart)
    invisible(gc())

    return(list(streets = streets, veh = veh))
  }

}
