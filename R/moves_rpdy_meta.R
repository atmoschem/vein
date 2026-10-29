#' MOVES estimation of using rates per distance by model year
#'
#' @description \code{\link{moves_rpdy_meta}} estimates running exhaust emissions
#' using MOVES emission factors.
#'
#' @param metadata data.frame with the metadata for a vein project for MOVES.
#' @param lkm Length of each link in miles
#' @param ef emission factors from EmissionRates_running exported from MOVES
#' @param fuel_type Data.frame of fuelSubtypeID exported by MOVES.
#' @param speed_bin Data.frame or vector of avgSpeedBinID as defined by MOVES.
#' @param profile Data.frame or Matrix with one row per hour (ncol(speed_bin)
#' rows) and one column per vehicle, named as metadata$vehicles. A column
#' "Hour" is ignored.
#' @param agemax Integer; max age for the fleet, assuming the same for all vehicles.
#' @param net Road network class sf
#' @param simplify Logical, to return the whole object or processed by streets and veh
#' @param verbose Logical; To show more information. Not implemented yet
#' @return a list with emissions at each street and data.base aggregated by categories.
#' @export
#' @importFrom data.table rbindlist as.data.table data.table dcast.data.table melt.data.table
#' @note The idea is the user enter with emissions factors by pollutant
#' @examples {
#' data(decoder)
#' decoder
#' }

moves_rpdy_meta <- function(metadata,
                            lkm,
                            ef,
                            fuel_type,
                            speed_bin,
                            profile,
                            agemax = 31,
                            net,
                            simplify = TRUE,
                            verbose = FALSE) {

  profile <- as.data.frame(profile)
  profile$Hour <- NULL

  `:` <- NULL

  age_total <- lxstart <- NULL

  data.table::rbindlist(lapply(1:ncol(speed_bin), function(i) {

    # hour of the day for any number of days
    hour_i <- (i - 1) %% 24 + 1

    hourID <- processID <- pollutantID <- sourceTypeID <- fuelTypeID <- roadTypeID <- NULL

    # 1 filter by pollutantID j
    uni_pol <- unique(ef$pollutantID)

    data.table::rbindlist(lapply(seq_along(uni_pol), function(j) {

      if (verbose) cat(paste0("Pollutant: ", uni_pol[j], "\n"))

      def <- ef[pollutantID == uni_pol[j]]

      # 2 filter by fuelTypeID k
      uni_fuel <- unique(def$fuelTypeID)

      data.table::rbindlist(lapply(seq_along(uni_fuel), function(k) {

        if (verbose) cat(paste0("Fuel: ", uni_fuel[k], "\n"))

        def <- def[fuelTypeID == uni_fuel[k]]

        # 3 filter by process l
        uni_process <- unique(def$processID)

        data.table::rbindlist(lapply(seq_along(uni_process), function(l) {

          if (verbose) cat(paste0("Process: ", uni_process[l], "\n"))

          def <- def[processID == uni_process[l]]

          # 3 fi;ter by sourceTypeID
          uni_source <- unique(def$sourceTypeID)

          data.table::rbindlist(lapply(seq_along(uni_source), function(m) {

            if (verbose) cat(paste0("sourceTypeID: ", uni_source[m], "\n"))

            def <- def[sourceTypeID == uni_source[m]]

            # 5) filtrar horas
            def <- def[hourID == hour_i, ]

            data.table::setorderv(def,
              cols = "modelYearID",
              order = -1)

            dfbin <- data.table::data.table(bin = 0:16)

            efbin <- data.table::data.table(bin = unique(def$avgSpeedBinID))

            efbin$id <- 1:nrow(efbin)

            dfbin <- merge(dfbin, efbin, by = "bin", all.x = T)

            if (sum(is.na(dfbin$id)) > 0) {

              fill_bins <- dfbin[!is.na(dfbin$id), ]$bin[1]

              missing_bins <- dfbin[is.na(dfbin$id), ]$bin

              if (verbose)
                message("Detected missing bins in ef: ",
                  paste(missing_bins, collapse = " "),
                  "\nAssuming: ", fill_bins)
            }

            modelYearID <- avgSpeedBinID <- .N <- NULL

            def[, `:=`(modelYearID, 1:.N), by = avgSpeedBinID]

            def_veh <- dcast.data.table(data = def,
              formula = avgSpeedBinID ~ modelYearID,
              value.var = "EF")

            names(def_veh)[2:ncol(def_veh)] <- paste0("age_", names(def_veh)[2:ncol(def_veh)])

            if (sum(is.na(dfbin$id)) > 0) {
              la <- replicate(n = length(missing_bins), expr = def_veh[avgSpeedBinID ==  fill_bins],
                simplify = F)

              la <- data.table::rbindlist(la)

              la$avgSpeedBinID <- missing_bins

              def_veh <- rbind(def_veh, la)

              data.table::setorderv(def_veh, cols = "avgSpeedBinID")
            }


            # speed bins of hour i; id keeps the order of the links
            dt <- data.table::data.table(id = seq_along(speed_bin[[i]]),
              avgSpeedBinID = speed_bin[[i]])

            df_net_ef <- merge(dt,
              def_veh,
              by = "avgSpeedBinID",
              all.x = T)
            df_net_ef <- df_net_ef[order(df_net_ef$id), ]
            df_net_ef$id <- NULL

            EF <- df_net_ef[, 2:ncol(df_net_ef)]

            # read vehicles

            nveh <- metadata[fuelTypeID == uni_fuel[k] & sourceTypeID == uni_source[m]]$vehicles

            if (verbose) cat(paste0("Reading: veh/", nveh, ".rds\n"))

            veh <- readRDS(paste0("veh/", nveh, ".rds"))


            if (uni_pol[j] == 91) {
              if (missing(fuel_type)) stop("Please, add `fuel_type` data.frame from MOVES")

              vehicles <- NULL

              df_fuel <- data.table::as.data.table(fuel_type)[fuelTypeID == metadata[vehicles == nveh]$fuelTypeID,
                lapply(.SD, mean, na.rm = T),
                .SDcols = c("carbonContent",
                  "humidityCorrectionCoeff",
                  "energyContent",
                  "fuelDensity"),
                by = fuelTypeID]

              EF <- EF / 1000 * 1 / df_fuel$energyContent * 1 / df_fuel$fuelDensity

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
                  data.table::data.table(emi = EF[[n]] * veh[[n]] *  lkm * profile[i, nveh],
                    id = 1:nrow(df_net_ef),
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
  })) -> lxspeed

  lxspeed$age_total <- rowSums(lxspeed[, paste0("age_", 1:agemax), with = F], na.rm = T)


  if (!simplify) {
    message("The table has size ", format(object.size(lxspeed), units = "Mb"))
    return(lxspeed)
  } else {

    # To obtain NMHC by processID, fuelTypeID and sourceTypeID, !simplify must be used

    by_street <- lxspeed[,
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


    names(lxspeed)

    by_veh <- lxspeed[,
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

    rm(lxspeed)
    invisible(gc())

    return(list(streets = streets, veh = veh))
  }

}
