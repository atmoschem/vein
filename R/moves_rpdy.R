#' MOVES estimation of using rates per distance by model year
#'
#' @description \code{\link{moves_rpdy}} estimates running exhaust emissions
#' using MOVES emission factors.
#'
#' @param veh "Vehicles" data-frame or list of "Vehicles" data-frame. Each data-frame
#' as number of columns matching the age distribution of that ype of vehicle.
#' The number of rows is equal to the number of streets link.
#' @param lkm Length of each link in miles
#' @param ef emission factors from EmissionRates_running exported from MOVES
#' @param source_type_id Number to identify type of vehicle as defined by MOVES.
#' @param fuel_type_id Number to identify type of fuel as defined by MOVES.
#' @param pollutant_id  Number to identify type of pollutant as defined by MOVES.
#' @param road_type_id  Number to identify type of road as defined by MOVES.
#' @param fuel_type Data.frame of fuelSubtypeID exported by MOVES.
#' @param speed_bin Data.frame or vector of avgSpeedBinID as defined by MOVES.
#' @param profile Data.frame or Matrix with nrows equal to 24 and ncol 7 day of
#' the week
#' @param vehicle Character, type of vehicle
#' @param vehicle_type Character, subtype of vehicle
#' @param fuel_subtype Character, subtype of vehicle
#' @param process_id Character, processID
#' @param net Road network class sf
#' @param path_all Character to export whole estimation. It is not recommended since it
#' is usually too heavy.
#' @param verbose Logical; To show more information. Not implemented yet
#' @return a list with emissions at each street and data.base aggregated by categories. See \code{link{emis_post}}
#' @export
#' @importFrom data.table rbindlist as.data.table data.table dcast.data.table melt.data.table
#' @note `decoder` shows a decoder for MOVES
#' @examples {
#' data(decoder)
#' decoder
#' }
moves_rpdy <- function (veh,
                        lkm,
                        ef,
                        source_type_id = 21,
                        fuel_type_id = 1,
                        pollutant_id = 91,
                        road_type_id = 5,
                        fuel_type,
                        speed_bin,
                        profile,
                        vehicle,
                        vehicle_type,
                        fuel_subtype,
                        process_id,
                        net,
                        path_all,
                        verbose = FALSE) {

  profile$Hour <- NULL

  ll <- if (is.data.frame(veh)) 1 else seq_along(veh)

  agemax <- ifelse(is.data.frame(veh), ncol(veh), ncol(veh[[1]]))

  lxspeed <- data.table::rbindlist(lapply(ll, function(i) {

    if(verbose) cat("Estimating emissions of veh", i, " sourceTypeID",
                    source_type_id[i], "and fuelTypeID",  fuel_type_id[i], "\n")

    data.table::rbindlist(lapply(1:ncol(speed_bin), function(j) {
      seq_horas <- rep(1:24, ncol(speed_bin)/24)

      hourID <- processID <- pollutantID <- sourceTypeID <- fuelTypeID <- roadTypeID <- NULL

      def <- ef[hourID == seq_horas[j] &
                  pollutantID == pollutant_id &
                  sourceTypeID == source_type_id[i] &
                  fuelTypeID == fuel_type_id[i],]

      data.table::setorderv(def, cols = "modelYearID", order = -1)

      # checkif there aremissing avgSpeedBins!
      dfbin <- data.table::data.table(bin = 0:16)
      efbin <- data.table::data.table(bin = unique(def$avgSpeedBinID))
      efbin$id <- 1:nrow(efbin)
      dfbin <- merge(dfbin, efbin, by = "bin", all.x = T)

      if(sum(is.na(dfbin$id)) > 0){
        fill_bins <- dfbin[!is.na(dfbin$id), ]$bin[1]
        missing_bins <- dfbin[is.na(dfbin$id), ]$bin
        if(verbose) warning("Detected missing bins in ef: ", paste(missing_bins, collapse = " "),
                            "\nAssuming: ", fill_bins)
      }

      # here we rename modelYearID by age of use
      modelYearID <- avgSpeedBinID <- .N <- NULL
      def[, modelYearID :=1:.N, by = avgSpeedBinID]

      def_veh <- dcast.data.table(data = def,
                                  formula = avgSpeedBinID ~ modelYearID,
                                  value.var = "EF")

      names(def_veh)[2:ncol(def_veh)] <- paste0("age_", names(def_veh)[2:ncol(def_veh)])

      if(sum(is.na(dfbin$id)) > 0){
        la <- replicate(n = length(missing_bins),
                        expr = def_veh[avgSpeedBinID == fill_bins],
                        simplify = F)
        la <- data.table::rbindlist(la)
        la$avgSpeedBinID <- missing_bins
        def_veh <- rbind(def_veh, la)
        data.table::setorderv(def_veh, cols = "avgSpeedBinID")
      }


      df_fuel <- data.table::as.data.table(fuel_type)[fuelTypeID == fuel_type_id[i],
                                                      lapply(.SD, mean, na.rm = T),
                                                      .SDcols = c("carbonContent",
                                                                  "humidityCorrectionCoeff",
                                                                  "energyContent",
                                                                  "fuelDensity"),
                                                      by = fuelTypeID]
      # def <- merge(def, df_fuel, by = "fuelTypeID")


      dt <- data.table::data.table(avgSpeedBinID = speed_bin[[j]])

      df_net_ef <- merge(dt,
                         def_veh,
                         by = "avgSpeedBinID",
                         all.x = T)

      #EmissionFactors

      EF <- df_net_ef[,2:ncol(df_net_ef)]

      if (pollutant_id == 91) {
        EF <- EF/1000 * 1/df_fuel$energyContent * 1/df_fuel$fuelDensity
        for(kk in 1:ncol(EF)) {
          EF[[kk]] <- units::set_units(EF[[kk]],"gallons/miles/veh")
        }

      } else {
        EF <- EmissionFactors(x = as.data.frame(EF), mass = "g", dist = "miles")

        for(kk in 1:ncol(EF)) {
          EF[[kk]] <- EF[[kk]]*units::set_units(1,"1/veh")
        }
      }

      if (is.data.frame(veh)) {

        if(ncol(EF) < ncol(veh)) {
          if(verbose)  message("Number of columns of EF is lower than number of columns of veh. Fixing")
          dif_col <- ncol(veh) - ncol(EF)
          for(kk in (ncol(EF) + 1):(ncol(EF) + dif_col)) {
            EF[[kk]] <- EF[[ncol(EF)]]
          }
        }

        lx <- data.table::rbindlist(lapply(1:agemax,
                                           function(k) {
                                             data.table::data.table(
                                               emi = EF[[k]] * veh[[k]] * lkm * profile[j, i],
                                               id = 1:nrow(df_net_ef),
                                               age = k,
                                               hour = j)
                                           }))
      } else if (is.list(veh)) {

        if(ncol(EF) < ncol(veh[[i]])) {
          if(verbose) message("Number of columns of EF is lower than number of columns of veh. Fixing")
          dif_col <- ncol(veh[[i]]) - ncol(EF)

          for(kk in (ncol(EF) + 1):(ncol(EF) + dif_col)) {
            EF[[kk]] <- EF[[ncol(EF)]]
          }
        }

        lx <- data.table::rbindlist(lapply(1:agemax,
                                           function(k) {
                                             data.table::data.table(
                                               emi = EF[[k]] * veh[[i]][[k]] * lkm * profile[j, i],
                                               id = 1:nrow(df_net_ef),
                                               age = k,
                                               hour = j)
                                           }))
      }
      emi <- data.table::dcast.data.table(lx,
                                          formula = id + hour ~ age,
                                          value.var = "emi")
      names(emi)[3:ncol(emi)] <- paste0("age_", names(emi)[3:ncol(emi)])
      emi$veh <- vehicle[i]
      emi$veh_type <- vehicle_type[i]
      emi$fuelTypeID <- fuel_subtype[i]
      emi$pollutantID <- pollutant_id

      CategoryField <- Description <- NULL

      emi$processID <- process_id
      emi$sourceTypeID <- source_type_id[i]
      emi
    }))
  }))
  if (!missing(path_all)) {
    if (verbose)
      message("The table has size ", format(object.size(lxspeed),
                                            units = "Mb"))
    saveRDS(lxspeed, paste0(path_all, ".rds"))
  }

  id <- hour <- . <- NULL

  by_street <- lxspeed[,
                       lapply(.SD, sum, na.rm = T),
                       .SDcols = paste0("age_", 1:agemax),
                       by = .(id, hour)]

  by_street$age_total <- rowSums(by_street[, 3:(agemax + 2)])

  age_total <- NULL

  by_street2 <- by_street[,
                          sum(age_total, na.rm = T),
                          by = .(id, hour)]

  streets <- data.table::dcast.data.table(by_street2,
                                          formula = id ~ hour,
                                          value.var = "V1")

  names(streets)[2:ncol(streets)] <- paste0("H", names(streets)[2:ncol(streets)])

  if (!missing(net)) {
    streets <- cbind(net, streets)
  }

  names(lxspeed)

  veh <- veh_type <- fuelTypeID <- pollutantID <- processID <- sourceTypeID <- NULL

  by_veh <- lxspeed[, -"id"][,
                             lapply(.SD, sum, na.rm = T),
                             .SDcols = 2:32,
                             by = .(hour,
                                    veh,
                                    veh_type,
                                    fuelTypeID,
                                    pollutantID,
                                    processID,
                                    sourceTypeID)
  ]

  veh <- data.table::melt.data.table(data = by_veh,
                                     id.vars = names(by_veh)[1:7],
                                     measure.vars = paste0("age_", 1:agemax))
  variable <- NULL
  veh[, age := as.numeric(gsub("age_", "", variable))]

  rm(lxspeed)
  invisible(gc())
  return(list(streets = streets, veh = veh))
}

