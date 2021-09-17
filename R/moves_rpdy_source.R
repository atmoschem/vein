#' MOVES estimation of using rates per distance by model year
#'
#' @description \code{\link{moves_rpdy_sf}} estimates running exhaust emissions
#' using MOVES emission factors.
#'
#' @param veh "Vehicles" data-frame or list of "Vehicles" data-frame. Each data-frame
#' as number of columns matching the age distribution of that ype of vehicle.
#' The number of rows is equal to the number of streets link.
#' @param lkm Length of each link in miles
#' @param ef emission factors from EmissionRates_running exported from MOVES
#' filtered by sourceTypeID and fuelTypeID.
#' @param speed_bin Data.frame or vector of avgSpeedBinID as defined by MOVES.
#' @param profile numeric vector  of normalized traffic for the morning rush hour
#' @param source_type_id Number to identify type of vehicle as defined by MOVES.
#' @param vehicle Character, type of vehicle
#' @param vehicle_type Character, subtype of vehicle
#' @param fuel_subtype Character, subtype of vehicle
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
moves_rpdy_sf <- function (veh,
                           lkm,
                           ef,
                           speed_bin,
                           profile,
                           source_type_id = 21,
                           vehicle = NULL,
                           vehicle_type = NULL,
                           fuel_subtype = NULL,
                           # net,
                           path_all,
                           verbose = FALSE) {


  ll <- if (is.data.frame(veh)) 1 else seq_along(veh)

  agemax <- ifelse(is.data.frame(veh), ncol(veh), ncol(veh[[1]]))

  data.table::rbindlist(lapply(1:ncol(speed_bin), function(i) {

    hourID <- processID <- pollutantID <- sourceTypeID <- fuelTypeID <- roadTypeID <- NULL

    # ensuring hours with length of any number of days
    seq_horas <- rep(1:24, ncol(speed_bin)/24)
    ef <- ef[hourID == seq_horas[i]]

    # k process ####
    pros <- unique(ef$processID)
    data.table::rbindlist(lapply(seq_along(pros), function(k) {
      ef <- ef[processID == pros[k]]

      # j pollutants ####
      pols <- unique(ef$pollutantID)
      data.table::rbindlist(lapply(seq_along(pols), function(j) {
        ef <- ef[pollutantID == pols[j]]



        data.table::setorderv(ef, cols = "modelYearID", order = -1)
        # checkif there aremissing avgSpeedBins!
        dfbin <- data.table::data.table(bin = 0:16)
        efbin <- data.table::data.table(bin = unique(ef$avgSpeedBinID))
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
        ef[, modelYearID :=1:.N, by = avgSpeedBinID]

        def_veh <- dcast.data.table(data = ef,
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

        dt <- data.table::data.table(avgSpeedBinID = speed_bin[[j]])

        df_net_ef <- merge(dt,
                           def_veh,
                           by = "avgSpeedBinID",
                           all.x = T)

        #EmissionFactors
        EF <- df_net_ef[,2:ncol(df_net_ef)]

        EF <- EmissionFactors(x = as.data.frame(EF), mass = "g", dist = "miles")

        for(kk in 1:ncol(EF)) {
          EF[[kk]] <- EF[[kk]]*units::set_units(1,"1/veh")
        }

        # estimating emissions ####
        if(ncol(EF) < ncol(veh)) {

          if(verbose)  message("Number of columns of EF is lower than number of columns of veh. Fixing")

          dif_col <- ncol(veh) - ncol(EF)
          for(kk in (ncol(EF) + 1):(ncol(EF) + dif_col)) {
            EF[[kk]] <- EF[[ncol(EF)]]
          }
        }

        lx <- data.table::rbindlist(lapply(1:agemax,
                                           function(ll) {
                                             data.table::data.table(
                                               emi = EF[[ll]] * veh[[ll]] * lkm * unlist(profile)[i],
                                               id = 1:nrow(df_net_ef),
                                               age = ll,
                                               hour = i)
                                           }))

        emi <- data.table::dcast.data.table(lx,
                                            formula = id + hour ~ age,
                                            value.var = "emi")
        names(emi)[3:ncol(emi)] <- paste0("age_", names(emi)[3:ncol(emi)])
        emi$veh <- vehicle
        emi$veh_type <- vehicle_type
        emi$fuelTypeID <- fuel_subtype
        emi$pollutantID <- pols[j]
        emi$processID <- pros[k]
        emi$sourceTypeID <- source_type_id
        emi
      }))
    }))
  })) -> lxspeed

  unique(lxspeed$hour)

  if (!missing(path_all)) {
    if (verbose)
      message("The table has size ", format(object.size(lxspeed),
                                            units = "Mb"))
    saveRDS(lxspeed, paste0(path_all, ".rds"))
  }
  # return(lxspeed)
  id <- hour <- . <- NULL

  by_street <- lxspeed[,
                       lapply(.SD, sum, na.rm = T),
                       .SDcols = paste0("age_", 1:agemax),
                       by = .(id,
                              pollutantID,
                              hour,
                              processID)
  ]

  by_street$age_total <- rowSums(by_street[, 4:(agemax + 3)])

  age_total <- NULL

  by_street2 <- by_street[,
                          sum(age_total, na.rm = T),
                          by = .(id,
                                 pollutantID,
                                 hour,
                                 processID)
  ]
  streets <- data.table::dcast.data.table(by_street2,
                                          formula = id + pollutantID + processID~ hour,
                                          value.var = "V1")

  names(streets)[4:ncol(streets)] <- paste0("H", names(streets)[4:ncol(streets)])

  # if (!missing(net)) {
  #   streets <- cbind(net, streets)
  # }

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
                                    sourceTypeID)]

  veh <- data.table::melt.data.table(data = by_veh,
                                     id.vars = names(by_veh)[1:7],
                                     measure.vars = paste0("age_", 1:agemax))
  variable <- NULL
  veh[, age := as.numeric(gsub("age_", "", variable))]

  rm(lxspeed)
  invisible(gc())
  streets$fuel <- fuel_subtype
  return(list(streets = streets, veh = veh))

}

