#' Estimation of exhaust emissions using MOVES
#'
#' @description \code{\link{moves_exhaust}} estimates running exhaust emissions
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
#' @param path_all Character to export whole estimation. It is not recommended since it
#' is usually too heavy.
#' @param verbose Logical; To show more information. Not implemented yet
#' @return a list with emissions at each street and data.base aggregated by categories. See \code{link{emis_post}}
#' @export
#' @importFrom data.table rbindlist as.data.table data.table dcast.data.table melt.data.table setDT
#' @note `decoder` shows a decoder for MOVES to identify
#' @examples {
#' data(decoder)
#' decoder
#' }
moves_exhaust <- function(veh, #  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
                           lkm,
                           ef,
                           source_type_id = 21, #Passenger car
                           fuel_type_id = 1, # Gasoline
                           pollutant_id = 91, # Total Energy Consumption
                           road_type_id = 5, # Urban Unrestricted Access
                           fuel_type,  # data.frame from moves
                           speed_bin,
                           profile,
                           vehicle = NULL,
                           vehicle_type = NULL,
                           fuel_subtype = NULL,
                           path_all, #path to save RDS
                           verbose = FALSE) {

  dec <- sysdata$decoder
  data.table::setDT(dec)
  profile$Hour <- NULL

  ll <- if(is.data.frame(veh)) 1 else seq_along(veh)

  agemax <- ifelse(
    is.data.frame(veh),
    ncol(veh),
    ncol(veh[[1]])
    )

  data.table::rbindlist(
    lapply(ll, function(i){

      data.table::rbindlist(
        lapply(1:ncol(speed_bin), function(j){

          hourID <- processID <- pollutantID <- sourceTypeID <- fuelTypeID <- roadTypeID <- NULL
          def <- ef[hourID == j &
                      pollutantID == pollutant_id &
                      processID == 1 &
                      sourceTypeID == source_type_id[i] &
                      fuelTypeID == fuel_type_id[i] &
                      roadTypeID == road_type_id, ]

          data.table::as.data.table(fuel_type)[,
                                               lapply(.SD, mean, na.rm = T),
                                               .SDcols = c("carbonContent",
                                                           "humidityCorrectionCoeff",
                                                           "energyContent",
                                                           "fuelDensity"),
                                               by = fuelTypeID] -> df_fuel

          def <- merge(def, df_fuel, by = "fuelTypeID")

          dt <- data.table::data.table(sourceTypeID = source_type_id[i],
                                       avgSpeedBinID = speed_bin[[j]])

          df_net_ef <- merge(dt,
                             def[, c("avgSpeedBinID",
                                     "ratePerDistance",
                                     "carbonContent",
                                     "humidityCorrectionCoeff",
                                     "energyContent",
                                     "fuelDensity")],
                             by = "avgSpeedBinID",
                             all.x = T)

          if(pollutant_id == 91) {
            df_net_ef$ef <- df_net_ef$ratePerDistance*1/df_net_ef$energyContent*1/df_net_ef$fuelDensity
            df_net_ef$ef <- units::set_units(df_net_ef$ef, "gallons/miles/veh")
          } else {
            df_net_ef$ef <- df_net_ef$ratePerDistance
            df_net_ef$ef <- units::set_units(df_net_ef$ef, "g/miles/veh")
          }


          if(is.data.frame(veh)) {
            data.table::rbindlist(lapply(1:agemax, function(k){
              data.table::data.table(emi = df_net_ef$ef*veh[[k]]*lkm*profile[j, i],
                                     id = 1:nrow(df_net_ef),
                                     age = k,
                                     hour = j)
            }))-> lx

          } else if(is.list(veh)) {
            data.table::rbindlist(lapply(1:agemax, function(k){
              data.table::data.table(emi = df_net_ef$ef*veh[[i]] [[k]]*lkm*profile[j, i],
                                     id = 1:nrow(df_net_ef),
                                     age = k,
                                     hour = j)
            }))-> lx

          }

          data.table::dcast.data.table(lx, formula = id+hour~age, value.var = "emi") -> emi
          names(emi)[3:ncol(emi)] <- paste0("age_", names(emi)[3:ncol(emi)])
          emi$veh <- vehicle[i]
          emi$veh_type <- vehicle_type[i]
          emi$fuel <- fuel_subtype[i]
          emi$pollutant <- pollutant_id

          CategoryField <- Description <- NULL

          emi$type_emi <- dec[CategoryField == "processID" & pollutantID == 1, Description]
          emi$sourceTypeID <- source_type_id[i]
          emi
        })
      )
    })
  ) -> lxspeed

  if(!missing(path_all)){
    if(verbose) message("The table has size ", format(object.size(lxspeed), units = "Mb"))
    saveRDS(lxspeed, paste0(path_all, ".rds"))
  }

  # input agemax ####

  id <- hour <- . <- NULL
  by_street <- lxspeed[,
                       lapply(.SD, sum, na.rm = T),
                       .SDcols =  paste0("age_", 1:agemax),
                       by = .(id, hour)]

  by_street$age_total <- rowSums(by_street[,  3:(agemax + 2)])

  age_total <- NULL
  by_street2 <- by_street[,
                          sum(age_total, na.rm = T),
                          by = .(id, hour)]


  data.table::dcast.data.table(by_street2, formula = id~hour, value.var = "V1") -> streets

  names(lxspeed)
  veh <- veh_type <- fuel <- pollutant <- type_emi <-  sourceTypeID <- NULL
  by_veh <- lxspeed[, -"id"][,
                             lapply(.SD, sum, na.rm = T),
                             .SDcols = 2:32,
                             by = .(hour, veh, veh_type, fuel, pollutant, type_emi, sourceTypeID)]

  data.table::melt.data.table(data = by_veh,
                              id.vars = names(by_veh)[1:7],
                              measure.vars = paste0("age_", 1:agemax)) -> veh

    variable <- NULL
    veh[, age := as.numeric(gsub("age_", "", variable))]


  rm(lxspeed)
  invisible(gc())

  return(
    list(
      streets = streets,
      veh = veh
    )
  )

}

