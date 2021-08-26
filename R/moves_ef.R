#' MOVES emission factors
#'
#' @description \code{\link{moves_ef}} reads and filter MOVES
#' data.frame of emission factors.
#'
#' @param ef emission factors from EmissionRates_running exported from MOVES
#' @param vehicles Name of category, with length equal to fuel_type_id and other with id
#' @param process_id Number to identify emission process defined by MOVES.
#' @param source_type_id Number to identify type of vehicle as defined by MOVES.
#' @param fuel_type_id Number to identify type of fuel as defined by MOVES.
#' @param pollutant_id  Number to identify type of pollutant as defined by MOVES.
#' @param road_type_id  Number to identify type of road as defined by MOVES.
#' @param speed_bin Data.frame or vector of avgSpeedBinID as defined by MOVES.
#' @return EmissionFactors data.frame
#' @export
#' @importFrom data.table rbindlist as.data.table data.table dcast.data.table melt.data.table setDT
#' @note `decoder` shows a decoder for MOVES to identify
#' @examples {
#' data(decoder)
#' decoder
#' }
moves_ef <- function(ef,
                     vehicles,
                     source_type_id = 21, #Passenger car
                     process_id = 1,
                     fuel_type_id = 1, # Gasoline
                     pollutant_id = 2, # Total Energy Consumption
                     road_type_id = 5, # Urban Unrestricted Access
                     speed_bin) {

  data.table::rbindlist(
    lapply(seq_along(source_type_id), function(i){
      data.table::rbindlist(
        lapply(1:ncol(speed_bin), function(j){

          hourID <- processID <- pollutantID <- sourceTypeID <- fuelTypeID <- roadTypeID <- NULL
          def <- ef[hourID == j &
                      pollutantID == pollutant_id &
                      processID == process_id &
                      sourceTypeID == source_type_id[i] &
                      fuelTypeID == fuel_type_id[i] &
                      roadTypeID == road_type_id, ]

          dt <- data.table::data.table(id =1:nrow(speed_bin),
                                       sourceTypeID = source_type_id[i],
                                       avgSpeedBinID = speed_bin[[j]])

          df_net_ef <- merge(dt,
                             def[, c("avgSpeedBinID",
                                     "ratePerDistance")],
                             by = "avgSpeedBinID",
                             all.x = T)
          df_net_ef$hour <- j
          df_net_ef$vehicles <- vehicles[i]
          df_net_ef
        }))
    })
  ) -> lxspeed

  data.table::setorderv(lxspeed, cols = "id")

  data.table::dcast.data.table(lxspeed[, c("id", "hour", "ratePerDistance", "vehicles")],
                               formula = id+vehicles~hour,
                               value.var = "ratePerDistance") -> emi

  return(emi)
}

