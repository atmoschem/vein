#' Average daily traffic (ADT) from hourly traffic data.
#'
#' @description \code{\link{adt}} calculates ADT based on hourly traffic data. The
#' input traffic data is usually for morning rush hours.
#'
#' @param pc numeric vector for passenger cars
#' @param lcv numeric vector for light commercial vehicles
#' @param hgv numeric vector for heavy good vehicles or trucks
#' @param bus numeric vector for bus
#' @param mc numeric vector for motorcycles
#' @param p_pc data-frame profile for passenger cars, 24 hours only.
#' @param p_lcv data-frame profile for light commercial vehicles, 24 hours only.
#' @param p_hgv data-frame profile for heavy good vehicles or trucks, 24 hours only.
#' @param p_bus data-frame profile for bus, 24 hours only.
#' @param p_mc data-frame profile for motorcycles, 24 hours only.
#' @param expanded boolean argument for returning numeric vector or "Vehicles"
#' @return numeric vector of total volume of traffic per link, or data-frames
#' of expanded traffic
#' @importFrom units as_units
#' @export
#' @examples {
#' }
emis_chem <- function(pc, lcv, hgv, bus, mc,
                p_pc, p_lcv, p_hgv, p_bus, p_mc,
                expanded = FALSE) {
  if(length(unlist(p_pc)) > 24 |
     length(unlist(p_lcv)) > 24 |
     length(unlist(p_hgv)) > 24 |
     length(unlist(p_bus)) > 24 |
     length(unlist(p_mc)) > 24){
    stop("Profiles must be for 24 hours only")
  }
  df_pc <- vein::temp_fact(q = pc, pro = p_pc)
  df_lcv <- vein::temp_fact(q = lcv, pro = p_lcv)
  df_hgv <- vein::temp_fact(q = hgv, pro = p_hgv)
  df_bus <- vein::temp_fact(q = bus, pro = p_bus)
  df_mc <- vein::temp_fact(q = mc, pro = p_mc)
  if (expanded == FALSE) {
    df <- rowSums(df_pc) + rowSums(df_lcv) + rowSums(df_hgv) +
      rowSums(df_bus) + rowSums(df_mc)
    return(df*units::as_units("d-1"))
  } else{
    df <- df_pc + df_lcv + df_hgv + df_bus + df_mc
    for (i  in 1:ncol(df) ) {
      df[, i] <- as.numeric(df[, i])
    }
    return(Vehicles(df))
  }
}
