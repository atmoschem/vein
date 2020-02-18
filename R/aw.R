#' Average Weight from hourly traffic data.
#'
#' @description \code{\link{aw}} average weight form traffic.
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
#' @param w_pc Numeric, factor equivalence
#' @param w_lcv Numeric, factor equivalence
#' @param w_hgv Numeric, factor equivalence
#' @param w_bus Numeric, factor equivalence
#' @param w_mc Numeric, factor equivalence
#' @param net SpatialLinesDataFrame or Spatial Feature of "LINESTRING"
#' @importFrom sf st_sf
#' @return data.frame with with average weight
#' @export
#' @examples \dontrun{
#' data(net)
#' data(pc_profile)
#' p1 <- pc_profile[, 1]
#' aw1 <- aw(pc = net$ldv*0.75,
#'             lcv = net$ldv*0.1,
#'             hgv = net$hdv,
#'             bus = net$hdv*0.1,
#'             mc = net$ldv*0.15,
#'             p_pc = p1,
#'             p_lcv = p1,
#'             p_hgv = p1,
#'             p_bus = p1,
#'             p_mc = p1)
#' head(aw1)
#' }
aw <- function(pc, lcv, hgv, bus, mc,
               p_pc, p_lcv, p_hgv, p_bus, p_mc,
               w_pc = 1,
               w_lcv = 3.5,
               w_hgv = 20,
               w_bus = 20,
               w_mc = 0.5,
               net) {
  pc <- remove_units(pc)
  lcv <- remove_units(lcv)
  hgv <- remove_units(hgv)
  bus <- remove_units(bus)
  mc <- remove_units(mc)

  df_pc <- vein::temp_fact(q = pc, pro = p_pc)
  df_lcv <- vein::temp_fact(q = lcv, pro = p_lcv)
  df_hgv <- vein::temp_fact(q = hgv, pro = p_hgv)
  df_bus <- vein::temp_fact(q = bus, pro = p_bus)
  df_mc <- vein::temp_fact(q = mc, pro = p_mc)

  fleet <-  df_pc + df_lcv + df_hgv + df_bus + df_mc

  df_pc_w <- vein::temp_fact(q = pc*w_pc, pro = p_pc)
  df_lcv_w <- vein::temp_fact(q = lcv*w_lcv, pro = p_lcv)
  df_hgv_w <- vein::temp_fact(q = hgv*w_hgv, pro = p_hgv)
  df_bus_w <- vein::temp_fact(q = bus*w_bus, pro = p_bus)
  df_mc_w <- vein::temp_fact(q = mc*w_mc, pro = p_mc)

  W <- (df_pc_w + df_lcv_w + df_hgv_w + df_bus_w + df_mc_w)/fleet
  W <- remove_units(W)
  W[is.na(W)] <- 0
  if(!missing(net)) {
    net <- sf::st_as_sf(net)
    W <- sf::st_sf(W, geometry = sf::st_geometry(net))
  } else {
    return(W)
  }
}
