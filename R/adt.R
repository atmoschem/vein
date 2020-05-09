#' Average daily traffic (ADT) from hourly traffic data.
#'
#' @description \code{\link{adt}} calculates ADT based on hourly traffic data.
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
#' @param feq_pc Numeric, factor equivalence
#' @param feq_lcv Numeric, factor equivalence
#' @param feq_hgv Numeric, factor equivalence
#' @param feq_bus Numeric, factor equivalence
#' @param feq_mc Numeric, factor equivalence
#' @return numeric vector of total volume of traffic per link as ADT
#' @export
#' @examples \dontrun{
#' data(net)
#' data(pc_profile)
#' p1 <- pc_profile[, 1]
#' adt1 <- adt(pc = net$ldv*0.75,
#'             lcv = net$ldv*0.1,
#'             hgv = net$hdv,
#'             bus = net$hdv*0.1,
#'             mc = net$ldv*0.15,
#'             p_pc = p1,
#'             p_lcv = p1,
#'             p_hgv = p1,
#'             p_bus = p1,
#'             p_mc = p1)
#' head(adt1)
#' }
adt <- function(pc, lcv, hgv, bus, mc,
                p_pc, p_lcv, p_hgv, p_bus, p_mc,
                feq_pc = 1,
                feq_lcv = 1.5,
                feq_hgv = 2,
                feq_bus = 2,
                feq_mc = 0.5) {
  pc <- remove_units(pc)
  lcv <- remove_units(lcv)
  hgv <- remove_units(hgv)
  bus <- remove_units(bus)
  mc <- remove_units(mc)

  df_pc <- vein::temp_fact(q = pc*feq_pc, pro = p_pc)
  df_lcv <- vein::temp_fact(q = lcv*feq_lcv, pro = p_lcv)
  df_hgv <- vein::temp_fact(q = hgv*feq_hgv, pro = p_hgv)
  df_bus <- vein::temp_fact(q = bus*feq_bus, pro = p_bus)
  df_mc <- vein::temp_fact(q = mc*feq_mc, pro = p_mc)

  veq = df_pc + df_lcv + df_hgv + df_bus + df_mc

  n <- ncol(veq)/24
  dx <- do.call("cbind", lapply(1:n, function(i){
    rowSums(veq[, (24*(i-1) + 1) : (24*(i-1) + 24)])
  }))

  ADT <- rowMeans(dx)
  return(Vehicles(ADT))
}
