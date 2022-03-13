#' Estimation of resuspension emissions from paved roads
#'
#' @description \code{emis_paved} estimates vehicular emissions from paved roads.
#' The vehicular emissions are estimated as the product of the vehicles on a
#' road, length of the road, emission factor from AP42 13.2.1 Paved roads.
#' It is assumed dry hours and annual aggregation should consider moisture factor.
#' It depends on Average Daily Traffic (ADT)
#'
#' @param veh Numeric vector with length of elements equals to number of streets
#' It is an array with dimenssions number of streets x hours of day x days of week
#' @param adt Numeric vector of with Average Daily Traffic (ADT)
#' @param lkm Length of each link
#' @param k K_PM30 = 3.23 (g/vkm), K_PM15 = 0.77 (g/vkm), K_PM10 = 0.62 (g/vkm)
#' and K_PM2.5 = 0.15  (g/vkm).
#' @param sL1 Silt loading (g/m2) for roads with ADT <= 500
#' @param sL2 Silt loading (g/m2) for roads with ADT > 500 and <= 5000
#' @param sL3 Silt loading (g/m2) for roads with ADT > 5000 and <= 1000
#' @param sL4 Silt loading (g/m2) for roads with ADT > 10000
#' @param W array of dimensions of veh. It consists in the hourly averaged
#' weight of traffic fleet in each road
#' @param net SpatialLinesDataFrame or Spatial Feature of "LINESTRING"
#' @return emission estimation  g/h
#' @export
#' @references EPA, 2016. Emission factor documentation for AP-42. Section
#' 13.2.1, Paved Roads. https://www3.epa.gov/ttn/chief/ap42/ch13/final/c13s0201.pdf
#'
#' CENMA Chile: Actualizacion de inventario de emisiones de contaminntes atmosfericos RM 2020
#' Universidad de Chile#'
#' @note silt values can vary a lot. For comparison:
#'
#' \tabular{lcc}{
#'   ADT         \tab US-EPA g/m2  \tab  CENMA (Chile) g/m2 \cr
#'   < 500       \tab 0.6           \tab  2.4   \cr
#'   500-5000    \tab 0.2           \tab  0.7   \cr
#'   5000-1000   \tab 0.06          \tab  0.6   \cr
#'   >10000      \tab  0.03         \tab  0.3   \cr
#' }
#' @examples \dontrun{
#' # Do not run
#' veh <- matrix(1000, nrow = 10,ncol = 10)
#' W <- veh*1.5
#' lkm <-  1:10
#' ADT <-1000:1010
#' emi  <- emis_paved(veh = veh, adt = ADT, lkm = lkm, k = 0.65, W = W)
#' class(emi)
#' head(emi)
#' }
emis_paved <- function(veh,         # hourly traffic flow multiplier of 24
                       adt,
                       lkm,
                       k = 0.62,   # K_PM10 = 0.62 (g/vkm)
                       sL1 = 0.6,  # g/m^2
                       sL2 = 0.2,  # g/m^2
                       sL3 = 0.06, # g/m^2
                       sL4 = 0.03, # g/m^2
                       W,
                       net = net) {

  veh <- remove_units(veh)
  adt <- remove_units(adt)
  lkm <- remove_units(lkm)
  W <- remove_units(W)
  veh$id <- NULL

  sL <- ifelse(adt <= 500, sL1,
               ifelse(adt > 500 & adt <= 5000, sL2,
                      ifelse(adt > 5000 & adt <=10000, sL3,
                             sL4)))
  emi <- veh * lkm * k * sL^0.91 * W^1.02

  emi[is.na(emi)] <- 0
  if(!missing(net)) {
    net <- sf::st_as_sf(net)
    emi <- sf::st_sf(Emissions(emi), geometry = sf::st_geometry(net))
    return(emi)
  } else {
    return(Emissions(emi))
  }
}
