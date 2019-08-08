#' Estimation of resuspension emissions from paved roads
#'
#' @description \code{emis_paved} estimates vehicular emissions from paved roads.
#' The vehicular emissions are estimated as the product of the vehicles on a
#' road, length of the road, emission factor from AP42 13.2.1 Paved roads.
#' It is assumed dry hours and anual aggregation should consider moisture factor.
#' It depends on Average Daily Traffic (ADT)
#'
#' @param veh Numeric vector with length of elements equals to number of streets
#' It is an array with dimenssions number of streets x hours of day x days of week
#' @param lkm Length of each link
#' @param k K_PM30 = 3.23 (g/vkm), K_PM15 = 0.77 (g/vkm), K_PM10 = 0.62 (g/vkm)
#' and K_PM2.5 = 0.15  (g/vkm).
#' @param sL1 Silt loading (g/m2) for roads with ADT <= 500
#' @param sL2 Silt loading (g/m2) for roads with ADT > 500 and <= 5000
#' @param sL3 Silt loading (g/m2) for roads with ADT > 5000 and <= 1000
#' @param sL4 Silt loading (g/m2) for roads with ADT > 10000
#' @param W array of dimensions of veh. It consists in the hourly averaged
#' weight of traffic fleet in each road
#' @return emission estimation  g/h
#' @export
#' @references EPA, 2016. Emission factor documentation for AP-42. Section
#' 13.2.1, Paved Roads. https://www3.epa.gov/ttn/chief/ap42/ch13/final/c13s0201.pdf
#' @examples {
#' # Do not run
#' veh <- array(pnorm(q=c(1:100), mean=500, sd = 100),
#'              c(100,24,7))
#' W <- veh*1e+05
#' lkm <-  rnorm(n = 100, mean = 10, sd = 1)
#' sL1 <- 0.6
#' emi  <- emis_paved(veh = veh, lkm = lkm, k = 0.65,
#'                        sL1 = sL1, sL2 = sL1/4, sL3 = sL1/16, sL4 = sL1/32,
#'                        W = W)
#' class(emi)
#' head(emi)
#' }
emis_paved <- function(veh,
                       lkm,
                       k = 0.62,   # K_PM10 = 0.62 (g/vkm)
                       sL1 = 0.6,  # g/m^2
                       sL2 = 0.2,  # g/m^2
                       sL3 = 0.06, # g/m^2
                       sL4 = 0.03, # g/m^2
                       W) {
  sL <- array(data = ifelse(veh <= 500, sL1,
                            ifelse(veh > 500 & veh <= 5000, sL2,
                                   ifelse(veh > 5000 & veh <=1000, sL3,
                                          sL4))),
              dim = dim(veh))
  lkm <- array(lkm, dim = dim(veh))
  k <- array(k, dim = dim(veh))
  emi <- veh * lkm * k * sL^0.91 * W^1.02
  emi[is.na(emi)] <- 0
  if(length(dim(emi)) == 2){
    names(emi) <- paste0("V",1:ncol(emi))
    return(Emissions(emi))
  } else {
    return(EmissionsArray(emi))
  }
}
