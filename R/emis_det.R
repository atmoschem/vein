#' Determine deterioration factors for urban conditions
#'
#' @description \code{\link{emis_det}} returns deterioration factors. The emission
#' factors comes from the guidelines for developing emission factors of the
#' EMEP/EEA air pollutant emission inventory guidebook
#' http://www.eea.europa.eu/themes/air/emep-eea-air-pollutant-emission-inventory-guidebook
#' This function subset an internal database of emission factors with each argument
#'
#' @param po Character; Pollutant "CO", "NOx" or "HC"
#' @param cc Character; Size of engine in cc converin "<=1400", "1400_2000" or ">2000"
#' @param eu Character; Euro standard:  "I", "II", "III", "III", "IV", "V", "VI", "VIc"
#' @param km Numeric; accumulated mileage in km
#' @return It returns a numeric vector representing the increase in emissions due to normal deterioring
#' @keywords deterioration emission factors
#' @export
#' @examples {
#' data(fkm)
#' pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)
#' km <- units::set_units(pckma[1:11], km)
#' (cod1 <- emis_det(po = "CO", cc = "<=1400", eu = "III", km = km))
#' }
emis_det <- function(po, cc, eu, km) {
  if (po %in% c("PRE")) {
    stop("No deterioration factors for this standards, yet")
  }
  if (po %in% c("V", "VI", "VIc")) {
    message("Assuming the same deterioration as euro III and IV")
  }

  if(class(km) != "units"){
    stop("km neeeds to has class 'units' in 'km'. Please, check package 'units'")
  }
  km <- as.numeric(km)
  #Check cc
  if(is.numeric(cc)){
    cc <- ifelse(cc <= 1400, "<=1400",
                 ifelse(cc >= 1400 & cc < 2000,
                        "1400_2000", ">2000"
                 ))
  }

  if (po == "CO" & eu %in% c("I", "II") &  cc == "<=1400") {
    mc <- ifelse(km<120000, 1.523e-05*km+0.557, 2.39)
  } else if (po == "CO" & eu %in% c("I", "II") & cc == "1400_2000") {
    mc <- ifelse(km<120000, 1.148e-05*km+0.543, 1.92)
  } else if (po == "CO" & eu %in% c("I", "II") & cc == ">2000") {
    mc <- ifelse(km<120000, 9.243e-06*km+0.565, 1.67)
  } else if (po == "CO" & eu %in% c("III", "IV") & cc == "<=1400") {
    mc <- ifelse(km<160000, 7.129e-06*km+0.769, 1.91)
  } else if (po == "CO" & eu %in% c("III", "IV") & cc %in% c("1400_2000", ">2000")) {
    mc <- ifelse(km<160000, 2.670e-06*km+0.955, 1.38)

  } else if (po == "NOx" & eu %in% c("I", "II") & cc %in% c("<=1400" , "1400_2000", ">2000")) {
    mc <- ifelse(km<120000, 1.598e-05*km+0.282, 2.2)

  } else if (po == "NOx" &  eu %in% c("III", "IV") & cc == "<=1400") {
    mc <- ifelse(km<160000, 1, 1)
  } else if (po == "NOx" &  eu %in% c("III", "IV") & cc %in% c("1400_2000", ">2000")) {
    mc <- ifelse(km<160000, 3.986e-06*km+0.932, 1.57)

  } else if (po == "HC" & eu %in% c("I", "II") & cc == "<=1400") {
    mc <- ifelse(km<120000, 1.215e-05*km+0.647, 2.1)
  } else if (po == "HC" & eu %in% c("I", "II") & cc == "1400_2000") {
    mc <- ifelse(km<120000, 1.212e-05*km+0.509, 1.99)
  } else if (po == "HC" & eu %in% c("I", "II") & cc == ">2000") {
    mc <- ifelse(km<120000, 1.208e-05*km+0.432, 1.88)
  } else if (po == "HC" & eu %in% c("III", "IV") & cc ==  "<=1400") {
    mc <- ifelse(km<160000, 3.419e-06*km+0.891, 1.44)
  } else if (po == "HC" & eu %in% c("III", "IV") & cc %in% c("1400_2000", ">2000")) {
    mc <- ifelse(km<160000, 1, 1)
  }
  mc <- ifelse(mc < 1, 1, mc)
  return(mc)
}
