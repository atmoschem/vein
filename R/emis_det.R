#' Determine deterioration factors for urban conditions
#'
#' @description \code{emis_det} returns deterioration factors. The emission
#' factors comes from the guidelines for developing emission factors of the
#' EMEP/EEA air pollutant emission inventory guidebook
#' http://www.eea.europa.eu/themes/air/emep-eea-air-pollutant-emission-inventory-guidebook
#' This function subset an internal database of emission factors with each argument
#'
#' @param po Pollutant
#' @param cc Size of engine in cc
#' @param eu Euro standard: "PRE", "I", "II", "III", "III", "IV", "V", "VI"
#' @param km mileage in km
#' @return It returns a numeric vector without "units"
#' @keywords deterioration emission factors
#' @export
#' @examples \dontrun{
#' # Do not run
#' }
emis_det <- function(po, cc, eu, km) {
  if (po == "CO" & (eu == "I" | eu == "II") &  cc <= 1400) {
    mc <- ifelse(km<120000, 1.523e-05*km+0.557, 2.39)
  } else if (po == "CO" & (eu == "I" | eu == "II") & cc > 1400 & cc <= 2000) {
    mc <- ifelse(km<120000, 1.148e-05*km+0.543, 1.92)
  } else if (po == "CO" & (eu == "I" | eu == "II") & cc > 2000) {
    mc <- ifelse(km<120000, 9.243e-06*km+0.565, 1.67)

  } else if (po == "NOx" & (eu == "I" | eu == "II") & cc > 0) {
    mc <- ifelse(km<120000, 1.598e-05*km+0.282, 2.2)

  } else if (po == "HC" & (eu == "I" | eu == "II") & cc <= 1400) {
    mc <- ifelse(km<120000, 1.215e-05*km+0.647, 2.1)
  } else if (po == "HC" & (eu == "I" | eu == "II") & cc > 1400 & cc <= 2000) {
    mc <- ifelse(km<120000, 1.212e-05*km+0.509, 1.99)
  } else if (po == "HC" & (eu == "I" | eu == "II") & cc > 2000) {
    mc <- ifelse(km<120000, 1.208e-05*km+0.432, 1.88)

    } else if (po == "CO" & (eu == "III" | eu == "IV") & cc < 1400) {
    mc <- ifelse(km<160000, 7.129e-06*km+0.769, 1.91)
    } else if (po == "CO" & (eu == "III" | eu == "IV") & cc >= 1400) {
      mc <- ifelse(km<160000, 2.670e-06*km+0.955, 1.38)

    } else if (po == "NOx" & (eu == "III" | eu == "IV") & cc < 1400) {
      mc <- ifelse(km<160000, 1, 1)
    } else if (po == "NOx" & (eu == "III" | eu == "IV") & cc >= 1400) {
      mc <- ifelse(km<160000, 3.986e-06*km+0.932, 1.57)

    } else if (po == "HC" & (eu == "III" | eu == "IV") & cc < 1400) {
      mc <- ifelse(km<160000, 3.419e-06*km+0.891, 1.44)
    } else if (po == "HC" & (eu == "III" | eu == "IV") & cc >= 1400) {
      mc <- ifelse(km<160000, 1, 1)
    }
  mc <- ifelse(mc<1,1,mc)
  return(mc)
}
