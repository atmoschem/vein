#' Fraction of mileage driven with a cold engine or catalizer below normal temperature
#'
#' This function depends length of trip and on ambient temperature.
#' From the guidelines  EMEP/EEA air pollutant emission inventory guidebook
#' http://www.eea.europa.eu/themes/air/emep-eea-air-pollutant-emission-inventory-guidebook
#'
#' @param ltrip Numeric; Length of trip. It must be in 'units' km.
#' @param ta Numeric or data.frame; average monthly temperature Celsius. It if is a
#' data.frame, it is convenient that each column is each month.
#' @keywords cold mileage
#' @note This function is set so that values vaires between 0 and 1.
#' @export
#' @examples \dontrun{
#' lkm <- units::set_units(1:10, km)
#' ta <- celsius(matrix(0:9, ncol = 12, nrow = 10))
#' a <- cold_mileage(lkm, rbind(ta, ta))
#' (a)
#' filled.contour(as.matrix(a), col = cptcity::lucky(n = 16))
#' }
cold_mileage <- function(ltrip, ta){
  # Check units
  if(class(ltrip) != "units"){
    stop("ltrip neeeds to has class 'units' in 'km'. Please, check package '?units::set_units'")
  }
  if(units(ltrip)$numerator == "m" ){
    stop("Units of lkm is 'm'. Please, check package '?units::set_units'")
  }
  if(units(ltrip)$numerator == "km" ){
    ltrip <- as.numeric(ltrip)
  }

  if(is.data.frame(ta) | is.matrix(ta)){
    if(class(ta[, 1]) != "units") stop("ta must be units in celsius, use units::set_units(ta, degC)")
    ta <- as.data.frame(ta)
    for(i in 1:ncol(ta)) {
      ta[, i] <- as.numeric(ta[, i])
    }
    clkm <- as.data.frame(
      sapply(
        1:ncol(ta),
        function(i){
          ifelse(
            0.6474 - 0.02545 * ltrip - (0.00974 - 0.000385 * ltrip) * ta[, i] < 0, 0,
            ifelse(
              0.6474 - 0.02545 * ltrip - (0.00974 - 0.000385 * ltrip) * ta[, i] > 1, 1,
              0.6474 - 0.02545 * ltrip - (0.00974 - 0.000385 * ltrip) * ta[, i]))
        }
      )
    )
  } else {
    if(class(ta) != "units") stop("ta must be units in celsius, use units::set_units(ta, degC)")
    ta <- as.numeric(ta)

    clkm <- ifelse(
      0.6474 - 0.02545 * ltrip - (0.00974 - 0.000385 * ltrip) * ta < 0, 0,
      ifelse(
        0.6474 - 0.02545 * ltrip - (0.00974 - 0.000385 * ltrip) * ta > 1, 1,
        0.6474 - 0.02545 * ltrip - (0.00974 - 0.000385 * ltrip) * ta))

  }


return(clkm)
}
