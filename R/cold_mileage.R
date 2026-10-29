#' Fraction of mileage driven with a cold engine or catalizer below normal temperature
#'
#' This function depends length of trip and on ambient temperature.
#' From the guidelines  EMEP/EEA air pollutant emission inventory guidebook
#' https://www.eea.europa.eu/en/analysis/publications/emep-eea-guidebook-2023
#'
#' @param ltrip Numeric; Length of trip. It must have class 'units' with a
#' length unit; it is converted to km.
#' @param ta Numeric or data.frame; average monthly temperature with class
#' 'units' (converted to Celsius, e.g. 'K' is accepted). It if is a
#' data.frame, it is convenient that each column is each month.
#' @keywords cold mileage
#' @note This function is set so that values varies between 0 and 1.
#' @export
#' @examples \dontrun{
#' lkm <- units::set_units(1:10, km)
#' ta <- celsius(matrix(0:9, ncol = 12, nrow = 10))
#' a <- cold_mileage(lkm, ta)
#' colplot(a)
#' }
cold_mileage <- function(ltrip, ta) {
  # Check units
  if (!inherits(ltrip, "units")) {
    stop("ltrip neeeds to has class 'units' in 'km'. Please, check package '?units::set_units'")
  }
  ltrip <- as.numeric(units::set_units(ltrip, "km", mode = "standard"))

  to_degC <- function(v) {
    if (!inherits(v, "units")) stop("ta must be units in celsius, use units::set_units(ta, degC)")
    as.numeric(units::set_units(v, "degC", mode = "standard"))
  }

  if (is.data.frame(ta) | is.matrix(ta)) {
    ta <- as.data.frame(ta)
    for (i in 1:ncol(ta)) {
      ta[, i] <- to_degC(ta[, i])
    }
    clkm <- as.data.frame(matrix(
      unlist(lapply(
        1:ncol(ta),
        function(i) {
          ifelse(
            0.6474 - 0.02545 * ltrip - (0.00974 - 0.000385 * ltrip) * ta[, i] < 0, 0,
            ifelse(
              0.6474 - 0.02545 * ltrip - (0.00974 - 0.000385 * ltrip) * ta[, i] > 1, 1,
              0.6474 - 0.02545 * ltrip - (0.00974 - 0.000385 * ltrip) * ta[, i]))
        }
      )),
      nrow = max(nrow(ta), length(ltrip)),
      ncol = ncol(ta)
    ))
  } else {
    ta <- to_degC(ta)

    clkm <- ifelse(
      0.6474 - 0.02545 * ltrip - (0.00974 - 0.000385 * ltrip) * ta < 0, 0,
      ifelse(
        0.6474 - 0.02545 * ltrip - (0.00974 - 0.000385 * ltrip) * ta > 1, 1,
        0.6474 - 0.02545 * ltrip - (0.00974 - 0.000385 * ltrip) * ta))

  }


  return(clkm)
}
