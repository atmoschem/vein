#' List of cold start emission factors of Light Duty Vehicles
#'
#' This function creates a list of functions of cold start emission factors
#' considering different euro emission standard to the elements of the list.
#'
#' @param df Deprecated and ignored; the number of functions is
#' \code{length(eu)}. Before vein 2.0.0 it was \code{nrow(df)}, which gave
#' NA functions when it did not match the number of euro standards.
#' @param v Category vehicle: "LDV"
#' @param ta ambient temperature. Montly average van be used
#' @param cc Size of engine in cc: <=1400", "1400_2000" and ">2000"
#' @param f Type of fuel: "G" or "D"
#' @param eu character vector of euro standards: "PRE", "I", "II", "III", "IV",
#' "V", "VI" or "VIc".
#' @param p Pollutant: "CO", "FC", "NOx", "HC" or "PM"
#' @return A list of cold start emission factors  g/km
#' @keywords cold start emission factors
#' @note The length of the list should be equal to the name of the age categories of
#' a specific type of vehicle
#' @export
#' @examples \dontrun{
#' # Do not run
#' df <- data.frame(age1 = c(1,1),
#'                  age2 = c(2,2))
#' eu = c("I", "PRE")
#' l <- ef_ldv_cold(t = 17, cc = "<=1400", f = "G",
#' eu = "I", p = "CO")
#' l_cold <- ef_ldv_cold_list(df, t = 17, cc = "<=1400", f = "G",
#' eu = eu, p = "CO")
#' length(l_cold)
#' }
ef_ldv_cold_list <- function(df, v = "LDV", ta, cc, f, eu, p) {
  if (!missing(df) && NROW(df) != length(eu) && NCOL(df) != length(eu)) {
    warning("'df' is ignored; the list has one function per element of 'eu'")
  }
  lista <- lapply(seq_along(eu), function(i) {
    ef_ldv_cold(v = v, ta = ta, cc = cc, f = f,
      eu = as.character(eu[i]), p = p, k = 1,
      show.equation = FALSE)
  })
  return(lista)
}
