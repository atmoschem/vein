#' Deprecated - EMFAC2017 emission factors for Statewide California, Calendar Year 2020
#'
#' \code{\link{ef_emfac}} returns emission factors reflecting California, US,
#' conditions. If the user enter speeds more emission factors are returned.
#' See details.
#'
#' @param veh Character; "one of the 40 vehicle categories shown below.
#' @param fuel Character; "Diesel", "Gasoline", "Electricity" or "Natural Gas"
#' @param mph Numeric; Speed in miles per hour (optional).
#' @param pol Character;
#' @param season Character: "winter" or "summer".
#' @param full Logical: To return the whole data.table or not.
#' @return data.table with emission factors.
#' @references https://arb.ca.gov/emfac/emissions-inventory
#' @name vein-deprecated
#' @seealso \code{\link{vein-deprecated}}
#' @keywords internal
NULL

#' @rdname vein-deprecated
#'
#' @export
#' @examples \dontrun{
#' # do not run
#' # DEPRECATED
#' }

ef_emfac <-  function(x,  ...) {
  .Deprecated("ef_emfac")
  "ef_emfac"
}
