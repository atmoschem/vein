#' Construction function for class "Evaporative"
#'
#' @description \code{Evaporative} returns a transformed object with class "Evaporative" and
#' units g. This class represents the daily emissions presented by
#' Mellios G and Ntziachristos (2016) Gasoline evaporation, Tier 2. Eventually
#' it will be incorporated the techniques of Tier 3.
#'
#'
#' @return Objects of class "Evaporative" or "units"
#'
#' @param x Object with class "numeric"
#' @param object Object with class "Evaporative"
#' @param ... ignored
#' @aliases Evaporative print.Evaporative summary.Evaporative
#' plot.Evaporative
#' @name vein-deprecated
#' @seealso \code{\link{vein-deprecated}}
#' @keywords internal
NULL

#' @rdname vein-deprecated
#' @section \code{Evaporative}:
#' For \code{Evaporative}, use \code{\link{emis_evap}}.
#' @export
#' @examples \dontrun{
#' # do not run
#' # DEPRECATED
#' }
Evaporative <- function(x, ...) {
  .Deprecated("Evaporative")
  "Evaporative"

}
