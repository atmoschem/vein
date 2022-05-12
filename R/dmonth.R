#' Number of days of the month
#'
#' \code{\link{ef_ldv_speed}} return the number of day sof the month
#'
#'
#' @param year Numeric
#' @param month Numeric
#' @return days of the month
#' @family helpers
#' @export
#' @examples \dontrun{
#' dmonth(2022, 1)
#' }
dmonth <- function(year, month) {
  date <- ISOdate(year, month, 1, 0, 0, 0)
  m <- format(date, format="%m")

  while (format(date, format="%m") == m) {
    date <- date + 1*3600
  }
  return(as.integer(format(date - 1, format="%d")))
}
