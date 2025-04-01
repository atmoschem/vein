#' Number of days of the month
#'
#' \code{\link{ef_ldv_speed}} return the number of days of the month
#'
#'
#' @param year Numeric
#' @param month Numeric
#' @return days of the month
#' @family helpers
#' @export
#' @examples \dontrun{
#' dmonth(2022, 1)
#' dmonth(Sys.Date())
#' }
dmonth <- function(year, month) {

  if(inherits(year, "Date")) {
    date <- year
  } else {
    date <- as.Date(ISOdate(year, month, 1, 0, 0, 0))

  }

  datef <- as.Date(unlist(lapply(seq_along(date), function(i) {
    seq.Date(date[i],
             length.out = 2,
             by = "1 months")[2]
  })),
  origin = "1970-01-01")

  ddif <- datef - date

    return(as.integer(ddif))
}
