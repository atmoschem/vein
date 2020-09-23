#' Check the number of threads
#'
#' @description \code{check_nt} check the number of threads in this machine
#'
#' @return Integer with the maxnumber of threads
#'
#' @examples {
#' check_nt()
#' }
#' @export
check_nt <- function() {
.Fortran("checkntf",
         nt = integer(1L))$nt
}
