#' Check the max number of threads
#'
#' @description \code{get_threads} check the number of threads in this machine
#'
#' @return Integer with the max number of threads
#'
#' @useDynLib vein
#' @export
#' @examples
#' {
#'   check_nt()
#' }
check_nt <- function() {
  .Fortran("checkntf",
           nt = integer(1L)
  )$nt
}
