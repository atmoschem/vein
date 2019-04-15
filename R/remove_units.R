#' Remove units
#'
#' \code{\link{remove_units}} Remove units from data.frames, matrix or units.
#'
#' @param x Object with class "data.frame", "matrix" or "units"
#' @return "data.frame", "matrix" or numeric
#' @keywords units
#' @importFrom units drop_units
#' @export
#' @examples {
#' ef1 <- ef_cetesb(p = "CO", c("PC_G", "PC_FE"))
#' class(ef1)
#' ef1
#' sapply(ef1, class)
#' a <- remove_units(ef1)
#' a
#' }
remove_units <- function(x){
  if ( is.matrix(x) ) {
    ef <- as.data.frame(x)
    for(i in 1:ncol(ef)){
      ef[,i] <- units::drop_units(ef[,i])
    }
    class(ef) <- "matrix"
    efx <- ef
  } else if ( is.data.frame(x) ) {
    ef <- x
    for(i in 1:ncol(ef)){
      ef[,i] <- units::drop_units(ef[,i])
    }
    class(ef) <- "data.frame"
  } else if( class(x) == "units") {
    ef <- units::drop_units(x)
  }
  return(ef)
}
