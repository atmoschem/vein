#' Remove units
#'
#' \code{\link{remove_units}} Remove units from data.frames, matrix or units.
#'
#' @param x Object with class "data.frame", "matrix" or "units"
#' @return "data.frame", "matrix" or numeric
#' @keywords units
#' @export
#' @examples {
#' ef1 <- ef_cetesb(p = "CO", c("PC_G", "PC_FE"))
#' class(ef1)
#' sapply(ef1, class)
#' a <- remove_units(ef1)
#' }
remove_units <- function(x){
  if ( is.matrix(x) ) {
    ef <- as.data.frame(x)
    for(i in 1:ncol(ef)){
      ef[,i] <- as.numeric(ef[,i])
    }
  } else if ( is.data.frame(x) ) {
    ef <- x
    for(i in 1:ncol(ef)){
      ef[,i] <- as.numeric(ef[,i])
    }
    class(ef) <- "data.frame"
  } else if(is.character(x) | is.list(x) | is.array(x)){
    ef <- x
    #nada
  } else {
    ef <- as.numeric(x)
  }
  return(ef)
}
