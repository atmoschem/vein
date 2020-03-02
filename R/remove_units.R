#' Remove units
#'
#' \code{\link{remove_units}} Remove units from sf,  data.frames, matrix or units.
#'
#' @param x Object with class "sf",  "data.frame", "matrix" or "units"
#' @return "sf", data.frame", "matrix" or numeric
#' @keywords units
#' @importFrom sf st_geometry st_set_geometry
#' @export
#' @examples \dontrun{
#' ef1 <- ef_cetesb(p = "CO", c("PC_G", "PC_FE"))
#' class(ef1)
#' sapply(ef1, class)
#' a <- remove_units(ef1)
#' }
remove_units <- function(x){
  if(inherits(x, "sf")) {
    geo <- sf::st_geometry(x)
    ef <- sf::st_set_geometry(x, NULL)
    for(i in 1:ncol(ef)){
      ef[,i] <- as.numeric(ef[,i])
    }
    ef <- sf::st_sf(ef, geometry = geo)
  } else if ( is.matrix(x) ) {
    ef <- as.data.frame(x)
    for(i in 1:ncol(ef)){
      ef[,i] <- as.numeric(ef[,i])
    }
  } else if ( is.data.frame(x) ) {
    ef <- x
    for(i in 1:ncol(ef)){
      ef[,i] <- as.numeric(ef[,i])
    }
  } else if(is.character(x) | is.list(x) | is.array(x)){
    ef <- x
    #nada
  } else {
    ef <- as.numeric(x)
  }
  return(ef)
}
