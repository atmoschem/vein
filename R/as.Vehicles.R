#' Construction function for class "Vehicles"
#'
#' @description Returns a tranformed object with class "Vehicles" and units
#'  1/h. The type of objects supported are of classes "matrix", "data.frame"
#'  and "numeric". If the object is a matrix it is converted to data.frame.
#'  If the object is "numeric" it is converted to class "units".
#'
#' @return Define vehicle classes inhetis of data.frame
#' @export
#' @examples \dontrun{
#' data(net)
#' lt <- as.Vehicles(net$hdv)
#' class(lt)
#' plot(lt)
#' }
as.Vehicles <- function(veh, ...) {
  if  ( is.matrix(veh) ) {
    veh <- as.data.frame(veh)
    for(i in 1:ncol(veh)){
      units(veh[,i]) <- with(ud_units, 1/h)
    }
    class(veh) <- c("Vehicles",class(veh))
  } else if ( is.data.frame(veh) ) {
    for(i in 1:ncol(veh)){
      units(veh[,i]) <- with(ud_units, 1/h)
    }
    class(veh) <- c("Vehicles",class(veh))
  } else {
    units(veh) <- with(ud_units, 1/h)
  }
  return(veh)
}
