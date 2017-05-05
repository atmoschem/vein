#' Construction function for class "Vehicles"
#'
#' @description Returns a tranformed object with class "Vehicles" and units
#'  1/h. The type of objects supported are of classes "matrix", "data.frame",
#'  "numeric" and "array". If the object is a matrix it is converted to data.frame.
#'  If the object is "numeric" it is converted to class "units". The function
#'  \code{\link{emis_paved}} needs veh to be an array, therefore in this case,
#'  veh must be an array in the total fleet at each street and dimensions
#'  total fleet, hours and days
#'
#' @return Objects of class "Vehicles" or "units"
#'
#' @param veh Object with class "Vehicles"
#' @param ... ignored
#'
#' @rdname Vehicles
#' @name Vehicles
#' @title Vehicles
#' @aliases NULL
NULL
#' @examples \dontrun{
#' data(net)
#' lt <- as.Vehicles(net$hdv)
#' class(lt)
#' plot(lt)
#' }
#' @export
Vehicles <- function(veh, ...) {
  if  ( is.matrix(veh) ) {
    veh <- as.data.frame(veh)
    for(i in 1:ncol(veh)){
      units(veh[,i]) <- with(units::ud_units, 1/h)
    }
    class(veh) <- c("Vehicles",class(veh))
  } else if ( is.data.frame(veh) ) {
    for(i in 1:ncol(veh)){
      units(veh[,i]) <- with(units::ud_units, 1/h)
    }
    class(veh) <- c("Vehicles",class(veh))
  } else if (is.array(veh) && length(dim(veh)) == 3 ) {
    class(veh) <- c("Vehicles",class(veh))
  } else if ( is.numeric(veh) ) {
    units(veh) <- with(units::ud_units, 1/h)
  }
  return(veh)
}
