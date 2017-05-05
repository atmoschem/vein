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
#' @param x Object with class "Vehicles"
#' @param object Object with class "Vehicles"
#' @param ... ignored
#'
#' @rdname Vehicles
#' @aliases Vehicles print.Vehicles summary.Vehicles plot.Vehicles
#' @examples \dontrun{
#' data(net)
#' lt <- as.Vehicles(net$hdv)
#' class(lt)
#' plot(lt)
#' }
#' @export
Vehicles <- function(x, ...) {
  veh <- x
  if  ( is.matrix(veh) ) {
    veh <- as.data.frame(veh)
    for(i in 1:ncol(veh)){
      veh[,i] <- veh[,i] * units::parse_unit(paste0(1," ", "h","-1"))
    }
    class(veh) <- c("Vehicles",class(veh))
  } else if ( is.data.frame(veh) ) {
    for(i in 1:ncol(veh)){
      veh[,i] <- veh[,i] * units::parse_unit(paste0(1," ", "h","-1"))
    }
    class(veh) <- c("Vehicles",class(veh))
  } else if (is.array(veh) && length(dim(veh)) == 3 ) {
    class(veh) <- c("Vehicles",class(veh))
  } else if ( is.numeric(veh) ) {
    veh <- veh *units::parse_unit(paste0(1," ", "h","-1"))
  }
  return(veh)
}

#' @rdname Vehicles
#' @method print Vehicles
#' @export
print.Vehicles <- function(x, ...) {
  cat("Result for Vehicles ")
  print(unclass(x),  ...)
}


#' @rdname Vehicles
#' @method summary Vehicles
#' @export
summary.Vehicles <- function(object, ...) {
  veh <- object
  avage <- sum(seq(1,ncol(veh))*colSums(veh)/sum(veh))
  cat("Vehicles by columns in study area = \n")
  print(summary(colSums(veh)) )
  cat("\nAverage = ", round(avage,2))
  summary(rowSums(veh))
  avveh <- mean(rowSums(veh), na.rm=T)
  cat("Vehicles by street in study area = \n")
  print(summary(rowSums(veh)))
  cat("\nAverage = ", round(avveh,2))
}

#' @rdname Vehicles
#' @method plot Vehicles
#' @export
plot.Vehicles <- function(x,   ...) {
  veh <- x
    avage <- sum(seq(1,ncol(veh)) * colSums(veh)/sum(veh))
    Veh <- Vehicles(colSums(veh))
    graphics::plot(Veh, type="l", ...)
    graphics::abline(v = avage, col="red")
    cat("\nAverage = ",round(avage,2))
}
