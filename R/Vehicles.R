#' Construction function for class "Vehicles"
#'
#' @description \code{Vehicles} returns a tranformed object with class "Vehicles" and units
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
#' @param message message with average age
#' @importFrom units as_units
#'
#' @rdname Vehicles
#' @aliases Vehicles print.Vehicles summary.Vehicles plot.Vehicles
#' @examples \dontrun{
#' lt <- rnorm(100, 300, 10)
#' class(lt)
#' vlt <- Vehicles(lt)
#' class(vlt)
#' plot(vlt)
#' LT_B5 <- age_hdv(x = lt,name = "LT_B5")
#' print(LT_B5)
#' summary(LT_B5)
#' plot(LT_B5)
#' }
#' @export
Vehicles <- function(x, ...) {
  if  ( is.matrix(x) ) {
    veh <- as.data.frame(x)
    for(i in 1:ncol(veh)){
      veh[,i] <- veh[,i]*units::as_units("km h-1")
    }
    class(veh) <- c("Vehicles",class(x))
  } else if ( is.data.frame(x) ) {
    veh <- x
    for(i in 1:ncol(veh)){
      veh[,i] <- veh[,i]*units::as_units("h-1")
    }
    class(veh) <- c("Vehicles",class(x))
  } else if ( class(x) == "units" ) {
    veh <- x
    message("Check units are 1/h")
  } else if( class(x) == "numeric" | class(x) == "integer" ) {
    veh <- x*units::as_units("h-1")
  }
  return(veh)
}

#' @rdname Vehicles
#' @method print Vehicles
#' @export
print.Vehicles <- function(x, ...) {
  cat("Result for Vehicles \n")
  NextMethod("print", x)
}


#' @rdname Vehicles
#' @method summary Vehicles
#' @export
summary.Vehicles <- function(object, ...) {
  veh <- object
  avage <- sum(seq(1,ncol(veh))*colSums(veh)/sum(veh))
  cat("\nVehicles by columns in study area = \n")
  print(summary(colSums(veh)) )
  cat("Average = ", round(avage,2),"\n")
  summary(rowSums(veh))
  avveh <- mean(rowSums(veh), na.rm=T)
  cat("Vehicles by street in study area = \n")
  print(summary(rowSums(veh)))
  cat("\nAverage = ", round(avveh,2))
}

#' @rdname Vehicles
#' @method plot Vehicles
#' @export
plot.Vehicles <- function(x,  ..., message = TRUE) {
  veh <- x
  if ( inherits(veh, "data.frame") ) {
    avage <- sum(seq(1,ncol(veh)) * colSums(veh)/sum(veh))
    Veh <- colSums(veh)
    Veh <- Veh*units::as_units("h-1")
    graphics::plot(Veh, type="l", ...)
    graphics::abline(v = avage, col="red")
    if(message){
    cat("\nAverage = ",round(avage,2))
  }}
}

