#' Construction function for class "Vehicles"
#'
#' @description \code{Vehicles} returns a tranformed object with class "Vehicles" and units
#'  'veh'. The type of objects supported are of classes "matrix", "data.frame",
#'  "numeric" and "array". If the object is a matrix it is converted to data.frame.
#'  If the object is "numeric" it is converted to class "units".
#'
#' @return Objects of class "Vehicles" or "units"
#'
#' @param x Object with class "Vehicles"
#' @param object Object with class "Vehicles"
#' @param ... ignored
#' @param message message with average age
#' @importFrom units as_units install_symbolic_unit
#'
#' @rdname Vehicles
#' @aliases Vehicles print.Vehicles summary.Vehicles plot.Vehicles
#' @examples {
#' lt <- rnorm(100, 300, 10)
#' class(lt)
#' vlt <- Vehicles(lt)
#' class(vlt)
#' plot(vlt)
#' LT_B5 <- age_hdv(x = lt,name = "LT_B5")
#' summary(LT_B5)
#' plot(LT_B5)
#' }
#' @export
Vehicles <- function(x, ...) {
  units::install_symbolic_unit("veh", warn = F)
  if  ( is.matrix(x) ) {
    veh <- as.data.frame(x)
    for(i in 1:ncol(veh)){
      veh[,i] <- veh[,i]*units::as_units("veh")
    }
    class(veh) <- c("Vehicles",class(veh))
  } else if ( is.data.frame(x) ) {
    veh <- x
    for(i in 1:ncol(veh)){
      veh[,i] <- veh[,i]*units::as_units("veh")
    }
    class(veh) <- c("Vehicles",class(x))
  } else if ( class(x) == "units" ) {
    veh <- x
    if(units(x)$numerator != "veh") stop("units are not 'veh'")
  } else if( class(x) == "numeric" | class(x) == "integer" ) {
    veh <- x*units::as_units("veh")
  }
  return(veh)
}

#' @rdname Vehicles
#' @method print Vehicles
#' @export
print.Vehicles <- function(x, ...) {
  nr <- ifelse(nrow(x) <= 5, nrow(x), 5)
  if(ncol(x) == 1) {
    ndf <- names(x)
    df <- data.frame(ndf = x[1:nr, ])
    names(df) <- ndf
    print.data.frame(df)
  } else {
    print.data.frame(x[1:nr, ])
  }
  if(nrow(x) > 5)     cat(paste0("... and ", nrow(x) - 5, " more rows\n"))
}

#' @rdname Vehicles
#' @method summary Vehicles
#' @export
summary.Vehicles <- function(object, ...) {
  units::install_symbolic_unit("veh", warn = F)
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
  units::install_symbolic_unit("veh", warn = F)
  veh <- x
  if ( inherits(veh, "data.frame") ) {
    avage <- sum(seq(1,ncol(veh)) * colSums(veh)/sum(veh))
    Veh <- colSums(veh)
    Veh <- Veh*units::as_units("veh")
    graphics::plot(Veh, type = "l", ...)
    graphics::abline(v = avage, col = "red")
    if(message){
    cat("\nAverage = ",round(avage,2))
  }}
}

