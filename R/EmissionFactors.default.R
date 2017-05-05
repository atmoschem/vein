#' Construction function for class "EmissionFactors"
#'
#' @description Returns a tranformed object with class "EmissionFactors" and
#' units g/km. This functions has arguments to change
#' the units.
#'
#' @return Objects of class "EmissionFactors" or "units"
#'
#' @param ef Object with class "EmissionFactors"
#' @param mass Character to determine the unit of the mass. Default is "g"
#' @param distance Character to determine the distance unit. Default is "km"
#' @param ... ignored
#' @rdname EmissionFactors.default
#' @name EmissionFactors.default
#' @title EmissionFactors
#' @aliases NULL
NULL
#' @note If the class ob the object is functions, as.EmissionFactors won't
#' append another class
#' @examples \dontrun{
#' data(fe2015)
#' names(fe2015)
#' class(fe2015)
#' df <- fe2015[fe2015$Pollutant=="CO", c(ncol(fe2015)-1,ncol(fe2015))]
#' ef1 <- as.EmissionFactors(df)
#' class(ef1)
#' head(ef1)
#' }
#' @export
EmissionFactors.default <- function(ef, mass = "g", distance = "km", ...) {
  if ( is.matrix(ef) ) {
    ef <- as.data.frame(ef)
    for(i in 1:ncol(ef)){
      ef[,i] <- ef[,i] * units::parse_unit(paste0(mass," ", distance, "-1"))
    }
    class(ef) <- c("EmissionFactors",class(ef))
    efx <- ef
  } else if ( is.data.frame(ef) ) {
    for(i in 1:ncol(ef)){
      ef[,i] <- ef[,i] * units::parse_unit(paste0(mass," ", distance, "-1"))
    }
    class(ef) <- c("EmissionFactors",class(ef))
    efx <- ef
  } else if ( is.numeric(ef) ) {
    units(ef) <- ef * units::parse_unit(paste0(mass," ", distance, "-1"))
    class(ef) <- c("EmissionFactors",class(ef))
    efx <- ef
  }
  return(efx)
}
