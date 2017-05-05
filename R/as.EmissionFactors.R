#' Construction function for class "EmissionFactors" and "EmissionFactorsList"
#'
#' @description Returns a tranformed object with class "EmissionFactors" and
#' units g/km. This functions has arguments to change
#' the units.
#'
#' @return Objects of class "EmissionFactors" or "units"
#'
#' @param ef Object with class "EmissionFactors"
#' @param lfx Logical value to determine if the returning object will be
#' a list of functions or not. Each function is dependent on the speed.
#' @param mass Character to determine the unit of the mass. Default is "g"
#' @param distance Character to determine the distance unit. Default is "km"
#' @param ... ignored
#' @export
#' @rdname as.EmissionFactors
#' @name as.EmissionFactors
#' @title as.EmissionFactors
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
#' ef2 <- as.EmissionFactors(df, lfx = T)
#' class(ef2)
#' class(ef2[[1]])
#' }
as.EmissionFactors <- function(ef, lfx = F, mass = "g", distance = "km", ...) {
  if (lfx==F && is.matrix(ef)) {
    ef <- as.data.frame(ef)
    for(i in 1:ncol(ef)){
      ef[,i] <- ef[,i] * units::parse_unit(paste0(mass," ", distance, "-1"))
    }
    class(ef) <- c("EmissionFactors",class(ef))
    efx <- ef
  } else if (lfx==F && is.data.frame(ef)) {
    for(i in 1:ncol(ef)){
      ef[,i] <- ef[,i] * units::parse_unit(paste0(mass," ", distance, "-1"))
    }
    class(ef) <- c("EmissionFactors",class(ef))
    efx <- ef
  } else if (lfx==F && is.numeric(ef)) {
    units(ef) <- ef * units::parse_unit(paste0(mass," ", distance, "-1"))
    class(ef) <- c("EmissionFactors",class(ef))
    efx <- ef
  } else if (lfx == T && (is.matrix(ef) || is.data.frame(ef))) {
    efx <- lapply(1:ncol(ef), function(i) {
      lapply(1:nrow(ef), function(j) {
        function(V) ef[j,i]
      })
    })
    for (i in 1:length(efx)) {
      class(efx[[i]]) <- c("EmissionFactorsList",class(efx))
    }
    class(efx) <- c("EmissionFactorsList",class(efx))
  } else  if (lfx == T && is.numeric(ef)) {
    ef <- ef * units::parse_unit(paste0(mass," ", distance, "-1"))
    efx <- lapply(1:length(ef), function(i) {function(V) ef[i] })
    class(ef) <- c("EmissionFactorsList",class(ef))
    efx <- ef
  } else if (is.list(ef) && is.function(ef[[1]])) {
    class(ef) <- c("EmissionFactorsList",class(ef))
    efx <- ef
  }
  return(efx)
}
