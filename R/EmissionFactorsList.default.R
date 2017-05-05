#' Construction function for class "EmissionFactorsList"
#'
#' @description Returns a tranformed object with class"EmissionsFactorsList".
#' This functions has arguments to change of the numeric elements of the list.
#'
#' @return Objects of class "EmissionFactorsList"
#'
#' @param ef Object with class "list"
#' @param lfx Logical value to determine if the returning object will be
#' a list of functions or not. Each function is dependent on the speed.
#' @param mass Character to determine the unit of the mass. Default is "g"
#' @param distance Character to determine the distance unit. Default is "km"
#' @param ... ignored
#' @rdname EmissionFactorsList.default
#' @name EmissionFactorsList.default
#' @title EmissionFactorsList
#' @aliases NULL
NULL
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
#' @export
EmissionFactorsList.default <- function(ef, lfx = F, mass = "g", distance = "km", ...) {
 if (lfx == T && (is.matrix(ef) || is.data.frame(ef))) {
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
