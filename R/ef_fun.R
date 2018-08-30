#' Experimental: Returns a function of Emission Factor by age of use
#'
#' @description \code{\link{ef_fun}} returns amount of vehicles at each age
#'
#' @param ef Numeric; numeric vector of emission factors.
#' @param type Character; "logistic"  by default so far.
#' @param x Numeric; vector for ages of use.
#' @param x0 Numeric;  the x-value of the sigmoid's midpoint,
#' @param k Numeric; the steepness of the curve.
#' @param L Integer; the curve's maximum value.
#' @return dataframe of age distrubution of vehicles at each street.
#' @export
#' @references https://en.wikipedia.org/wiki/Logistic_function
#' @examples {
#' data(fe2015)
#' CO <- vein::EmissionFactors(fe2015[fe2015$Pollutant == "CO", 11])
#' ef_logit <- ef_fun(ef = CO, x0 = 27, k = 0.4, L = 33)
#' plot(ef_logit, type = "b", pch = 16)
#' lines(ef_logit, pch = 16, col = "blue")
#' }
ef_fun <- function(ef,
                   type = "logistic",
                   x = 1:length(ef),
                   x0 = mean(ef),
                   k = 1/4,
                   L = max(ef)) {
  if(type == "logistic"){
    FD <- function(x, x0, L, k) {
      L/(1 + exp(1)^(-k*(x - x0))  )
    }
    a <- vein::EmissionFactors(FD(x, x0 = x0, k = k, L = L))
    return(a)
  }
}

