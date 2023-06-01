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
#' @param verbose Logical; to show the equation.
#' @return numeric vector.
#' @export
#' @references https://en.wikipedia.org/wiki/Logistic_function
#' @examples \dontrun{
#' CO <- ef_cetesb(p = "CO", veh = "PC_G")
#' ef_logit <- ef_fun(ef = CO, x0 = 27, k = 0.4, L = max(CO))
#' df <- data.frame(CO, ef_logit)
#' colplot(df)
#' }
ef_fun <- function(ef,
                   type = "logistic",
                   x = 1:length(ef),
                   x0 = mean(ef),
                   k = 1/4,
                   L = max(ef),
                   verbose = TRUE) {
  if(type == "logistic"){
    FD <- function(x, x0, L, k) {
      L/(1 + exp(1)^(-k*(x - x0))  )
    }
    if(verbose) print(FD)
    a <- vein::EmissionFactors(FD(x, x0 = x0, k = k, L = L))
    return(a)
  }
}

