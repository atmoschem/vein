#' Emission factor that incorporates the effect of high emitters
#'
#' \code{\link{ef_whe}} return weighted emission factors of vehicles considering
#' that one part of the fleet has a normal deterioration and another has a
#' deteriorated fleet that would be rejected in a inspection and mantainence
#' program but it is still in circulation. This emission factor might be
#' applicable in cities without a inspection and mantainence program and with
#' Weighted emission factors considering that part of the fleet are high emitters.
#'
#' @param efhe Numeric; Emission factors of high emitters vehicles. This vehicles
#' would be rejected in a inspection and mantainnence program.
#' @param phe Numeric; Percentage of high emitters.
#' @param ef Numeric; Emission factors deteriorated vehicles under normal
#' conditions. These vehicles would be approved in a inspection and mantainence
#' program.
#' @return An emission factor by annual mileage.
#' @keywords emission factors high emitters
#' @export
#' @examples {
#' # Do not run
#' # Let's say high emitter is 5 times the normal ef.
#' co_efhe <- ef_cetesb(p = "COd", "PC_G") * 5
#' # Let's say that the perfil of high emitters increases linearly
#' # till 30 years and after that percentage is constant
#' perc <- c(seq(0.01, 0.3, 0.01), rep(0.3, 10))
#' # Now, lets use our ef with normal deterioration
#' co_ef_normal <- ef_cetesb(p = "COd", "PC_G")
#' efd <- ef_whe(efhe = co_efhe,
#'               phe = perc,
#'               ef = co_ef_normal)
#' # now, we can plot the three ef
#' colplot(data.frame(co_efhe, co_ef_normal, efd))
#' }
ef_whe <- function(efhe,
                   phe,
                   ef){
  if(missing(efhe)) stop("No high emitter emission factor 'he'")
  if(missing(phe)) stop("No percentage of high emitter vehicles 'phe'")
  if(missing(ef)) stop("No emission factors with normal deterioration 'ef'")
  eff <- as.numeric(efhe) * as.numeric(phe) + as.numeric(ef) * (1 - phe)
  return(EmissionFactors(eff))
}
