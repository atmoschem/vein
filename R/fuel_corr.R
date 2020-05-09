#' Correction due Fuel effects
#'
#' @description Take into account the effect of better fuels on vehicles with
#' older technology. If the ratio is less than 1, return 1. It means that it is
#' nota degradation function.
#'
#' @param euro Character; Euro standards ("PRE", "I", "II", "III", "IV", "V",
#' VI, "VIc")
#' @param g Numeric; vector with parameters of gasoline with the names:
#' e100(vol. %), aro (vol. %), o2 (wt. %), e150 (%), olefin vol. % and s
#' (sulphur, ppm)
#' @param d Numeric; vector with parameters for diesel with the names:
#' den (density at 15 celcius degrees kg/m3), pah (%), cn (number), t95
#' (Back end distillation in Celcius degrees) and s  (sulphur, ppm)
#' @return A list with the correction of emission factors.
#' @note This function cannot be used to account for deterioration, therefore,
#' it is restricted to values between 0 and 1.
#' Parameters for gasoline (g):
#'
#' O2 = Oxygenates in %
#'
#' S = Sulphur content in ppm
#'
#' ARO = Aromatics content in %
#'
#' OLEFIN = Olefins content in %
#'
#' E100 = Mid range volatility in %
#'
#' E150 = Tail-end volatility in %
#'
#' Parameters for diesel (d):
#'
#' DEN = Density at 15 C (kg/m3)
#'
#' S = Sulphur content in ppm
#'
#' PAH = Aromatics content in %
#'
#' CN = Cetane number
#'
#' T95 = Back-end distillation in o C.
#'
#' @export
#' @examples \dontrun{
#' f <- fuel_corr(euro = "I")
#' names(f)
#' }
fuel_corr <- function(euro,
                      g = c(e100 = 52,   # vol. %
                            aro = 39,    # vol. %
                            o2 = 0.4,    # wt. %
                            e150 = 86,   # %
                            olefin = 10, # vol. %
                            s = 165),    # ppm
                      d = c(den = 840,     # kg/m3
                            pah = 9,       # %
                            cn = 51,       # number
                            t95 = 350,     # C
                            s = 400)       # ppm
){
  # Pre Euro
  bg1996 <- c(e100 = 52, aro = 39, o2 = 0.4, e150 = 86, olefin = 10, s = 165)
  # Euro 3
  bg2000 <- c(e100 = 52, aro = 37, o2 = 1, e150 = 86, olefin = 10, s = 130)
  # Euro 4
  bg2005 <- c(e100 = 52, aro = 33, o2 = 1.5, e150 = 86, olefin = 10, s = 40)
  # Pre Euro
  bd1996 <-  c(den = 840, pah = 9, cn = 51, t95 = 350, s = 400)
  # Euro 3
  bd2000 <-  c(den = 840,  pah = 7, cn = 53, t95 = 330, s = 300)
  # Euro 4
  bd2005 <-  c(den = 835,  pah = 5, cn = 53, t95 = 320, s = 40)
  f_co_ldv_g <- function(e100, aro, o2, s, e150) {
    (2.459 - 0.05513*e100 + 0.0005343*e100^2 + 0.009226*aro -
       0.0003101*(97-s))*(1 - 0.037*(o2 - 1.75))*(1-0.008*(e150 - 90.2))}
  f_cov_ldv_g <- function(aro, e100, s, olefin, o2, e150){
    (0.1347 + 0.0005489*aro + 25.7*aro*exp(-0.2642*e100) - 0.0000406*(
      97 - s))*(1-0.004*(olefin - 4.97))*(1 - 0.022*(o2 - 1.75))*(
        1 - 0.01*(e150 - 90.2))}
  f_nox_ldv_g <- function(aro, e100, s, olefin, o2, e150){
    (0.1884 - 0.001438*aro + 0.00001959*aro*e100 - 0.00005302*(
      97 - s))*(1 + 0.004*(olefin - 4.97))*(1 + 0.001*(o2 - 1.75))*(
        1 + 0.008*(e150 - 90.2))}

  f_co_ldv_d <- function(den, pah, cn, t95) {
    -1.3250726 + 0.003037*den - 0.0025643*pah - 0.015856*cn + 0.0001706*t95}
  f_cov_ldv_d <- function(den, pah, cn, t95) {
    -0.293192 + 0.0006759*den - 0.0007306*pah - 0.0032733*cn - 0.000038*t95}
  f_nox_ldv_d <- function(den, pah, cn, t95) {
    1.0039726 - 0.0003113*den + 0.0027263*pah - 0.0000883*cn - 0.0005805*t95}
  f_pm_ldv_d <- function(den, pah, cn, t95, s){
    (-0.3879873 + 0.0004677*den + 0.0004488*pah + 0.0004098*cn + 0.0000788*t95)*(
      1 - 0.015*(450 - s)/100)}

  f_co_hdv <- function(den, pah, cn, t95) {
    2.24407 - 0.0011*den + 0.00007*pah - 0.00768*cn + 0.0001706*t95}
  f_cov_hdv <- function(den, pah, cn, t95) {
    1.61466 - 0.00123*den + 0.00133*pah - 0.00181*cn - 0.00068*t95}
  f_nox_hdv <- function(den, pah, cn, t95) {
    -1.75444 + 0.00906*den - 0.0163*pah + 0.00493*cn + 0.00266*t95}
  f_pm_hdv <- function(den, pah, cn, t95, s){
    (0.06959 + 0.00006*den + 0.00065*pah - 0.00001*cn)*(1 - 0.0086*(450 - s)/100)}

  # if(tveh == "LDVG"){
  if(euro %in% c("PRE", "I", "II")){
    fco_ldv_g <- f_co_ldv_g(e100 = g[["e100"]],
                            aro = g[["aro"]],
                            o2 = g[["o2"]],
                            s = g[["s"]],
                            e150 = g[["e150"]])/
      f_co_ldv_g(e100 = bg1996[["e100"]],
                 aro = bg1996[["aro"]],
                 o2 = bg1996[["o2"]],
                 s = bg1996[["s"]],
                 e150 = bg1996[["e150"]])
    fcov_ldv_g <- f_cov_ldv_g(aro = g[["aro"]],
                              e100 = g[["e100"]],
                              s = g[["s"]],
                              olefin = g[["olefin"]],
                              o2 = g[["o2"]],
                              e150 = bg1996[["e150"]])/
      f_cov_ldv_g(aro = bg1996[["aro"]],
                  e100 = bg1996[["e100"]],
                  s = bg1996[["s"]],
                  olefin = bg1996[["olefin"]],
                  o2 = bg1996[["o2"]],
                  e150 = bg1996[["e150"]])
    fnox_ldv_g <- f_nox_ldv_g(aro = g[["aro"]],
                              e100 = g[["e100"]],
                              s = g[["s"]],
                              olefin = g[["olefin"]],
                              o2 = g[["o2"]],
                              e150 = g[["e150"]])/
      f_nox_ldv_g(aro = bg1996[["aro"]],
                  e100 = bg1996[["e100"]],
                  s = bg1996[["s"]],
                  olefin = bg1996[["olefin"]],
                  o2 = bg1996[["o2"]],
                  e150 = bg1996[["e150"]])
  } else if(euro == "III"){
    fco_ldv_g <- f_co_ldv_g(e100 = g[["e100"]],
                            aro = g[["aro"]],
                            o2 = g[["o2"]],
                            s = g[["s"]],
                            e150 = g[["e150"]])/
      f_co_ldv_g(e100 = bg2000[["e100"]],
                 aro = bg2000[["aro"]],
                 o2 = bg2000[["o2"]],
                 s = bg2000[["s"]],
                 e150 = bg2000[["e150"]])
    fcov_ldv_g <- f_cov_ldv_g(aro = g[["aro"]],
                              e100 = g[["e100"]],
                              s = g[["s"]],
                              olefin = g[["olefin"]],
                              o2 = g[["o2"]],
                              e150 = bg2000[["e150"]])/
      f_cov_ldv_g(aro = bg2000[["aro"]],
                  e100 = bg2000[["e100"]],
                  s = bg2000[["s"]],
                  olefin = bg2000[["olefin"]],
                  o2 = bg2000[["o2"]],
                  e150 = bg2000[["e150"]])
    fnox_ldv_g <- f_nox_ldv_g(aro = g[["aro"]],
                              e100 = g[["e100"]],
                              s = g[["s"]],
                              olefin = g[["olefin"]],
                              o2 = g[["o2"]],
                              e150 = g[["e150"]])/
      f_nox_ldv_g(aro = bg2000[["aro"]],
                  e100 = bg2000[["e100"]],
                  s = bg2000[["s"]],
                  olefin = bg2000[["olefin"]],
                  o2 = bg2000[["o2"]],
                  e150 = bg2000[["e150"]])
  } else if(euro == "IV"){
    fco_ldv_g <- f_co_ldv_g(e100 = g[["e100"]],
                            aro = g[["aro"]],
                            o2 = g[["o2"]],
                            s = g[["s"]],
                            e150 = g[["e150"]])/
      f_co_ldv_g(e100 = bg2005[["e100"]],
                 aro = bg2005[["aro"]],
                 o2 = bg2005[["o2"]],
                 s = bg2005[["s"]],
                 e150 = bg2005[["e150"]])
    fcov_ldv_g <- f_cov_ldv_g(aro = g[["aro"]],
                              e100 = g[["e100"]],
                              s = g[["s"]],
                              olefin = g[["olefin"]],
                              o2 = g[["o2"]],
                              e150 = bg2005[["e150"]])/
      f_cov_ldv_g(aro = bg2005[["aro"]],
                  e100 = bg2005[["e100"]],
                  s = bg2005[["s"]],
                  olefin = bg2005[["olefin"]],
                  o2 = bg2005[["o2"]],
                  e150 = bg2005[["e150"]])
    fnox_ldv_g <- f_nox_ldv_g(aro = g[["aro"]],
                              e100 = g[["e100"]],
                              s = g[["s"]],
                              olefin = g[["olefin"]],
                              o2 = g[["o2"]],
                              e150 = g[["e150"]])/
      f_nox_ldv_g(aro = bg2005[["aro"]],
                  e100 = bg2005[["e100"]],
                  s = bg2005[["s"]],
                  olefin = bg2005[["olefin"]],
                  o2 = bg2005[["o2"]],
                  e150 = bg2005[["e150"]])
  } else if(euro %in% c("V", "VI", "VIc")){
    fco_ldv_g <- fcov_ldv_g <- fnox_ldv_g <- 1
  }

  # } else if(tveh == "LDVD"){
  if(euro %in% c("PRE", "I", "II")){
    fco_ldv_d <- f_co_ldv_d(den = d[["den"]],
                            pah = d[["pah"]],
                            cn = d[["cn"]],
                            t95 = d[["t95"]])/
      f_co_ldv_d(den = bd1996[["den"]],
                 pah = bd1996[["pah"]],
                 cn = bd1996[["cn"]],
                 t95 = bd1996[["t95"]])
    fcov_ldv_d <- f_cov_ldv_d(den = d[["den"]],
                              pah = d[["pah"]],
                              cn = d[["cn"]],
                              t95 = d[["t95"]])/
      f_cov_ldv_d(den = bd1996[["den"]],
                  pah = bd1996[["pah"]],
                  cn = bd1996[["cn"]],
                  t95 = bd1996[["t95"]])
    fnox_ldv_d <- f_nox_ldv_d(den = d[["den"]],
                              pah = d[["pah"]],
                              cn = d[["cn"]],
                              t95 = d[["t95"]])/
      f_nox_ldv_d(den = bd1996[["den"]],
                  pah = bd1996[["pah"]],
                  cn = bd1996[["cn"]],
                  t95 =bd1996[["t95"]])
    fpm_ldv_d <- f_pm_ldv_d(den = d[["den"]],
                            pah = d[["pah"]],
                            cn = d[["cn"]],
                            t95 = d[["t95"]],
                            s = d[["s"]])/
      f_pm_ldv_d(den = bd1996[["den"]],
                 pah = bd1996[["pah"]],
                 cn = bd1996[["cn"]],
                 t95 = bd1996[["t95"]],
                 s = bd1996[["s"]])

  } else if(euro == "III"){
    fco_ldv_d <- f_co_ldv_d(den = d[["den"]],
                            pah = d[["pah"]],
                            cn = d[["cn"]],
                            t95 = d[["t95"]])/
      f_co_ldv_d(den = bd2000[["den"]],
                 pah = bd2000[["pah"]],
                 cn = bd2000[["cn"]],
                 t95 = bd2000[["t95"]])
    fcov_ldv_d <- f_cov_ldv_d(den = d[["den"]],
                              pah = d[["pah"]],
                              cn = d[["cn"]],
                              t95 = d[["t95"]])/
      f_cov_ldv_d(den = bd2000[["den"]],
                  pah = bd2000[["pah"]],
                  cn = bd2000[["cn"]],
                  t95 = bd2000[["t95"]])
    fnox_ldv_d <- f_nox_ldv_d(den = d[["den"]],
                              pah = d[["pah"]],
                              cn = d[["cn"]],
                              t95 = d[["t95"]])/
      f_nox_ldv_d(den = bd2000[["den"]],
                  pah = bd2000[["pah"]],
                  cn = bd2000[["cn"]],
                  t95 =bd2000[["t95"]])
    fpm_ldv_d <- f_pm_ldv_d(den = d[["den"]],
                            pah = d[["pah"]],
                            cn = d[["cn"]],
                            t95 = d[["t95"]],
                            s = d[["s"]])/
      f_pm_ldv_d(den = bd2000[["den"]],
                 pah = bd2000[["pah"]],
                 cn = bd2000[["cn"]],
                 t95 = bd2000[["t95"]],
                 s = bd2000[["s"]])

  } else if(euro == "IV"){
    fco_ldv_d <- f_co_ldv_d(den = d[["den"]],
                            pah = d[["pah"]],
                            cn = d[["cn"]],
                            t95 = d[["t95"]])/
      f_co_ldv_d(den = bd2005[["den"]],
                 pah = bd2005[["pah"]],
                 cn = bd2005[["cn"]],
                 t95 = bd2005[["t95"]])
    fcov_ldv_d <- f_cov_ldv_d(den = d[["den"]],
                              pah = d[["pah"]],
                              cn = d[["cn"]],
                              t95 = d[["t95"]])/
      f_cov_ldv_d(den = bd2005[["den"]],
                  pah = bd2005[["pah"]],
                  cn = bd2005[["cn"]],
                  t95 = bd2005[["t95"]])
    fnox_ldv_d <- f_nox_ldv_d(den = d[["den"]],
                              pah = d[["pah"]],
                              cn = d[["cn"]],
                              t95 = d[["t95"]])/
      f_nox_ldv_d(den = bd2005[["den"]],
                  pah = bd2005[["pah"]],
                  cn = bd2005[["cn"]],
                  t95 =bd2005[["t95"]])
    fpm_ldv_d <- f_pm_ldv_d(den = d[["den"]],
                            pah = d[["pah"]],
                            cn = d[["cn"]],
                            t95 = d[["t95"]],
                            s = d[["s"]])/
      f_pm_ldv_d(den = bd2005[["den"]],
                 pah = bd2005[["pah"]],
                 cn = bd2005[["cn"]],
                 t95 = bd2005[["t95"]],
                 s = bd2005[["s"]])
  } else if(euro %in% c("V", "VI", "VIc")){
    fco_ldv_d <- fcov_ldv_d <- fnox_ldv_d <- fpm_ldv_d <- 1
  }

  # } else if(tveh == "HDV"){
  if(euro %in% c("PRE", "I", "II")){
    fco_hdv <- f_co_hdv(den = d[["den"]],
                        pah = d[["pah"]],
                        cn = d[["cn"]],
                        t95 = d[["t95"]])/
      f_co_hdv(den = bd1996[["den"]],
               pah = bd1996[["pah"]],
               cn = bd1996[["cn"]],
               t95 = bd1996[["t95"]])
    fcov_hdv <- f_cov_hdv(den = d[["den"]],
                          pah = d[["pah"]],
                          cn = d[["cn"]],
                          t95 = d[["t95"]])/
      f_cov_hdv(den = bd1996[["den"]],
                pah = bd1996[["pah"]],
                cn = bd1996[["cn"]],
                t95 = bd1996[["t95"]])
    fnox_hdv <- f_nox_hdv(den = d[["den"]],
                          pah = d[["pah"]],
                          cn = d[["cn"]],
                          t95 = d[["t95"]])/
      f_nox_hdv(den = bd1996[["den"]],
                pah = bd1996[["pah"]],
                cn = bd1996[["cn"]],
                t95 =bd1996[["t95"]])
    fpm_hdv <- f_pm_hdv(den = d[["den"]],
                        pah = d[["pah"]],
                        cn = d[["cn"]],
                        t95 = d[["t95"]],
                        s = d[["s"]])/
      f_pm_hdv(den = bd1996[["den"]],
               pah = bd1996[["pah"]],
               cn = bd1996[["cn"]],
               t95 = bd1996[["t95"]],
               s = bd1996[["s"]])
  } else if(euro == "III"){
    fco_hdv <- f_co_hdv(den = d[["den"]],
                        pah = d[["pah"]],
                        cn = d[["cn"]],
                        t95 = d[["t95"]])/
      f_co_hdv(den = bd2000[["den"]],
               pah = bd2000[["pah"]],
               cn = bd2000[["cn"]],
               t95 = bd2000[["t95"]])
    fcov_hdv <- f_cov_hdv(den = d[["den"]],
                          pah = d[["pah"]],
                          cn = d[["cn"]],
                          t95 = d[["t95"]])/
      f_cov_hdv(den = bd2000[["den"]],
                pah = bd2000[["pah"]],
                cn = bd2000[["cn"]],
                t95 = bd2000[["t95"]])
    fnox_hdv <- f_nox_hdv(den = d[["den"]],
                          pah = d[["pah"]],
                          cn = d[["cn"]],
                          t95 = d[["t95"]])/
      f_nox_hdv(den = bd2000[["den"]],
                pah = bd2000[["pah"]],
                cn = bd2000[["cn"]],
                t95 =bd2000[["t95"]])
    fpm_hdv <- f_pm_hdv(den = d[["den"]],
                        pah = d[["pah"]],
                        cn = d[["cn"]],
                        t95 = d[["t95"]],
                        s = d[["s"]])/
      f_pm_hdv(den = bd2000[["den"]],
               pah = bd2000[["pah"]],
               cn = bd2000[["cn"]],
               t95 = bd2000[["t95"]],
               s = bd2000[["s"]])
  } else if(euro == "IV"){
    fco_hdv <- f_co_hdv(den = d[["den"]],
                        pah = d[["pah"]],
                        cn = d[["cn"]],
                        t95 = d[["t95"]])/
      f_co_hdv(den = bd2005[["den"]],
               pah = bd2005[["pah"]],
               cn = bd2005[["cn"]],
               t95 = bd2005[["t95"]])
    fcov_hdv <- f_cov_hdv(den = d[["den"]],
                          pah = d[["pah"]],
                          cn = d[["cn"]],
                          t95 = d[["t95"]])/
      f_cov_hdv(den = bd2005[["den"]],
                pah = bd2005[["pah"]],
                cn = bd2005[["cn"]],
                t95 = bd2005[["t95"]])
    fnox_hdv <- f_nox_hdv(den = d[["den"]],
                          pah = d[["pah"]],
                          cn = d[["cn"]],
                          t95 = d[["t95"]])/
      f_nox_hdv(den = bd2005[["den"]],
                pah = bd2005[["pah"]],
                cn = bd2005[["cn"]],
                t95 =bd2005[["t95"]])
    fpm_hdv <- f_pm_hdv(den = d[["den"]],
                        pah = d[["pah"]],
                        cn = d[["cn"]],
                        t95 = d[["t95"]],
                        s = d[["s"]])/
      f_pm_hdv(den = bd2005[["den"]],
               pah = bd2005[["pah"]],
               cn = bd2005[["cn"]],
               t95 = bd2005[["t95"]],
               s = bd2005[["s"]])
  } else if(euro %in% c("V", "VI", "VIc")){
    fco_hdv <- fcov_hdv <- fnox_hdv <- fpm_hdv <- 1
  }
  fif <- function(x) ifelse(x >= 1, 1, x)
  dfl <- list(
    LDVG = list(CO = list(fif(fco_ldv_g)),
                COV = list(fif(fcov_ldv_g)),
                NOx = list(fif(fnox_ldv_g))),
    LDVD = list(CO = list(fif(fco_ldv_d)),
                COV = list(fif(fcov_ldv_d)),
                NOx = list(fif(fnox_ldv_d)),
                PM = list(fif(fnox_ldv_d))),
    HDV = list(CO = list(fif(fco_hdv)),
               COV = list(fif(fcov_hdv)),
               NOx = list(fif(fnox_hdv)),
               PM = list(fif(fpm_hdv))))
  return(dfl)

}

