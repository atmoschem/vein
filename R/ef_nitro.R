#' Emissions factors of N2O and NH3
#'
#' This function returns emission factors as a functions of accumulated mileage.
#' The emission factors comes from the guidelines  EMEP/EEA air pollutant
#' emission inventory guidebook
#' http://www.eea.europa.eu/themes/air/emep-eea-air-pollutant-emission-inventory-guidebook
#'
#' @param v Category vehicle: "PC", "LCV", "LDV", "Motorcycle", "Trucks",
#' "HDV", "HDV-A", "BUS" or "Coach".
#' @param t Type: "Cold", "Hot", "<50", ">=50", ">3.5", "7.5_12", "12_18", "28_34",
#' ">34" and "ALL".
#' @param cc "Urban", "Rural", "Highway" and "ALL".
#' @param f Type of fuel: "G", "D" or "LPG"
#' @param eu Euro standard: "PRE", "I", "II", "III", "IV", "V", "VI",
#' "VIc", "2S",  4S" and "ALL"
#' @param p Pollutant: "N2O", "NH3"
#' @param S Sulphur (ppm). Number.
#' @param k Multiplication factor
#' @param show.equation Option to see or not the equation parameters
#' @return an emission factor function which depends on the accumulated mileage
#' @keywords speed emission factors
#' @export
#' @examples \dontrun{
#' # Do not run
#' efe10 <- ef_nitro(v = "PC", t = "Hot", cc = "Urban", f = "G",
#' eu = "III", p = "NH3", S = 10,
#' show.equation = F)
#' efe50 <- ef_nitro(v = "PC", t = "Hot", cc = "Urban", f = "G",
#' eu = "III", p = "NH3", S = 50,
#' show.equation = T)
#' efe10(10)
#' efe50(10)
#' }
ef_nitro <- function(v, t, cc, f, eu, p, S, k = 1, show.equation = TRUE){
  ef <- sysdata[[8]]
  df <- ef[ef$VEH        == v &
           ef$TYPE       == t &
           ef$CONDITION  == cc &
           ef$FUEL       == f &
           ef$EURO       == eu &
           ef$POLLUTANT  == p, ]
  a  = df$a
  b  = df$b
  ab = df$ab
  c  = df$c
  d  = df$d
  cd = df$cd
  e  = df$e
  f  = df$f
  ef = df$ef
  Equation = paste0("(",as.character(df$Y), ")", "*", k)
  lista <- list(a,b,ab,c,d,cd,e,f,ef,Equation)
 if (show.equation == TRUE) {
    print(lista)
 }
  f1 <- function(km){
    eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k)))
  }
  return(f1)
}
