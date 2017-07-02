#' Emissions factors of N2O and NH3
#'
#' This function returns emission factors as a functions of accumulated mileage.
#' The emission factors comes from the guidelines  EMEP/EEA air pollutant
#' emission inventory guidebook
#' http://www.eea.europa.eu/themes/air/emep-eea-air-pollutant-emission-inventory-guidebook
#'
#' @param v Category vehicle: "PC", "LCV", "Motorcycle" or "Moped
#' @param t Type: "PC", "LCV", "LDV", "Motorcycles", "Trucks", "HDV", "HDV-A",
#' "BUS" and "Coach"
#' @param cc "Cold", "Hot", "<50", ">=50", ">3.5", "7.5_12", "12_28", "28_34",
#' ">34", "ALL".
#' @param f Type of fuel: "G", "D" or "LPG"
#' @param eu Euro standard: "PRE", "I", "II", "III", "III+DPF", "IV", "V", "VI",
#' "VIc", "2S",  4S" and "ALL"
#' @param p Pollutant: "N2O", "NH3"
#' @param S Sulphur (ppm)
#' @param k Multiplication factor
#' @param show.equation Option to see or not the equation parameters
#' @return an emission factor function which depends of the average speed V  g/km
#' @keywords speed emission factors
#' @export
#' @examples \dontrun{
#' # Do not run
#' V <- 0:150
#' ef1 <- ef_ldv_speed(v = "PC",t = "PRE_ECE", cc = "ALL", f = "G", eu = "PRE", p = "CO")
#' plot(1:150, ef1(1:150))
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
