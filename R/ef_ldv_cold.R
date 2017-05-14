#' Cold-Start Emissions factors for Light Duty Vehicles
#'
#' This function returns speed functions which depends on ambient temperature
#' average speed. The emission factors comes from the guidelines  EMEP/EEA air pollutant
#' emission inventory guidebook
#' http://www.eea.europa.eu/themes/air/emep-eea-air-pollutant-emission-inventory-guidebook
#'
#' @param v Category vehicle:  "LDV"
#' @param ta Ambient temperature. Monthly men can be used
#' @param cc Size of engine in cc: "<=1400",  "1400_2000" or ">2000"
#' @param f Type of fuel: "G", "D" or "LPG"
#' @param eu Euro standard: "PRE", "I", "II", "III",  "IV", "V", "VI" or "VIc"
#' @param p Pollutant: "CO", "FC", "NOx", "HC" or "PM"
#' @param k Multiplication factor
#' @param show.equation Option to see or not the equation parameters
#' @return an emission factor function which depends of the average speed V
#' and ambient temperature. g/km
#' @keywords cold emission factors
#' @export
#' @examples \dontrun{
#' # Do not run
#' V <- 0:150
#' ef1 <- ef_ldv_cold(ta = 15, cc = "<=1400", f ="G", eu = "I",
#' p = "CO")
#' ef1(10)
#' }
ef_ldv_cold <- function(v = "LDV", ta, cc, f, eu, p, k = 1, show.equation = FALSE){
  ef_ldv <- sysdata[[5]]
  df <- ef_ldv[ef_ldv$VEH == v &
             ef_ldv$CC == cc &
             ef_ldv$FUEL == f &
             ef_ldv$EURO == eu &
             ef_ldv$POLLUTANT == p, ]

  lista <- list(a = df$a,
                b = df$b,
                c = df$c,
                d = df$d,
                e = df$e,
                f = df$f,
                g = df$g,
                h = df$h,
                i = df$i,
                Equation = paste0("(",as.character(df$Y), ")", "*", k))
  if (show.equation == TRUE) {
    print(lista)
  }
  f1 <- function(V){
    a <- df$a
    b <- df$b
    c <- df$c
    d <- df$d
    e <- df$e
    f <- df$f
    g <- df$g
    h <- df$h
    i <- df$i
    V <- ifelse(V<df$MINV,df$MINV,ifelse(V>df$MAXV,df$MAXV,V))
    eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k)))
  }
  return(f1)
}
