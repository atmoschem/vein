#' Emissions factors for Light Duty Vehicles and Motorcycles
#'
#' This function returns speed dependent emission factors. The emission factors
#'  comes from the guidelines  EMEP/EEA air pollutant emission inventory guidebook
#' http://www.eea.europa.eu/themes/air/emep-eea-air-pollutant-emission-inventory-guidebook
#'
#' The argument of this functions have several options which results in
#' different combinations that returns emission factors. If a combination of
#' any option is wrong it will return an empty value. Therefore, it is
#' important ti know the combinations.
#'
#' v = PC
#' \tabular{rrrrrr}{
#' v \tab t \tab cc \tab f \tab eu \tab p\cr
#' PC  \tab 6 \tab 160 \tab 110 \tab 3.90 \tab 3.90\cr
#' \tab 4 \tab 108 \tab  93 \tab 3.85 \tab 3.90\cr
#' \tab 6 \tab 258 \tab 110 \tab 3.08 \tab 3.90\cr
#' \tab 8 \tab 360 \tab 175 \tab 3.15 \tab 3.90
#' }
#'
#' @param v Category vehicle: "PC", "LCV", "Motorcycle" or "Moped
#' @param t Sub-category of of vehicle: "PRE_ECE", "ECE_1501", "ECE_1502",
#' "ECE_1503", "ECE_1504" , "IMPROVED_CONVENTIONAL", "OPEN_LOOP", "ALL",
#' "2S"  or "4S"
#' @param cc Size of engine in cc: "ALL", "<=1400", ">1400", "1400_2000", ">2000",
#' "<=800", "800_1400", "<=2000", "2S", "<=50", ">=50", "<=250", "250_750", ">=750",
#' or ">50"
#' @param f Type of fuel: "G", "D", "LPG" or "FH" (Full Hybrid: starts by electric motor)
#' @param eu Euro standard: "PRE", "I", "II", "III", "III+DPF", "IV", "V", "VI", "VIc"
#' or "ALL"
#' @param p Pollutant: "CO", "FC", "NOx", "HC" or "PM"
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
ef_ldv_speed <- function(v, t, cc, f, eu, p, k = 1, show.equation = TRUE){
  ef_ldv <- sysdata[[1]]
  df <- ef_ldv[ef_ldv$VEH == v &
              ef_ldv$TYPE == t &
             ef_ldv$CC == cc &
             ef_ldv$FUEL == f &
             ef_ldv$EURO == eu &
             ef_ldv$POLLUTANT == p, ]  # Seleçõa a equação (string) por filtro

  lista <- list(a = df$a,
                b = df$b,
                c = df$c,
                d = df$d,
                e = df$e,
                f = df$f,
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
    V <- ifelse(V<df$MINV,df$MINV,ifelse(V>df$MAXV,df$MAXV,V)) #Limita valores de V
    eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k)))
  }    # converte a equação a função
  return(f1)
}
