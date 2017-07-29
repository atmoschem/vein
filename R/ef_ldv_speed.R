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
#'
#' @param v Category vehicle: "PC", "LCV", "Motorcycle" or "Moped
#' @param t Sub-category of of vehicle: PC:  "ECE_1501", "ECE_1502",
#' "ECE_1503", "ECE_1504" , "IMPROVED_CONVENTIONAL", "OPEN_LOOP", "ALL",
#' "2S"  or "4S". LCV: "4S", Motorcycle: "2S" or "4S". Moped: "2S" or "4S"
#' @param cc Size of engine in cc:  PC: "<=1400", ">1400", "1400_2000", ">2000",
#' "<=800", "<=2000". Motorcycle:  ">=50" (for "2S"), "<=250", "250_750", ">=750".
#' Moped: "<=50". LCV :  "<3.5" for gross weight.
#' @param f Type of fuel: "G", "D", "LPG" or "FH" (Full Hybrid: starts by electric motor)
#' @param eu Euro standard: "PRE", "I", "II", "III", "III+DPF", "IV", "V", "VI" or "VIc"
#' @param p Pollutant: "CO", "FC", "NOx", "HC" or "PM"
#' @param k Multiplication factor
#' @param show.equation Option to see or not the equation parameters
#' @return an emission factor function which depends of the average speed V  g/km
#' @keywords speed emission factors
#' @note t = "ALL" and cc == "ALL" works for several pollutants because emission
#' fators are the same. Some exceptions are with NOx and FC because size of engine.
#' @export
#' @examples \dontrun{
#' # Do not run
#' # Passenger Cars PC
#' # Emission factor function
#' V <- 0:150
#' ef1 <- ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
#' p = "CO")
#' efs <- EmissionFactors(ef1(1:150))
#' plot(Speed(1:150), efs, xlab = "speed[km/h]")
#'
#' # List of Copert emission factors for 40 years fleet of Passenger Cars.
#' # Assuming a euro distribution of euro V, IV, III, II, and I of
#' # 5 years each and the rest 15 as PRE euro:
#' euro <- c(rep("V", 5), rep("IV", 5), rep("III", 5), rep("II", 5),
#'           rep("I", 5), rep("PRE", 15))
#' speed <- 25
#' lef <- lapply(1:40, function(i) {
#' ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
#'           eu = euro[i], p = "CO", show.equation = F)(25) })
#' # to check the emission factor with a plot
#' efs <- EmissionFactors(unlist(lef)) #returns 'units'
#' plot(efs, xlab = "age")
#' lines(efs, type = "l")
#'
#' # Light Commercial Vehicles
#' V <- 0:150
#' ef1 <- ef_ldv_speed(v = "LCV",t = "4S", cc = "<3.5", f = "G", eu = "PRE",
#' p = "CO")
#' efs <- EmissionFactors(ef1(1:150))
#' plot(Speed(1:150), efs, xlab = "speed[km/h]")
#' lef <- lapply(1:40, function(i) {
#' ef_ldv_speed(v = "LCV", t = "4S", cc = "<3.5", f = "G",
#'           eu = euro[i], p = "CO", show.equation = F)(25) })
#' # to check the emission factor with a plot
#' efs <- EmissionFactors(unlist(lef)) #returns 'units'
#' plot(efs, xlab = "age")
#' lines(efs, type = "l")
#'
#' # Motorcycles
#' V <- 0:150
#' ef1 <- ef_ldv_speed(v = "Motorcycle",t = "4S", cc = "<=250", f = "G",
#' eu = "PRE", p = "CO")
#' efs <- EmissionFactors(ef1(1:150))
#' plot(Speed(1:150), efs, xlab = "speed[km/h]")
#' # euro for motorcycles
#' eurom <- c(rep("III", 5), rep("II", 5), rep("I", 5), rep("PRE", 25))
#' lef <- lapply(1:30, function(i) {
#' ef_ldv_speed(v = "Motorcycle", t = "4S", cc = "<=250", f = "G",
#' eu = eurom[i], p = "CO",
#' show.equation = F)(25) })
#' efs <- EmissionFactors(unlist(lef)) #returns 'units'
#' plot(efs, xlab = "age")
#' lines(efs, type = "l")
#' }
ef_ldv_speed <- function(v, t  = "4S", cc, f, eu, p, k = 1, show.equation = TRUE){
  ef_ldv <- sysdata[[1]]
  df <- ef_ldv[ef_ldv$VEH == v &
              ef_ldv$TYPE == t &
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
    V <- ifelse(V<df$MINV,df$MINV,ifelse(V>df$MAXV,df$MAXV,V))
    eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k)))
  }
  return(f1)
}
