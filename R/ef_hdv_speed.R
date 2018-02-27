#' Emissions factors for Heavy Duty Vehicles based on average speed
#'
#' This function returns speed dependent emission factors. The emission factors
#' comes from the guidelines  EMEP/EEA air pollutant emission inventory guidebook
#' http://www.eea.europa.eu/themes/air/emep-eea-air-pollutant-emission-inventory-guidebook
#'
#' @param v Category vehicle: "Coach", "Trucks" or "Ubus"
#' @param t Sub-category of of vehicle: "3Axes", "Artic", "Midi", "RT, "Std" and "TT"
#' @param g Gross weight of each category: "<=18", ">18", "<=15", ">15 & <=18", "<=7.5",
#' ">7.5 & <=12", ">12 & <=14", ">14 & <=20", ">20 & <=26", ">26 & <=28", ">28 & <=32",
#' ">32", ">20 & <=28", ">28 & <=34", ">34 & <=40", ">40 & <=50" or ">50 & <=60"
#' @param eu Euro emission standard: "PRE", "I", "II", "III", "IV" and "V"
#' @param gr Gradient or slope of road: -0.06, -0.04, -0.02, 0.00, 0.02. 0.04 or 0.06
#' @param l Load of the vehicle: 0.0, 0.5 or 1.0
#' @param p Pollutant: "CO", "FC", "NOx" or "HC"
#' @param k Multiplication factor
#' @param show.equation Option to see or not the equation parameters
#' @return an emission factor function which depends of the average speed V  g/km
#' @keywords speed emission factors
#' @export
#' @examples {
#' V <- 0:130
#' ef1 <- ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
#' l = 0.5, p = "HC")
#' plot(1:130, ef1(1:130), pch = 16, type = "b")
#' euro <- c(rep("V", 5), rep("IV", 5), rep("III", 5), rep("II", 5),
#'           rep("I", 5), rep("PRE", 15))
#' lef <- lapply(1:30, function(i) {
#' ef_hdv_speed(v = "Trucks", t = "RT", g = ">32", gr = 0,
#' eu = euro[i], l = 0.5, p = "NOx",
#' show.equation = FALSE)(25) })
#' efs <- EmissionFactors(unlist(lef)) #returns 'units'
#' plot(efs, xlab = "age")
#' lines(efs, type = "l")
#' }
ef_hdv_speed <- function(v, t, g, eu, gr = 0, l = 0.5 ,p, k=1, show.equation=TRUE){
  ef_hdv <- sysdata[[2]]
  df <- ef_hdv[ef_hdv$VEH == v &
                 ef_hdv$TYPE == t &
                 ef_hdv$GW == g &
                 ef_hdv$EURO == eu &
                 ef_hdv$GRA == gr &
                 ef_hdv$LOAD == l &
                 ef_hdv$POLLUTANT == p, ]
  lista <- list(a = df$a,
                b = df$b,
                c = df$c,
                d = df$d,
                e = df$e,
                Equation = paste0("(",as.character(df$Y), ")", "*", k))
  if (show.equation == TRUE) {
    print(lista)
  }
  f1 <- function(V){
    a <- df$a;
    b <- df$b;
    c <- df$c ;
    d <- df$d;
    e <- df$e
    V <- ifelse(V<df$MINV,df$MINV,ifelse(V>df$MAXV,df$MAXV,V))
    eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k)))
  }
  return(f1)
}
