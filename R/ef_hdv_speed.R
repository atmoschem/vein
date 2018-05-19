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
#' @param p Character; pollutant: "CO", "FC", "NOx", "HC", "PM", "NMHC", "CH4",
#' "CO2",  "SO2" or "Pb". Only when p is "SO2" pr "Pb" x is needed. Also
#' polycyclic aromatic hydrocarbons (PAHs) and persistent organi pollutants (POPs).
#' @param x Numeric; if pollutant is "SO2", it is sulphur in fuel in ppm, if is
#' "Pb", Lead in fuel in ppm.
#' @param k Multiplication factor
#' @param show.equation Option to see or not the equation parameters
#' @return an emission factor function which depends of the average speed V  g/km
#' @keywords speed emission factors
#' @note  \strong{Pollutants}: "CO", "NOx", "HC", "PM", "CH4", "NMHC", "CO2", "SO2",
#' "Pb".
#'
#' \strong{PAH and POP}: "indeno(1,2,3-cd)pyrene", "benzo(k)fluoranthene",
#' "benzo(b)fluoranthene", "benzo(ghi)perylene", "fluoranthene",
#' "benzo(a)pyrene", "pyrene", "perylene",  "anthanthrene", "benzo(b)fluorene",
#' "benzo(e)pyrene", "triphenylene", "benzo(j)fluoranthene",
#' "dibenzo(a,j)anthacene", "dibenzo(a,l)pyrene", "3,6-dimethyl-phenanthrene",
#' "benzo(a)anthracene", "acenaphthylene", "acenapthene", "fluorene",
#' "chrysene", "phenanthrene", "napthalene",  "anthracene", "coronene",
#' "dibenzo(ah)anthracene".
#'
#' \strong{Dioxins and furans}: PCDD, PCDF and PCB expressed as (g equivalent
#' toxicity / km).
#'
#' \strong{Metals}: "As", "Cd", "Cr", "Cu", "Hg", "Ni", "Pb", "Se", "Zn" (g/km).
#' \strong{NMHC}:
#'
#' \emph{ALKANES}: "ethane", "propane", "butane", "isobutane", "pentane",
#' "isopentane", "hexane", "heptane", "octane", "TWO_methylhexane", "nonane",
#' "TWO_methylheptane", "THREE_methylhexane", "decane", "THREE_methylheptane",
#' "alcanes_C10_C12", "alkanes_C13".
#'
#' \emph{CYCLOALKANES}: "cycloalcanes".
#'
#' \emph{ALKENES}: "ethylene", "propylene", "propadiene", "ONE_butene",
#' "isobutene", "TWO_butene", "ONE_3_butadiene", "ONE_pentene", "TWO_pentene",
#' "ONE_hexene", "dimethylhexene".
#'
#' \emph{ALKYNES}:"ONE_butine", "propine", "acetylene".
#'
#' \emph{ALDEHYDES}: "formaldehyde", "acetaldehyde", "acrolein", "benzaldehyde",
#' "crotonaldehyde", "methacrolein", "butyraldehyde", "isobutanaldehyde",
#' "propionaldehyde", "hexanal", "i_valeraldehyde", "valeraldehyde",
#' "o_tolualdehyde", "m_tolualdehyde", "p_tolualdehyde".
#'
#' \emph{KETONES}: "acetone", "methylethlketone".
#'
#' \emph{AROMATICS}: "toluene", "ethylbenzene", "m_p_xylene", "o_xylene",
#' "ONE_2_3_trimethylbenzene", "ONE_2_4_trimethylbenzene",
#' "ONE_3_5_trimethylbenzene", "styrene", "benzene", "C9", "C10", "C13".
#' @export
#' @examples {
#' # Quick view
#' pol <- c("CO", "NOx", "HC", "NMHC", "CH4", "FC", "PM", "CO2", "SO2")
#' f <- sapply(1:length(pol), function(i){
#' print(pol[i])
#' ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
#' l = 0.5, p = pol[i], x = 10)(30)
#' })
#' f
#' # PAH POP
#' ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
#' l = 0.5, p = "napthalene", x = 10)(30)
#' ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
#' l = 0.5, p = "fluoranthene", x = 10)(30)
#'
#' # Dioxins and Furans
#' ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
#' l = 0.5, p = "PCB", x = 10)(30)
#'
#' # NMHC
#' ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
#' l = 0.5, p = "heptane", x = 10)(30)
#'
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
ef_hdv_speed <- function(v, t, g, eu, x, gr = 0, l = 0.5 ,p, k=1,
                         show.equation = FALSE){
  ef_hdv <- sysdata[[2]]
  df <- ef_hdv[ef_hdv$VEH == v &
                 ef_hdv$TYPE == t &
                 ef_hdv$GW == g &
                 ef_hdv$EURO == eu &
                 ef_hdv$GRA == gr &
                 ef_hdv$LOAD == l &
                 ef_hdv$POLLUTANT == p, ]
  if (show.equation == TRUE) {
    cat(paste0("a = ", df$a,
               ", b = ", df$b,
               ", c = ", df$c,
               ", d = ", df$d,
               ", e = ", df$e,
               ", f = ", df$f, "\n"))
    cat(paste0("Equation = ", "(",as.character(df$Y), ")", "*", k))
  }
  if(p %in% c("SO2","Pb")){
    f1 <- function(V){
      a <- df$a
      b <- df$b
      c <- df$c
      d <- df$d
      e <- df$e
      f <- df$f
      x <- x
      V <- ifelse(V < df$MINV, df$MINV,
                  ifelse(V > df$MAXV, df$MAXV, V))
      eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k)))
    }
  } else {
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
  }
  return(f1)
}
