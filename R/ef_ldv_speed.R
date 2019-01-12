#' Emissions factors for Light Duty Vehicles and Motorcycles
#'
#' \code{\link{ef_ldv_speed}} returns speed dependent emission factors. The emission factors
#'  comes from the guidelines  EMEP/EEA air pollutant emission inventory guidebook
#' http://www.eea.europa.eu/themes/air/emep-eea-air-pollutant-emission-inventory-guidebook
#'
#' The argument of this functions have several options which results in
#' different combinations that returns emission factors. If a combination of
#' any option is wrong it will return an empty value. Therefore, it is
#' important ti know the combinations.
#'
#'
#' @param v Character; category vehicle: "PC", "LCV", "Motorcycle" or "Moped
#' @param t Character; sub-category of of vehicle: PC:  "ECE_1501", "ECE_1502",
#' "ECE_1503", "ECE_1504" , "IMPROVED_CONVENTIONAL", "OPEN_LOOP", "ALL",
#' "2S"  or "4S". LCV: "4S", Motorcycle: "2S" or "4S". Moped: "2S" or "4S"
#' @param cc Character; size of engine in cc:  PC: "<=1400", ">1400", "1400_2000", ">2000",
#' "<=800", "<=2000". Motorcycle:  ">=50" (for "2S"), "<=250", "250_750", ">=750".
#' Moped: "<=50". LCV :  "<3.5" for gross weight.
#' @param f Character; type of fuel: "G", "D", "LPG" or "FH" (Full Hybrid: starts by electric motor)
#' @param eu Character; euro standard: "PRE", "I", "II", "III", "III+DPF", "IV", "V", "VI" or "VIc".
#' When the pollutan is active surface or number of particles, eu can also be "III+DISI"
#' @param p Character; pollutant: "CO", "FC", "NOx", "NO", "NO2", "HC", "PM", "NMHC", "CH4",
#' "CO2",  "SO2" or "Pb". Only when p is "SO2" pr "Pb" x is needed. Also
#' polycyclic aromatic hydrocarbons (PAHs), persistent organi pollutants (POPs),
#' and Number of particles and Active Surface.
#' @param x Numeric; if pollutant is "SO2", it is sulphur in fuel in ppm, if is
#' "Pb", Lead in fuel in ppm.
#' @param k Numeric; multiplication factor
#' @param show.equation Logical; option to see or not the equation parameters
#' @return An emission factor function which depends of the average speed V  g/km
#' @keywords speed emission factors
#' @note t = "ALL" and cc == "ALL" works for several pollutants because emission
#' fators are the same. Some exceptions are with NOx and FC because size of engine.
#' \strong{Pollutants (g/km)}: "CO", "NOx", "HC", "PM", "CH4", "NMHC", "CO2", "SO2",
#' "Pb", "FC".
#'
#' \strong{PAH and POP (g/km)}: "indeno(1,2,3-cd)pyrene", "benzo(k)fluoranthene",
#' "benzo(b)fluoranthene", "benzo(ghi)perylene", "fluoranthene",
#' "benzo(a)pyrene", "pyrene", "perylene",  "anthanthrene", "benzo(b)fluorene",
#' "benzo(e)pyrene", "triphenylene", "benzo(j)fluoranthene",
#' "dibenzo(a,j)anthacene", "dibenzo(a,l)pyrene", "3,6-dimethyl-phenanthrene",
#' "benzo(a)anthracene", "acenaphthylene", "acenapthene", "fluorene",
#' "chrysene", "phenanthrene", "napthalene",  "anthracene", "coronene",
#' "dibenzo(ah)anthracene".
#'
#' \strong{Dioxins and furans(g equivalent toxicity / km)}: "PCDD", "PCDF" and "PCB".
#'
#' \strong{Metals (g/km)}: "As", "Cd", "Cr", "Cu", "Hg", "Ni", "Pb", "Se", "Zn".
#'
#' \strong{NMHC (g/km)}:
#'
#' \emph{ALKANES (g/km)}: "ethane", "propane", "butane", "isobutane", "pentane",
#' "isopentane", "hexane", "heptane", "octane", "TWO_methylhexane", "nonane",
#' "TWO_methylheptane", "THREE_methylhexane", "decane", "THREE_methylheptane",
#' "alcanes_C10_C12", "alkanes_C13".
#'
#' \emph{CYCLOALKANES (g/km)}: "cycloalcanes".
#'
#' \emph{ALKENES (g/km)}: "ethylene", "propylene", "propadiene", "ONE_butene",
#' "isobutene", "TWO_butene", "ONE_3_butadiene", "ONE_pentene", "TWO_pentene",
#' "ONE_hexene", "dimethylhexene".
#'
#' \emph{ALKYNES (g/km)}:"ONE_butine", "propine", "acetylene".
#'
#' \emph{ALDEHYDES (g/km)}: "formaldehyde", "acetaldehyde", "acrolein", "benzaldehyde",
#' "crotonaldehyde", "methacrolein", "butyraldehyde", "isobutanaldehyde",
#' "propionaldehyde", "hexanal", "i_valeraldehyde", "valeraldehyde",
#' "o_tolualdehyde", "m_tolualdehyde", "p_tolualdehyde".
#'
#' \emph{KETONES (g/km)}: "acetone", "methylethlketone".
#'
#' \emph{AROMATICS (g/km)}: "toluene", "ethylbenzene", "m_p_xylene", "o_xylene",
#' "ONE_2_3_trimethylbenzene", "ONE_2_4_trimethylbenzene",
#' "ONE_3_5_trimethylbenzene", "styrene", "benzene", "C9", "C10", "C13".
#'
#' \emph{Active Surface (cm2/km)}: "AS_urban", "AS_rural", "AS_highway"
#'
#' \emph{Total Number of particles (N/km)}: "N_urban", "N_rural", "N_highway",
#' "N_50nm_urban", "N_50_100nm_rural", "N_100_1000nm_highway".
#'
#' The available standards for Active Surface or number of particles are Euro I,
#' II, III, III+DPF dor diesle and III+DISI for gasoline. Pre euro vehicles
#' has the value of Euro I and  euro IV, V, VI and VIc the value of euro III.
#'
#' @export
#' @examples {
#' # Do not run
#' # Passenger Cars PC
#' # Emission factor function
#' V <- 0:150
#' ef1 <- ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
#' p = "CO")
#' efs <- EmissionFactors(ef1(1:150))
#' plot(Speed(1:150), efs, xlab = "speed[km/h]")
#'
#' # Quick view
#' pol <- c("CO", "NOx", "HC", "NMHC", "CH4", "FC", "PM", "CO2", "SO2",
#' "AS_urban", "AS_rural", "AS_highway",
#' "N_urban", "N_rural", "N_highway",
#' "N_50nm_urban", "N_50_100nm_rural", "N_100_1000nm_highway")
#' f <- sapply(1:length(pol), function(i){
#' ef_ldv_speed("PC", "4S", "<=1400", "G", "PRE", pol[i], x = 10)(30)
#' })
#' f
#' # PAH POP
#' ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
#' p = "indeno(1,2,3-cd)pyrene")(10)
#' ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
#' p = "napthalene")(10)
#'
#' # Dioxins and Furans
#' ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
#' p = "PCB")(10)
#'
#' # NMHC
#' ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
#' p = "hexane")(10)
#'
#' # List of Copert emission factors for 40 years fleet of Passenger Cars.
#' # Assuming a euro distribution of euro V, IV, III, II, and I of
#' # 5 years each and the rest 15 as PRE euro:
#' euro <- c(rep("V", 5), rep("IV", 5), rep("III", 5), rep("II", 5),
#'           rep("I", 5), rep("PRE", 15))
#' speed <- 25
#' lef <- lapply(1:40, function(i) {
#' ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
#'           eu = euro[i], p = "CO", show.equation = FALSE)(25) })
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
#'           eu = euro[i], p = "CO", show.equation = FALSE)(25) })
#' # to check the emission factor with a plot
#' efs <- EmissionFactors(unlist(lef)) #returns 'units'
#' plot(efs, xlab = "age")
#' lines(efs, type = "l")
#'
#' # Motorcycles
#' V <- 0:150
#' ef1 <- ef_ldv_speed(v = "Motorcycle",t = "4S", cc = "<=250", f = "G",
#' eu = "PRE", p = "CO",show.equation = TRUE)
#' efs <- EmissionFactors(ef1(1:150))
#' plot(Speed(1:150), efs, xlab = "speed[km/h]")
#' # euro for motorcycles
#' eurom <- c(rep("III", 5), rep("II", 5), rep("I", 5), rep("PRE", 25))
#' lef <- lapply(1:30, function(i) {
#' ef_ldv_speed(v = "Motorcycle", t = "4S", cc = "<=250", f = "G",
#' eu = eurom[i], p = "CO",
#' show.equation = FALSE)(25) })
#' efs <- EmissionFactors(unlist(lef)) #returns 'units'
#' plot(efs, xlab = "age")
#' lines(efs, type = "l")
#' }
ef_ldv_speed <- function(v, t  = "4S", cc, f, eu, p, x, k = 1,
                         show.equation = FALSE){
  ef_ldv <- sysdata$ldv
  df <- ef_ldv[ef_ldv$VEH == v &
                 ef_ldv$TYPE == t &
                 ef_ldv$CC == cc &
                 ef_ldv$FUEL == f &
                 ef_ldv$EURO == eu &
                 ef_ldv$POLLUTANT == p, ]

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
