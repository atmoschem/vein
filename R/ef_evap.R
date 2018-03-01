#' Evaporative emission factor
#'
#' @description A lookup table with tier 2 evaporative emission factors
#' from EMEP/EEA emisison guidelines
#'
#' @param ef Name of  evaporative emission factor as *eshotc*: mean hot-soak with
#' carburator, *eswarmc*: mean cold and warm-soak with carburator, eshotfi: mean
#' hot-soak with fuel injection, *erhotc*: mean hot running losses with
#' carburator, *erwarmc* mean cold and warm running losses, *erhotfi* mean hot
#' running losses with fuel injection
#' @param v Type of vehicles, "PC", "Motorcycles", "Motorcycles_2S" and "Moped"
#' @param cc Size of engine in cc. PC "<=1400",  "1400_2000" and "2000"
#' Motorcycles_2S:  "<=50". Motorcyces: ">50", "<250", "250_750" and ">750"
#' @param dt Average daily temperature variation: "-5_10", "0_15", "10_25"
#' and "20_35"
#' @param ca Size of canister: "no" meaning no canister, "small", "medium" and
#' "large"
#' @param k multiplication factor
#' @param show when TRUE shows row of table with respective emission factor.
#' @return emission factors in g/trip or g/proced. The object has class (g)
#' but it order to know it is g/trip or g/proceed the argument show must by T
#' @references Mellios G and Ntziachristos 2016. Gasoline evaporation. In:
#' EEA, EMEP. EEA air pollutant emission inventory guidebook-2009. European
#' Environment Agency, Copenhagen, 2009
#' @export
#' @examples \dontrun{
#' # Do not run
#' ef_evap(ef = "erhotc",v = "PC", cc = "<=1400", dt = "0_15", ca = "no",
#' show = TRUE)
#' }
ef_evap <- function (ef, v, cc, dt, ca, k = 1, show = FALSE)
{
  ef_ev <- sysdata[[3]]
  df <- ef_ev[ef_ev$ef == ef & ef_ev$veh == v & ef_ev$cc == cc &
                ef_ev$dt == dt & ef_ev$canister == ca, ]
  g <- df$g*k * units::as_units("g")
  if (show == TRUE) {
    print(df)
  }
  return(g)
}
