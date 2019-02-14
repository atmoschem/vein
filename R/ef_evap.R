#' Evaporative emission factor
#'
#' @description \code{\link{ef_evap}} is a lookup table with tier 2 evaporative emission factors
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
#' @param dt Character or Numeric: Average monthly temperature variation: "-5_10", "0_15", "10_25"
#' and "20_35". This argument can vector with several elements.
#' @param ca Size of canister: "no" meaning no canister, "small", "medium" and
#' "large"
#' @param k multiplication factor
#' @param show when TRUE shows row of table with respective emission factor.
#' @return emission factors in g/trip or g/proced. The object has class (g)
#' but it order to know it is g/trip or g/proceed the argument show must by T
#' @references Mellios G and Ntziachristos 2016. Gasoline evaporation. In:
#' EEA, EMEP. EEA air pollutant emission inventory guidebook-2009. European
#' Environment Agency, Copenhagen, 2009
#' @importFrom data.table rbindlist
#' @export
#' @examples {
#' # Do not run
#' ef_evap(ef = "erhotc", v = "PC", cc = "<=1400", dt = "0_15", ca = "no",
#' show = TRUE)
#' ef_evap(ef = c("erhotc",  "eshotc"), v = "PC", cc = "<=1400", dt = "0_15", ca = "no",
#' show = TRUE)
#' temps <- 10:20
#' ef_evap(ef = c("erhotc",  "eshotc"), v = "PC", cc = "<=1400", dt = temps, ca = "no",
#' show = TRUE)
#' }
ef_evap <- function (ef, v, cc, dt, ca, k = 1, show = FALSE){
  a <- (-5+10)/2
  b <-   (0+15)/2
  c <- (10+25)/2
  d <- (20+35)/2
  ta <- "-5_10"
  tb <- "0_15"
  tc <- "10_25"
  td <- "20_35"
  ef_ev <- sysdata$ev
  if(class(dt) == "factor"){
    dt <-as.character(dt)
  }
  if(is.numeric(dt)){
    dt = ifelse(dt < a, ta,
                ifelse(dt >=a & dt < b, tb,
                       ifelse(dt >= b & dt < c, tc, td
                       )))
  }

  if(length(dt) == 1){
    df <- ef_ev[ef_ev$ef %in% ef & ef_ev$veh %in% v & ef_ev$cc %in% cc &
                  ef_ev$dt == dt & ef_ev$canister %in% ca, ]

  } else {
    df <- do.call("rbind",
                  lapply(1:length(dt), function(i){
                    ef_ev[ef_ev$ef %in% ef & ef_ev$veh %in% v & ef_ev$cc %in% cc &
                            ef_ev$dt == dt[i] & ef_ev$canister %in% ca, ]
                  }))

  }
  g <- Emissions(df$g*k)
  if (show == TRUE) {
    return(df)
  } else {
    return(g)
  }
}
