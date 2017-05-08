#' Estimation of evaporative emissions
#'
#' @description Estimation of evaporative emissions from EMEP/EEA emisison guidelines
#'
#' @param veh Total number of vehicles by age of use
#' @param name Character of type of vehicle
#' @param size Character of size of vehicle
#' @param fuel Character of fuel of vehicle
#' @param aged Age distribution vector. E.g.: 1:40
#' @param nd4 Number of days with temperature between 20 and 35 celcius degrees
#' @param nd3 Number of days with temperature between 10 and 25 celcius degrees
#' @param nd2 Number of days with temperature between 0 and 15 celcius degrees
#' @param nd1 Number of days with temperature between -5 and 10 celcius degrees
#' @param hs_nd4 average daily hot-soak evaporative emissions for days with
#' temperature between 20 and 35 celcius degrees
#' @param hs_nd3 average daily hot-soak evaporative emissions for days with
#' temperature between 10 and 25 celcius degrees
#' @param hs_nd2 average daily hot-soak evaporative emissions for days with
#' temperature between 0 and 15 celcius degrees
#' @param hs_nd1 average daily hot-soak evaporative emissions for days with
#' temperature between -5 and 10 celcius degrees
#' @param rl_nd4 average daily running losses evaporative emissions for days with
#' temperature between 20 and 35 celcius degrees
#' @param rl_nd3 average daily running losses evaporative emissions for days with
#' temperature between 10 and 25 celcius degrees
#' @param rl_nd2 average daily running losses evaporative emissions for days with
#' temperature between 0 and 15 celcius degrees
#' @param rl_nd1 average daily running losses evaporative emissions for days with
#' temperature between -5 and 10 celcius degrees
#' @param d_nd4 average daily diurnal evaporative emissions for days with
#' temperature between 20 and 35 celcius degrees
#' @param d_nd3 average daily diurnal evaporative emissions for days with
#' temperature between 10 and 25 celcius degrees
#' @param d_nd2 average daily diurnal evaporative emissions for days with
#' temperature between 0 and 15 celcius degrees
#' @param d_nd1 average daily diurnal evaporative emissions for days with
#' temperature between -5 and 10 celcius degrees
#' @return dataframe of emission estimation in grams/days
#' @references Mellios G and Ntziachristos 2016. Gasoline evaporation. In:
#' EEA, EMEP. EEA air pollutant emission inventory guidebook-2009. European
#' Environment Agency, Copenhagen, 2009
#' @export
#' @examples \dontrun{
#' # Do not run
#' ef1 <- ef_evap(ef = "erhotc",v = "PC", cc = "<=1400", dt = "0_15", ca = "no")
#' dfe <- emis_evap(rep(50,3),"PC","<=1400","G", 1:3,
#'                      10,4,2,1,
#'                      ef1*1:3, ef1*1:3, ef1*1:3, ef1*1:3,
#'                      ef1*1:3, ef1*1:3, ef1*1:3, ef1*1:3,
#'                      ef1*1:3, ef1*1:3, ef1*1:3, ef1*1:3)
#' }
emis_evap <- function(veh, name, size, fuel, aged,
                      nd4, nd3, nd2, nd1,
                      hs_nd4, hs_nd3, hs_nd2, hs_nd1,
                      rl_nd4, rl_nd3, rl_nd2, rl_nd1,
                      d_nd4, d_nd3, d_nd2, d_nd1) {
  if (missing(veh) | is.null(veh)) {
    stop (print("Missing vehicles"))
  } else {
    veh <- as.numeric(veh)
  df <- data.frame(name = rep(name, max(aged)*4*3),
                   size = rep(size, max(aged)*4*3),
                   age = rep(aged, 4*3),
                   evaporative = c(rep("Hot Soak", 4*max(aged)),
                                   rep("Running Losses", 4*max(aged)),
                                   rep("Diurnal", 4*max(aged))),
                   g = Evaporative(
                     c(hs_nd4, hs_nd3, hs_nd2, hs_nd1,
                       rl_nd4, rl_nd3, rl_nd2, rl_nd1,
                       d_nd4, d_nd3, d_nd2, d_nd1)*veh),
                   days = rep(c(rep(nd4,max(aged)),
                                rep(nd3,max(aged)),
                                rep(nd2,max(aged)),
                                rep(nd1,max(aged))),3),
                   Temperature = rep(c(rep("20_35",max(aged)),
                                rep("10_25",max(aged)),
                                rep("0_10",max(aged)),
                                rep("-5_10",max(aged))),3))
  message(paste0("Annual evaporative NMHC emissions of ",
                 name, "_", size, " are ",
                 round(sum(df$g*df$days, na.rm = T)/1000000,2),
                 " t/(time-lapse)"))
  }
  return(df)
}

