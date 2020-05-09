#' Estimation of evaporative emissions 2
#'
#' @description \code{emis_evap} performs the estimation of evaporative emissions
#' from EMEP/EEA emisison guidelines with Tier 2.
#'
#' @param veh Total number of vehicles by age of use. If is a lsit of 'Vehicles'
#' data-frames, it will sum the columns of the eight element of the list
#' representing the 8th hour. It was chosen this hour because it is morning rush
#' hour but the user can adapt the data to this function
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
#' data(net)
#' PC_G <- c(33491,22340,24818,31808,46458,28574,24856,28972,37818,49050,87923,
#'           133833,138441,142682,171029,151048,115228,98664,126444,101027,
#'           84771,55864,36306,21079,20138,17439, 7854,2215,656,1262,476,512,
#'           1181, 4991, 3711, 5653, 7039, 5839, 4257,3824, 3068)
#' veh <- data.frame(PC_G = PC_G)
#' pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
#' ef1 <- ef_evap(ef = "erhotc",v = "PC", cc = "<=1400", dt = "0_15", ca = "no")
#' dfe <- emis_evap2(veh = pc1,
#'                  name = "PC",
#'                  size = "<=1400",
#'                  fuel = "G",
#'                  aged = 1:ncol(pc1),
#'                  nd4 = 10,
#'                  nd3 = 4,
#'                  nd2 = 2,
#'                  nd1 = 1,
#'                  hs_nd4 = ef1*1:ncol(pc1),
#'                  hs_nd3 = ef1*1:ncol(pc1),
#'                  hs_nd2 = ef1*1:ncol(pc1),
#'                  hs_nd1 = ef1*1:ncol(pc1),
#'                  d_nd4 = ef1*1:ncol(pc1),
#'                  d_nd3 = ef1*1:ncol(pc1),
#'                  d_nd2 = ef1*1:ncol(pc1),
#'                  d_nd1 = ef1*1:ncol(pc1),
#'                  rl_nd4 = ef1*1:ncol(pc1),
#'                  rl_nd3 = ef1*1:ncol(pc1),
#'                  rl_nd2 = ef1*1:ncol(pc1),
#'                  rl_nd1 = ef1*1:ncol(pc1))
#' lpc <- list(pc1, pc1, pc1, pc1,
#'             pc1, pc1, pc1, pc1)
#' dfe <- emis_evap2(veh = lpc,
#'                  name = "PC",
#'                  size = "<=1400",
#'                  fuel = "G",
#'                  aged = 1:ncol(pc1),
#'                  nd4 = 10,
#'                  nd3 = 4,
#'                  nd2 = 2,
#'                  nd1 = 1,
#'                  hs_nd4 = ef1*1:ncol(pc1),
#'                  hs_nd3 = ef1*1:ncol(pc1),
#'                  hs_nd2 = ef1*1:ncol(pc1),
#'                  hs_nd1 = ef1*1:ncol(pc1),
#'                  d_nd4 = ef1*1:ncol(pc1),
#'                  d_nd3 = ef1*1:ncol(pc1),
#'                  d_nd2 = ef1*1:ncol(pc1),
#'                  d_nd1 = ef1*1:ncol(pc1),
#'                  rl_nd4 = ef1*1:ncol(pc1),
#'                  rl_nd3 = ef1*1:ncol(pc1),
#'                  rl_nd2 = ef1*1:ncol(pc1),
#'                  rl_nd1 = ef1*1:ncol(pc1))
#' }
emis_evap2 <- function(veh,
                       name, size, fuel, aged,
                       nd4, nd3, nd2, nd1,
                       hs_nd4, hs_nd3, hs_nd2, hs_nd1,
                       rl_nd4, rl_nd3, rl_nd2, rl_nd1,
                       d_nd4, d_nd3, d_nd2, d_nd1) {
  if (inherits(veh, "list")){
    veh <- colSums(veh[[8]])
  } else {
    veh <- colSums(veh)
  }

  df <- data.frame(name = rep(name, max(aged)*4*3),
                   size = rep(size, max(aged)*4*3),
                   age = rep(aged, 4*3),
                   evaporative = c(rep("Hot Soak", 4*max(aged)),
                                   rep("Running Losses", 4*max(aged)),
                                   rep("Diurnal", 4*max(aged))),
                   g = c(hs_nd4, hs_nd3, hs_nd2, hs_nd1,
                         rl_nd4, rl_nd3, rl_nd2, rl_nd1,
                         d_nd4, d_nd3, d_nd2, d_nd1)*veh,
                   days = rep(c(rep(nd4,max(aged)),
                                rep(nd3,max(aged)),
                                rep(nd2,max(aged)),
                                rep(nd1,max(aged))),3),
                   Temperature = rep(c(rep("20_35",max(aged)),
                                       rep("10_25",max(aged)),
                                       rep("0_10",max(aged)),
                                       rep("-5_10",max(aged))),3))
  message(paste0("Evaporative NMHC emissions of ",
                 name, "_", size, " are ",
                 round(sum(df$g*df$days, na.rm = T)/1000000,2),
                 " t/(time-lapse)"))

  return(df)
}

