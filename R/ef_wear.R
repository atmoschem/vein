#' Emissions factors from tyre, break and road surface wear
#'
#' @description \code{\link{ef_wear}} estimates wear emissions.
#' The sources are tyres, breaks and road surface.
#'
#' @param wear Character; type of wear: "tyre", "break" and "road"
#' @param type Character; type of vehicle: "2W", "PC", "LCV", 'HDV"
#' @param pol Character; pollutant: "TSP", "PM10", "PM2.5", "PM1" and "PM0.1"
#' @param speed Data.frame of speeds
#' @param load Load of the HDV
#' @param axle Number of axle of the HDV
#' @return emission factors grams/km
#' @references Ntziachristos and Boulter 2016. Automobile tyre and break wear
#' and road abrasion. In: EEA, EMEP. EEA air pollutant emission inventory
#' guidebook-2009. European Environment Agency, Copenhagen, 2016
#' @export
#' @examples \dontrun{
#' data(net)
#' data(pc_profile)
#' pc_week <- temp_fact(net$ldv+net$hdv, pc_profile)
#' df <- netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' ef <- ef_wear(wear = "tyre", type = "PC", pol = "PM10", speed = df)
#' }
ef_wear <- function (wear, type, pol = "TSP", speed, load = 0.5, axle=2) {
  if(is.data.frame(speed)){
    speed <- speed
  } else if(is.matrix(speed)){
    speed <- speed
  } else if(is.vector(speed)){
    speed <- matrix(as.numeric(speed), ncol = 1)
  }
  for (i  in 1:ncol(speed) ) {
    speed[, i] <- as.numeric(speed[, i])
  }

  if (wear == "tyre") {
    ef <- ifelse(type=="2W", 0.0046,
                 ifelse(type=="PC",0.0107,
                        ifelse(type=="LCV", 0.0109,
                               ifelse(type=="HDV",
          (axle/2)*(1.41 + 1.38*load)*0.0107))))

    f <- ifelse(pol=="TSP", 1,
                ifelse(pol=="PM10", 0.6,
                       ifelse(pol=="PM2.5", 0.42,
         ifelse(pol=="PM1",0.06,
                ifelse(pol=="PM0.1",0.048,"pol")))))

sv <- do.call("cbind",lapply(1:ncol(speed), function(i){
      ifelse(speed[, i] > 40, 1.39,
             ifelse(speed[, i] >= 40 & speed <= 90,
                    -0.00974*speed[, i] + 1.78, 0.902))
    }))


  } else if (wear == "break"){
    ef <- ifelse(type=="2W", 0.0037, ifelse(type=="PC",0.0075,
          ifelse(type=="LCV", 0.0117, ifelse(type=="HDV",
          3.13*(1 + 0.79*load)*0.0037))))

    f <- ifelse(pol=="TSP", 1, ifelse(pol=="PM10",0.98,
         ifelse(pol=="PM2.5", 0.39, ifelse(pol=="PM1",0.1,
          ifelse(pol=="PM0.1",0.08,"pol")))))

    sv <- do.call("cbind",lapply(1:ncol(speed), function(i){
      ifelse(speed[, i] > 40, 1.67,
             ifelse(speed[, i] >= 40 & speed <= 90,
                    -0.0270*speed[, i] + 2.75, 0.185))
    }))

  } else if (wear == "road"){
    sv <- 1
    ef <- ifelse(type=="2W", 0.0060, ifelse(type=="PC",0.0150,
          ifelse(type=="LCV", 0.0150, ifelse(type=="HDV",0.0760))))

    f <- ifelse(pol=="TSP", 1, ifelse(pol=="PM10",0.5,
          ifelse(pol=="PM2.5", 0.27, ifelse(pol=="PM1", 0.27,
          ifelse(pol=="PM0.1", 0.27,"pol")))))
  }
  efw <- ef*f*sv
  return(EmissionFactors(efw))
}
