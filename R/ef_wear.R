#' Emissions factors from tyre, break and road surface wear
#'
#' @description Estimation of wear emissions. The sources are tyres, breaks
#' and road surface.
#'
#' @param wear  Type of wear: "tyre", "break" and "road"
#' @param type TYpe of vehicle: "2W", "PC", "LCV", 'HDV"
#' @param pol Pollutant: "TSP", "PM10", "PM2.5", "PM1" and "PM0.1"
#' @param speed List of speeds
#' @param load Load of the HDV
#' @param axle Number of axle of the HDV
#' @return emission factors grams/km
#' @references Ntziachristos and Boulter 2016. Automobile tyre and break wear
#' and road abrasion. In: EEA, EMEP. EEA air pollutant emission inventory
#' guidebook-2009. European Environment Agency, Copenhagen, 2016
#' @export
#' @examples \dontrun{
#' # Do not run
#' }
ef_wear <- function (wear, type, pol = "TSP", speed, load = 0.5, axle=2) {
  if (wear == "tyre") {
    ef <- ifelse(type=="2W", 0.0046, ifelse(type=="PC",0.0107,
          ifelse(type=="LCV", 0.0109, ifelse(type=="HDV",
          (axle/2)*(1.41 + 1.38*load)*0.0107))))

    f <- ifelse(pol=="TSP", 1, ifelse(pol=="PM10",0.6,ifelse(pol=="PM2.5",
         0.42, ifelse(pol=="PM1",0.06, ifelse(pol=="PM0.1",0.048,"pol")))))

    sv <- ifelse(speed > 40,1.39,ifelse(speed >= 40 & speed <= 90,
                    -0.00974*speed + 1.78, 0.902))

  } else if (wear == "break"){
    ef <- ifelse(type=="2W", 0.0037, ifelse(type=="PC",0.0075,
          ifelse(type=="LCV", 0.0117, ifelse(type=="HDV",
          3.13*(1 + 0.79*load)*0.0037))))

    f <- ifelse(pol=="TSP", 1, ifelse(pol=="PM10",0.98,
         ifelse(pol=="PM2.5", 0.39, ifelse(pol=="PM1",0.1,
          ifelse(pol=="PM0.1",0.08,"pol")))))

    sv <- ifelse(speed > 40, 1.67, ifelse(speed >= 40 & speed <= 90,
                    -0.0270*speed + 2.75, 0.185))

  } else if (wear == "road"){
    sv <- 1
    ef <- ifelse(type=="2W", 0.0060, ifelse(type=="PC",0.0150,
          ifelse(type=="LCV", 0.0150, ifelse(type=="HDV",0.0760))))

    f <- ifelse(pol=="TSP", 1, ifelse(pol=="PM10",0.5,
          ifelse(pol=="PM2.5", 0.27, ifelse(pol=="PM1", 0.27,
          ifelse(pol=="PM0.1", 0.27,"pol")))))
  }
  efw <- ef*f*sv
  return(as.EmissionsFactors(efw))
}
