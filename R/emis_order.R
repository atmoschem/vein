#' Re-order the emission to match specific hours and days
#'
#' @description returns the emission array matching with corresponding weekdays and with
#' the desired number of hours, recycling or droping hours from the emission array. For
#' instance, if your emissions starts Monday at 00:00 and cover 168 hours, and you
#' want to reorder them to start saturday you with a total a new length of hours
#' of 241, you must emis_order(EMISSION, as.Date("2016-04-06"), 241)
#'
#' @param EMISSION one of the following:
#'
#' 1) GriddedEmissionsArray or array with characteristics of GriddedEmissionsArray
#' 2) Spatial object of class "Spatial". Columns are hourly emissions.
#' 3) Spatial Object of class "sf". Columns are hourly emissions.
#' 4) "data.frame", "matrix" or "Emissions". Columns are hourly emissions.
#'
#' @param start Date or the start weekday or first 3 letters
#' @param hours Numeric; number of hours needed to the simualation
#' @param utc Integer; transform local into UTC emissions. For instance,
#' utc = -3 means that the first hour of emissions is at 21:00 of the previous
#' day.
#' @param verbose Logical; display additional information
#' @aliases weekly
#' @importFrom sp CRS
#' @importFrom sf st_as_sf st_geometry st_set_geometry st_sf st_crs
#' @format Emissions
#'
#' @author Daniel Schuch & Sergio Ibarra
#' @importFrom data.table wday
#' @return GriddedEmissionsArray, sf or data.frame, depending on the class of
#' EMISSION
#' @export
#' @note This function assumes that the emissions have hours with length of
#' factor of 24, e.g: 24 hours, 24*2 hours etc. Then, it re-order the emissions
#' by the hours of estimations to match another length of emissions. For
#' instance, if the input covers 168 hours and it is desired an object of 241
#' hours that start saturday, this function can do that. It is useful when you
#' are going to start a air quality simulation for specific periods of time.
#' @examples {
#' data(net)
#' data(pc_profile)
#' data(fe2015)
#' data(fkm)
#' PC_G <- c(33491,22340,24818,31808,46458,28574,24856,28972,37818,49050,87923,
#'           133833,138441,142682,171029,151048,115228,98664,126444,101027,
#'           84771,55864,36306,21079,20138,17439, 7854,2215,656,1262,476,512,
#'           1181, 4991, 3711, 5653, 7039, 5839, 4257,3824, 3068)
#' veh <- data.frame(PC_G = PC_G)
#' pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
#' pcw <- temp_fact(net$ldv+net$hdv, pc_profile)
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' pckm <- units::set_units(fkm[[1]](1:24), "km")
#' pckma <- cumsum(pckm)
#' cod1 <- emis_det(po = "CO", cc = 1000, eu = "III", km = pckma[1:11])
#' cod2 <- emis_det(po = "CO", cc = 1000, eu = "I", km = pckma[12:24])
#' #vehicles newer than pre-euro
#' co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
#' cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
#' lef <- ef_ldv_scaled(co1, cod, v = "PC", t = "4S", cc = "<=1400",
#'                      f = "G",p = "CO", eu=co1$Euro_LDV)
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed, agemax = 41,
#'               profile = pc_profile, simplify = TRUE)
#' class(E_CO)
#' E_CO_STREETS <- emis_post(arra = E_CO, pollutant = "CO", by = "streets", net = net)
#' g <- make_grid(net, 1/102.47/2, 1/102.47/2) #500m in degrees
#' E_CO_g <- emis_grid(spobj = E_CO_STREETS, g = g, sr= 31983)
#' head(E_CO_g) #class sf
#' gr <- GriddedEmissionsArray(E_CO_g, rows = 19, cols = 23, times = 168, T)
#' wCO <- emis_order(gr, start = "sat", hours = 24, verbose = TRUE)
#' wCO <- emis_order(E_CO_STREETS, start = as.Date("2016-04-06"), hours = 241, verbose = TRUE)
#' }
#'
emis_order <- function(EMISSION, start = "mon", hours = 168,
                       utc, verbose = TRUE){
  if(verbose){
    message(paste0("Class :",
                   class(EMISSION), '\n'))
  }

  seg <- 1:24
  ter <- 25:48
  qua <- 49:72
  qui <- 73:96
  sex <- 97:120
  sab <- 121:144
  dom <- 145:168

  index <- vector(mode = "numeric",length = hours)

  if(class(start)[1] == "Date"){
    if(verbose)
      cat("\nusing date:", paste(start),"\n")
    s <- data.table::wday(start)
    if(s == 1) start = "mon"
    if(s == 2) start = "tue"
    if(s == 3) start = "wed"
    if(s == 4) start = "thu"
    if(s == 5) start = "fri"
    if(s == 6) start = "sat"
    if(s == 7) start = "sun"
  }
  if(is.character(start)){
    if(start == "mon")
      index <- suppressWarnings(matrix( c(seg,ter,qua,qui,sex,sab,dom),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(start == "tue")
      index <- suppressWarnings(matrix( c(ter,qua,qui,sex,sab,dom,seg),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(start == "wed")
      index <- suppressWarnings(matrix( c(qua,qui,sex,sab,dom,seg,ter),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(start == "thu")
      index <- suppressWarnings(matrix( c(qui,sex,sab,dom,seg,ter,qua),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(start == "fri")
      index <- suppressWarnings(matrix( c(sex,sab,dom,seg,ter,qua,qui),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(start == "sat")
      index <- suppressWarnings(matrix( c(sab,dom,seg,ter,qua,qui,sex),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(start == "sun")
      index <- suppressWarnings(matrix( c(dom,seg,ter,qua,qui,sex,sab),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(!missing(utc)){
      if(utc > 0){
        index <- c(index[abs(-3):length(index)],
                   max(index[abs(-3):length(index)]):(length(index) -1))

      } else if(utc < 0){
        AA <- length(index) + utc
        a <- index[(AA + 1):length(index)]
        b <- index[1:(length(index) + utc)]
        index <- c(a, b)
      }
    }

    if(verbose)
      cat("emissions starting at",start,"with",hours,"hours\n")
  }
  else{
    stop("invalid start argument!\n") # nocov
  }

  if(class(EMISSION)[1] %in% c("EmissionsArray")){
    stop("Please, convert to an object with length of dimensions of 2 or 3, such as 'GriddedEmissionsArray'")
  } else if(class(EMISSION)[1] %in% c("GriddedEmissionsArray", "array")){
    # this lines rearange the GriddedEmissionsArray output to be used in wrf_put
    NEW   <- array(NA, dim = c(dim(EMISSION)[1],dim(EMISSION)[2],hours))
    NEW   <- EMISSION[, , c(index)]
    return(NEW)
  } else if(class(EMISSION)[1] %in% c("SpatialPolygonsDataFrame")){
    sr <- sf::st_crs(sf::st_as_sf(EMISSION))
    id <- 1:nrow(EMISSION)
    EMISSION$id <- NULL
    df <-  sf::st_as_sf(EMISSION)
    geo <- sf::st_geometry(df)
    df <- sf::st_set_geometry(df, NULL)
    dft   <- df[, index]
    dft <- cbind(data.frame(id = id), dft)
    dft <- sf::st_sf(dft, geometry = geo, crs = sr)
    return(dft)
  } else if(class(EMISSION)[1] == c("sf")){
    id <- 1:nrow(EMISSION)
    EMISSION$id <- NULL
    geo <- sf::st_geometry(EMISSION)
    df <- sf::st_set_geometry(EMISSION, NULL)
    dft   <- df[, index]
    dft <- cbind(data.frame(id = id), dft)
    dft <- sf::st_sf(dft, geometry = geo, crs = sf::st_crs(EMISSION))
    return(dft)
  } else if (class(EMISSION)[1] %in% c("matrix")){
    id <- 1:nrow(EMISSION)
    df <- as.data.frame(EMISSION)
    dft   <- df[, index]
    dft <- cbind(data.frame(id = id), dft)
    return(dft)
  } else  if(class(EMISSION)[1] %in% c("data.frame",  "Emissions")){
    id <- 1:nrow(EMISSION)
    EMISSION$id <- NULL
    df <- as.data.frame(EMISSION)
    dft   <- df[, index]
    dft <- cbind(data.frame(id = id), dft)
    return(dft)
  }
}
