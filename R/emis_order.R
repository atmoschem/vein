#' Re-order the emission to match specific hours and days
#'
#' @description Emissions are ususally estimated for a year, 24 hours or one week from monday to
#' sunday (with 168 hours). This depends on the availability of traffic data.
#' When an air quality simulation is going to be done, they cover
#' specific periods of time. For instance, WRF Chem emissions files supports periods of time,
#' or two emissions sets for a representative day (0-12z 12-0z). Also a WRF Chem simulation
#' scan starts a thursday at 00:00 UTC, cover 271 hours of simulations, but hour emissions are in local
#' time and cover only 168 hours starting on monday. This function tries to transform our emissions
#' in local time to the desired utc time, by recycling the local emissions.
#'
#' @param x one of the following:
#' \itemize{
#' \item Spatial object of class "Spatial". Columns are hourly emissions.
#' \item Spatial Object of class "sf". Columns are hourly emissions.
#' \item "data.frame", "matrix" or "Emissions".
#'}
#'
#'In all cases, columns are hourly emissions.
#' @param lt_emissions Local time of the emissions at first hour. It must be
#' the \strong{before}  time of start_utc_time. For instance, if
#' start_utc_time is 2020-02-02 00:00, and your emissions starts monday at 00:00,
#' your lt_emissions must be 2020-01-27 00:00. The argument tz_lt will detect your
#' current local time zone and do the rest for you.
#'
#' @param start_utc_time UTC time for the desired first hour. For instance,
#' the first hour of the namelist.input for WRF.
#' @param desired_length Integer; length to recycle or subset local emissions. For instance, the length
#' of the WRF Chem simulations, states at namelist.input.
#' @param tz_lt Character, Time zone of the local emissions. Default value is derived from
#' Sys.timezone(), however, it accepts any other. If you enter a wrong tz, this function will show
#' you a menu to choose one of the 697 time zones available.
#' @param k Numeric, factor.
#' @param net SpatialLinesDataFrame or Spatial Feature of "LINESTRING".
#' @param verbose Logical, to show more information, default is TRUE.
#' @importFrom sf st_as_sf st_geometry st_set_geometry st_sf st_crs
#' @aliases weekly emis_order
#' @return sf or data.frame
#' @seealso  \code{\link{GriddedEmissionsArray}}
#' @export
#' @examples {
#' #do not run
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
#' wCO <- emis_order(x = E_CO_g,
#'                    lt_emissions = "2020-02-19 00:00",
#'                    start_utc_time = "2020-02-20 00:00",
#'                    desired_length = 241)
#' }
#'
emis_order <- function(x, # 24 hours or one week
                        lt_emissions,
                        start_utc_time,
                        desired_length,
                        tz_lt = Sys.timezone(),
                        k = 1,
                        net,
                        verbose = TRUE) {

  if(as.POSIXct(lt_emissions) >= as.POSIXct(start_utc_time)) {
    stop("lt_emissions must start before start_utc_time")
  }
  if(verbose) cat("Transforming into data.frame\n")

  if(class(x)[1] == "sf"){
    x <- sf::st_set_geometry(x, NULL)
  }
  # x <- remove_units(x)

  x$id <- NULL

  dfx <- data.frame(nx = names(x))

  nx <- ncol(x)


  # check tz
  itz <- intersect(tz_lt, OlsonNames())

  if(length(itz) == 0){
    choice <- utils::menu(OlsonNames(), title="Choose yout time zone")
    tz_lt <- OlsonNames()[choice]
  }
  if(verbose) cat("Your local_tz is: ", tz_lt, "\n")

  # first_hour_lt <- as.POSIXct(x = local_time, tz = "Asia/Tokyo")
  # first_hour_lt <- as.POSIXct(x = local_time, tz = "America/Sao_Paulo")
  first_hour_lt <- as.POSIXct(x = lt_emissions, tz = tz_lt)
  futc <- as.POSIXct(format(lt_emissions, tz = "UTC"), tz = "UTC")

  a <- futc -  first_hour_lt
  if(verbose) cat("Difference with UTC: ", a, "\n")

  # primero ver start_day
  seq_wrf <- seq.POSIXt(from = as.POSIXct(start_utc_time, tz = "UTC"),
                        by = 3600,
                        length.out = desired_length)
  dwrf <- data.frame(seq_wrf = seq_wrf,
                     cutc = as.character(seq_wrf))


  lt <- seq.POSIXt(from = first_hour_lt,
                   by = 3600,
                   #I need that lt be long enoigh so that dwrf$cutc is inside df_x$cutc
                   # Hence, recycling names(x)
                   length.out = nx*desired_length)


  df_x <- data.frame(lt = lt,
                     utc = as.POSIXct(format(lt, tz = "UTC"), tz = "UTC"),
                     cutc = as.character(as.POSIXct(format(lt, tz = "UTC"), tz = "UTC")))
  df_x$nx <- names(x)

  df <- merge(x = dwrf, y = df_x, by = "cutc", all.x = T)
  x <- x[, df$nx] * k

  if(!missing(net)){
    netsf <- sf::st_as_sf(net)
    dfsf <- sf::st_sf(x, geometry = netsf$geometry)
    return(dfsf)
  } else {
    return(x)
  }
}
