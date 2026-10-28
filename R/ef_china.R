#' Emissions factors from Chinese emissions guidelines
#'
#' \code{\link{ef_china}} returns emission factors as vector or data.frames.
#' The emission factors  comes from the chinese emission guidelines (v3) from the
#' Chinese Ministry of Ecology and Environment
#' http://www.mee.gov.cn/gkml/hbb/bgth/201407/W020140708387895271474.pdf
#'
#' @family China
#' @param v Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"
#' @param t Character; sub-category of of vehicle: PV Gasoline:  "Mini", "Small","Medium",
#' "Large", "Taxi", "Motorcycles", "Moped", PV Diesel: "Mediumbus", "Largebus",
#'  "3-Wheel". Trucks: "Mini", "Light" , "Medium", "Heavy"
#' @param standard Character or data.frame; "PRE", "I", "II", "III", "IV", "V", "VI".
#' When it is a data.frame, it each row is a different region and ta, humidity,
#' altitud, speed, sulphur and load_factor lengths have the same as the number of
#' rows (humidity can also have length 1).
#' @param f Character;fuel: "G", "D", "CNG", "G HY", "D HY D". Speed and
#' sulphur corrections of "G" are used for "CNG" and "G HY" and of "D" for
#' "D HY D"; "ALL" is not supported.
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @param k Numeric; multiplication factor
#' @param ta Numeric; temperature of ambient in celcius degrees. When standard is a
#' data.frame, the length must be equal to the number of rows of standard.
#' @param humidity Numeric; relative humidity as fraction (0-1). When standard is a
#' data.frame, the length must be equal to the number of rows of standard.
#' @param altitude Numeric; altitude in meters. When standard is a
#' data.frame, the length must be equal to the number of rows of standard.
#' @param speed Numeric; altitude in km/h When standard is a
#' data.frame, the length must be equal to the number of rows of standard.
#' @param baseyear_det Integer; any of 2015, 2016, 2017, 2018. Deterioration
#' is applied to gasoline CO, HC and NOx (it was ignored when standard was a
#' character vector in earlier versions).
#' @param sulphur Numeric; sulphur in ppm. When standard is a
#' data.frame, the length must be equal to the number of rows of standard.
#' @param load_factor Numeric; When standard is a
#' data.frame, the length must be equal to the number of rows of standard.
#' @param details Logical; When TRUE, it shows a description of the vehicle in chinese
#' and english. Only when length standard is 1.
#' @param correction_only Logical; When TRUE, return only correction factors.
#' @return An emission factor
#' @keywords ef_china emission factors China
#' @note Combination of vehicles:
#' \tabular{ccc}{
#'   v    \tab t      \tab    f  \cr
#'   PV   \tab Mini   \tab   G HY   \cr
#'   PV   \tab Bus   \tab   D HY D   \cr
#'   PV   \tab Mini   \tab   CNG   \cr
#'   PV   \tab Bus   \tab   CNG   \cr
#'   PV   \tab Mini   \tab   G   \cr
#'   PV   \tab Small   \tab   G   \cr
#'   PV   \tab Medium   \tab   G   \cr
#'   PV   \tab Large   \tab   G   \cr
#'   PV   \tab Taxi   \tab   G   \cr
#'   PV   \tab Bus   \tab   G   \cr
#'   PV   \tab Motorcycles   \tab   G   \cr
#'   PV   \tab Moped   \tab   G   \cr
#'   PV   \tab Mini   \tab   D   \cr
#'   PV   \tab Small   \tab   D   \cr
#'   PV   \tab Mediumbus   \tab   D   \cr
#'   PV   \tab Medium   \tab   D   \cr
#'   PV   \tab Largebus   \tab   D   \cr
#'   PV   \tab Bus   \tab   D   \cr
#'   PV   \tab 3-Wheel   \tab   D   \cr
#'   PV   \tab Small   \tab   ALL   \cr
#'   PV   \tab Mediumbus   \tab   ALL   \cr
#'   PV   \tab Largebus   \tab   ALL   \cr
#'   PV   \tab Taxi   \tab   ALL   \cr
#'   PV   \tab Bus   \tab   ALL   \cr
#'   Trucks   \tab Bus   \tab   G   \cr
#'   Trucks   \tab Light   \tab   G   \cr
#'   Trucks   \tab Medium   \tab   G   \cr
#'   Trucks   \tab Heavy   \tab   G   \cr
#'   Trucks   \tab Light   \tab   D   \cr
#'   Trucks   \tab Medium   \tab   D   \cr
#'   Trucks   \tab Heavy   \tab   D   \cr
#'   Trucks   \tab Low Speed   \tab   D   \cr
#'   Trucks   \tab Mini   \tab   D   \cr
#' }
#'
#' standard VI is assumed as V for the base emission factor and deterioration.
#'
#' load_factor classes: 0, (0, 0.5), 0.5, (0.5, 0.6], (0.6, 0.75] and > 0.75
#' (earlier versions compared sulphur instead of load_factor for the classes
#' above 0.5). Speeds of 80 km/h or more use the S80 correction (80 km/h
#' returned 0 in earlier versions).
#'
#' @seealso \code{\link{ef_ldv_speed}} \code{\link{emis_hot_td}}
#' @importFrom units as_units
#' @export
#' @examples \dontrun{
#' # when standard is 'character'
#' # Checking
#' df_st <- rev(c(as.character(as.roman(5:1)), "PRE"))
#' ef_china(t = "Mini", f = "G", standard = df_st, p = "CO")
#' ef_china(t = "Mini", f = "G", standard = df_st, p = "HC")
#' ef_china(t = "Mini", f = "G", standard = df_st, p = "NOx")
#' ef_china(t = "Mini", f = "G", standard = df_st, p = "PM2.5")
#' ef_china(t = "Mini", f = "G", standard = df_st, p = "PM10")
#'
#' ef_china(t = "Small", f = "G", standard = df_st, p = "CO")
#' ef_china(t = "Small", f = "G", standard = df_st, p = "HC")
#' ef_china(t = "Small", f = "G", standard = df_st, p = "NOx")
#' ef_china(t = "Small", f = "G", standard = df_st, p = "PM2.5")
#' ef_china(t = "Small", f = "G", standard = df_st, p = "PM10")
#'
#'
#' ef_china(t = "Mini",
#'         standard = c("PRE"),
#'         p = "CO",
#'         k = 1,
#'         ta = celsius(15),
#'         humidity = 0.5,
#'         altitude = 1000,
#'         speed = Speed(30),
#'         baseyear_det = 2014,
#'         sulphur = 50,
#'         load_factor = 0.5,
#'         details = FALSE)
#' ef_china(standard = c("PRE", "I"), p = "CO", correction_only = TRUE)
#'
#' # when standard is 'data.frame'
#' df_st <- matrix(c("V", "IV", "III", "III", "II", "I", "PRE"), nrow = 2, ncol = 7, byrow = TRUE)
#' df_st <- as.data.frame(df_st)
#' a <- ef_china(standard = df_st,
#'               p = "PM10",
#'               ta = rep(celsius(15), 2),
#'               altitude = rep(1000, 2),
#'               speed = rep(Speed(30), 2),
#'               sulphur = rep(50, 2))
#' dim(a)
#' dim(df_st)
#' ef_china(standard = df_st, p = "PM2.5", ta = rep(celsius(20), 2),
#' altitude = rep(1501, 2), speed = rep(Speed(29), 2), sulphur = rep(50, 2))
#' a
#'
#' # when standard, temperature and humidity are data.frames
#' # assuming 10 regions
#' df_st <- matrix(c("V", "IV", "III", "III", "II", "I", "PRE"), nrow = 10, ncol = 7, byrow = TRUE)
#' df_st <- as.data.frame(df_st)
#' df_t <- matrix(21:30, nrow = 10, ncol = 12, byrow = TRUE)
#' df_t <- as.data.frame(df_t)
#' for(i in 1:12) df_t[, i] <- celsius(df_t[, i])
#'
#' # assuming 10 regions
#' df_h <- matrix(seq(0.4, 0.5, 0.05), nrow = 10, ncol = 12, byrow = TRUE)
#' df_h <- as.data.frame(df_h)
#' a <- ef_china(standard = df_st, p = "CO", ta = df_t, humidity = df_h,
#' altitude = rep(1501, 10), speed = rep(Speed(29), 10), sulphur = rep(50, 10))
#' a
#' a <- ef_china(standard = df_st, p = "PM2.5", ta = df_t, humidity = df_h,
#' altitude = rep(1501, 10), speed = rep(Speed(29), 10), sulphur = rep(50, 10))
#' a
#' a <- ef_china(standard = df_st, p = "PM10", ta = df_t, humidity = df_h,
#' altitude = rep(1501, 10), speed = rep(Speed(29), 10), sulphur = rep(50, 10))
#' a
#' dim(a)
#' }
ef_china <- function(v = "PV",
                     t = "Small",
                     f = "G",
                     standard,
                     p,
                     k = 1,
                     ta = celsius(15),
                     humidity = 0.5,
                     altitude = 1000,
                     speed = Speed(30),
                     baseyear_det = 2016,
                     sulphur = 50,
                     load_factor = 0.5,
                     details = FALSE,
                     correction_only = FALSE){
  ef_china <- sysdata$ef_china
  det_china <- sysdata$det_china
  sulphur_china <- sysdata$sulphur_china
  speed_china <- sysdata$speed_china
  ev <- ef_china[ef_china$POLLUTANT %in% c("Evaporative_driving",
                                           "Evaporative_parking"), ]
  ef_china <- ef_china[!ef_china$POLLUTANT %in% c("Evaporative_driving",
                                                  "Evaporative_parking"), ]
  fl <- data.frame(stringsAsFactors=FALSE,
                   POLLUTANT = c("CO", "HC", "NOx", "PM2.5", "PM10"),
                   L0 = c(0.87, 1, 0.83, 0.9, 0.9),
                   L50 = c(1, 1, 1, 1, 1),
                   L60 = c(1.07, 1, 1.09, 1.05, 1.05),
                   L75 = c(1.16, 1, 1.21, 1.13, 1.13),
                   L100 = c(1.33, 1, 1.43, 1.26, 1.26)
  )

  #Check standard
  if(is.matrix(standard) | is.data.frame(standard)){
    eu <- as.data.frame(standard)
    for(i in 1:ncol(standard)) standard[, i] <- as.character(standard[, i])
  } else {
    standard = as.character(standard)
  }

  # Check speed
  if(!inherits(speed, "units")){
    stop("speed neeeds to has class 'units' in 'km/h'. Please, check package '?units::set_units'")
  }
  if(units(speed) != units(units::as_units("km/h"))){
    stop("Units of speed must be 'km/h' ")
  }
  if(units(speed) == units(units::as_units("km/h"))){
    speed <- as.numeric(speed)
  }
  #Check k
  if(length(k) > 1) stop("Length of k must be 1")
  #Check arguments
  if(!v %in% unique(ef_china$VEH)) {
    cat("'v' must be one of:", unique(ef_china$VEH), "\n")
    stop("")
  }
  if(!t %in% unique(ef_china$TYPE)) {
    cat("'t' must be one of:", unique(ef_china$TYPE), "\n")
    stop("")
  }
  if(!f %in% unique(ef_china$FUEL)) {
    cat("'f' must be one of:", unique(ef_china$FUEL), "\n")
    stop("")
  }
  if(!p %in% unique(ef_china$POLLUTANT)) {
    cat("'p' must be one of:", unique(ef_china$POLLUTANT), "\n")
    stop("")
  }
  # fuel of the speed and sulphur correction tables
  if (f %in% c("G", "G HY", "CNG")) {
    fcor <- "G"
  } else if (f %in% c("D", "D HY D")) {
    fcor <- "D"
  } else {
    stop("speed and sulphur corrections are not available for f = '", f,
         "', use 'G' or 'D'")
  }
  # standard VI is assumed as V for the base EF and deterioration
  std_ef <- function(s) ifelse(s == "VI", "V", s)
  get_ef <- function(s) {
    df <- ef_china[ef_china$VEH == v &
                     ef_china$TYPE == t &
                     ef_china$FUEL == f &
                     ef_china$STANDARD == std_ef(s) &
                     ef_china$POLLUTANT == p, ]
    if(nrow(df) != 1) stop("No emission factor for v = '", v, "', t = '", t,
                           "', f = '", f, "', standard = '", s,
                           "' and p = '", p, "'")
    df
  }
  get_det <- function(s) {
    ts <- ifelse(t %in% c("Mini", "Small"),"group1",
                 ifelse(t == "Taxi", "Taxi","group2"))
    detfac <- det_china[det_china$TYPE == ts &
                          det_china$FUEL == f &
                          det_china$STANDARD == std_ef(s) &
                          det_china$POLLUTANT == p &
                          det_china$YEAR == baseyear_det, ]
    if(nrow(detfac) != 1) stop("No deterioration factor for standard '", s,
                               "' and baseyear_det = ", baseyear_det,
                               ". baseyear_det must be one of: ",
                               paste(sort(unique(det_china$YEAR[det_china$STANDARD == "V"])),
                                     collapse = ", "))
    detfac$DET
  }
  load_cor <- function(EF, dfl) {
    ifelse(
      load_factor == 0, EF*dfl$L0,
      ifelse(
        load_factor > 0 & load_factor < 0.5, EF*dfl$L50,
        ifelse(load_factor == 0.5, EF,
               ifelse(
                 load_factor > 0.5 & load_factor <= 0.6, EF*dfl$L60,
                 ifelse(
                   load_factor > 0.6 & load_factor <= 0.75, EF*dfl$L75,
                   EF*dfl$L100)))))
  }


  # fun starts ####
  if(!is.data.frame(standard) & !is.matrix(standard) & !is.data.frame(ta)){
    dff <- lapply(1:length(standard), function(i){
      df <- get_ef(standard[i])
      # details
      if(details){
        cat("English: ", df$Description)
        cat("\n\n")
        cat("Chinese: ", df$CHN)
        cat("\n")
      }

      # Check to return only correction
      if(correction_only) df$EF <- 1

      if(!inherits(ta, "units")) stop("ta must be units in celsius, use celsius(ta)")
      ta <- as.numeric(ta)

      # Check correction gasoline - ta
      if(f == "G"){
        if(t != "Motorcycles"){
          if(p == "CO"){
            df$EF <- ifelse(ta < 10, df$EF*1.36,
                            ifelse(ta > 25, df$EF*1.23, df$EF))
          } else if(p == "HC"){
            df$EF <- ifelse(ta < 10, df$EF*1.47,
                            ifelse(ta > 25, df$EF*1.08, df$EF))
          } else if(p == "NOx"){
            df$EF <- ifelse(ta < 10, df$EF*1.15,
                            ifelse(ta > 25, df$EF*1.31, df$EF))
          }}
      }
      # Check correction diesel - ta
      if(f == "D"){
        if(p == "CO"){
          if(t %in% c("Small", "Light")){
            df$EF <- ifelse(ta > 25, df$EF*1.33, df$EF)
          } else if(!t %in% c("Small", "Light")){
            df$EF <- ifelse(ta > 25, df$EF*1.3, df$EF)
          }
        } else if(p == "HC"){
          if(t == c("Small")){
            df$EF <- ifelse(ta > 25, df$EF*1.07, df$EF)
          } else if(!t == c("Light")){
            df$EF <- ifelse(ta > 25, df$EF*1.06, df$EF)
          }
        } else if(p == "NOx"){
          if(t == c("Small")){
            df$EF <- ifelse(ta > 25, df$EF*1.17,
                            ifelse(ta < 10, df$EF*1.06, df$EF))
          } else if(t == c("Light")){
            df$EF <- ifelse(ta > 25, df$EF*1.17,
                            ifelse(ta < 10, df$EF*1.05, df$EF))
          } else {
            df$EF <- ifelse(ta > 25, df$EF*1.15,
                            ifelse(ta < 10, df$EF*1.06, df$EF))
          }
        } else if(p %in% c("PM2.5", "PM10")){
          if(t %in% c("Small", "Bus")){
            df$EF <- ifelse(ta > 25, df$EF*0.68,
                            ifelse(ta < 10, df$EF*1.87, df$EF))
          } else if(t == c("Light")){
            df$EF <- ifelse(ta > 25, df$EF*0.9,
                            ifelse(ta < 10, df$EF*1.27, df$EF))
          } else {
            df$EF <- ifelse(ta > 25, df$EF*0.74,
                            ifelse(ta < 10, df$EF*1.7, df$EF))
          }
        }
      }
      # Check correction NOx - humidity
      if(p == "NOx" & f == "G"){
        df$EF <- ifelse(humidity <0.5, df$EF*1.06,
                        ifelse(
                          humidity > 0.5, df$EF*0.92,
                          df$EF))
      } else if(p == "NOx" & f == "D"){
        df$EF <- ifelse(humidity < 0.5, df$EF*1.04,
                        ifelse(
                          humidity > 0.5, df$EF*0.94,
                          df$EF))
      }
      # Check correction ta and humidity - gasoline
      if(ta > 24){
        if(f == "G"){
          if(p == "CO"){
            df$EF <- ifelse(
              humidity > 0.5, df$EF*1.04,
              ifelse(humidity < 0.5, df$EF*0.97, df$EF))
          } else if (p == "HC"){
            df$EF <- ifelse(
              humidity > 0.5, df$EF*1.01,
              ifelse(humidity < 0.5, df$EF*0.99, df$EF))
          } else if(p == "NOx"){
            df$EF <- ifelse(humidity > 0.5, df$EF*0.87,
                            ifelse(humidity < 0.5, df$EF*1.13, df$EF))
          }
        } else if(f == "D"){
          if(p == "NOx"){
            df$EF <- ifelse(humidity > 0.5, df$EF*0.88,
                            ifelse(humidity < 0.5, df$EF*1.12, df$EF))
          }
        }
      }
      # Check altitude
      if(altitude > 1500){
        if(f == "G"){
          if(t %in% c("Mini", "Small", "Light", "Taxi", "Motorcycles",
                      "Moped")){
            df$EF <- ifelse(p == "CO", df$EF*1.58,
                            ifelse(p == "HC", df$EF*2.46,
                                   ifelse(p == "NOx", df$EF*3.15, df$EF)))
          } else if(!t %in% c("Mini", "Small", "Light", "Taxi", "Motorcycles",
                              "Moped")){
            df$EF <- ifelse(p == "CO", df$EF*3.95,
                            ifelse(p == "HC", df$EF*2.26,
                                   ifelse(p == "NOx", df$EF*0.88, df$EF)))
          }
        } else if(f == "D"){
          if(t %in% c("Small", "Light")){
            df$EF <- ifelse(p == "CO", df$EF*1.2,
                            ifelse(p == "HC", df$EF*1.32,
                                   ifelse(p == "NOx", df$EF*1.35, df$EF*1.35)))
          } else if(!t %in% c("Small", "Light")){
            df$EF <- ifelse(p == "CO", df$EF*2.46,
                            ifelse(p == "HC", df$EF*2.05,
                                   ifelse(p == "NOx", df$EF*1.02, df$EF)))
          }}
      }
      # Check speed
      efspeed <- speed_china[speed_china$FUEL == fcor &
                               speed_china$STANDARD == standard[i] &
                               speed_china$POLLUTANT == p, ]
      df$EF <- ifelse(
        speed < 20, df$EF*efspeed$S20,
        ifelse(
          speed >= 20 & speed < 30, df$EF*efspeed$S20_30,
          ifelse(
            speed == 30 , df$EF,
            ifelse(
              speed > 30 & speed < 40, df$EF*efspeed$S30_40,
              ifelse(
                speed >= 40 & speed < 80, df$EF*efspeed$S40_80,
                df$EF*efspeed$S80)))))
      # Check deterioration
      if(f == "G" & p %in% c("CO", "HC", "NOx")){
        df$EF <- df$EF*get_det(standard[i])
      }
      # Check sulphur
      sulphur_china <- sulphur_china[sulphur_china$FUEL == fcor &
                                       sulphur_china$STANDARD == standard[i] &
                                       sulphur_china$POLLUTANT == p, ]
      df$EF <- ifelse(
        sulphur <= 10, df$EF*sulphur_china$S10,
        ifelse(
          sulphur > 10 & sulphur <= 50, df$EF*sulphur_china$S50,
          ifelse(
            sulphur > 50 & sulphur <= 150, df$EF*sulphur_china$S150,
            ifelse(sulphur > 150 & sulphur <= 350, df$EF*sulphur_china$S350,
                   ifelse(
                     sulphur > 350 & sulphur <= 500, df$EF*sulphur_china$S500,
                     df$EF*sulphur_china$S800)))))
      # Check load
      dfl <- fl[fl$POLLUTANT == p, ]
      df$EF <- load_cor(df$EF, dfl)
    })

    if(correction_only){
      return(unlist(dff)*k)
    } else {
      return(EmissionFactors(unlist(dff)*k))
    }

    # standard is data.frames ####
  } else if((is.matrix(standard) | is.data.frame(standard)) & !is.data.frame(ta)) {
    standard <- as.data.frame(standard)
    if(length(humidity) == 1) humidity <- rep(humidity, nrow(standard))
    if(length(humidity) != nrow(standard)) stop("length of 'humidity' must be the same as the number of rows of 'standard'")

    dff <- do.call("rbind", lapply(1:nrow(standard), function(j){
      do.call("cbind", lapply(1:ncol(standard), function(i){
        df <- get_ef(standard[j,i][[1]])
        # Check to return only correction
        if(correction_only) df$EF <- 1

        if(!inherits(ta, "units")) stop("ta must be units in celsius, use celsius(ta)")
        ta <- as.numeric(ta)

        if(length(ta) != nrow(standard)) stop("length of 'ta' must be the same as the number of rows of 'standard'")

        # Check correction gasoline - ta
        if(f == "G"){
          if(t != "Motorcycles"){
            if(p == "CO"){
              df$EF <- ifelse(ta[j] < 10, df$EF*1.36,
                              ifelse(ta[j] > 25, df$EF*1.23, df$EF))
            } else if(p == "HC"){
              df$EF <- ifelse(ta[j] < 10, df$EF*1.47,
                              ifelse(ta[j] > 25, df$EF*1.08, df$EF))
            } else if(p == "NOx"){
              df$EF <- ifelse(ta[j] < 10, df$EF*1.15,
                              ifelse(ta[j] > 25, df$EF*1.31, df$EF))
            }
          }}

        # Check correction diesel - ta
        if(f == "D"){
          if(p == "CO"){
            if(t %in% c("Small", "Light")){
              df$EF <- ifelse(ta[j] > 25, df$EF*1.33, df$EF)
            } else if(!t %in% c("Small", "Light")){
              df$EF <- ifelse(ta[j] > 25, df$EF*1.3, df$EF)
            }
          } else if(p == "HC"){
            if(t == c("Small")){
              df$EF <- ifelse(ta[j] > 25, df$EF*1.07, df$EF)
            } else if(!t == c("Light")){
              df$EF <- ifelse(ta[j] > 25, df$EF*1.06, df$EF)
            }
          } else if(p == "NOx"){
            if(t == c("Small")){
              df$EF <- ifelse(ta[j] > 25, df$EF*1.17,
                              ifelse(ta[j] < 10, df$EF*1.06, df$EF))
            } else if(t == c("Light")){
              df$EF <- ifelse(ta[j] > 25, df$EF*1.17,
                              ifelse(ta[j] < 10, df$EF*1.05, df$EF))
            } else {
              df$EF <- ifelse(ta[j] > 25, df$EF*1.15,
                              ifelse(ta[j] < 10, df$EF*1.06, df$EF))
            }
          } else if(p %in% c("PM2.5", "PM10")){
            if(t %in% c("Small", "Bus")){
              df$EF <- ifelse(ta[j] > 25, df$EF*0.68,
                              ifelse(ta[j] < 10, df$EF*1.87, df$EF))
            } else if(t == c("Light")){
              df$EF <- ifelse(ta[j] > 25, df$EF*0.9,
                              ifelse(ta[j] < 10, df$EF*1.27, df$EF))
            } else {
              df$EF <- ifelse(ta[j] > 25, df$EF*0.74,
                              ifelse(ta[j] < 10, df$EF*1.7, df$EF))
            }
          }
        }
        # Check correction NOx - humidity
        if(p == "NOx" & f == "G"){
          df$EF <- ifelse(humidity[j] > 0.5, df$EF*0.92,
                          ifelse(humidity[j] < 0.5, df$EF*1.06, df$EF))
        } else if(p == "NOx" & f == "D"){
          df$EF <- ifelse(humidity[j] > 0.5, df$EF*0.94,
                          ifelse(humidity[j] < 0.5, df$EF*1.04, df$EF))
        } else {
          df$EF <- df$EF
        }
        # Check correction ta[j] and humidity - gasoline
        if(ta[j] > 24){
          if(f == "G"){
            if(p == "CO"){
              df$EF <- ifelse(humidity[j] > 0.5, df$EF*1.04,
                              ifelse(humidity[j] < 0.5, df$EF*0.97, df$EF))
            } else if (p == "HC"){
              df$EF <- ifelse(humidity[j] > 0.5, df$EF*1.01,
                              ifelse(humidity[j] < 0.5, df$EF*0.99, df$EF))
            } else if(p == "NOx"){
              df$EF <- ifelse(humidity[j] > 0.5, df$EF*0.87,
                              ifelse(humidity[j] < 0.5, df$EF*1.13, df$EF))
            }
          } else if(f == "D"){
            if(p == "NOx"){
              df$EF <- ifelse(humidity[j] > 0.5, df$EF*0.88,
                              ifelse(humidity[j] < 0.5, df$EF*1.12, df$EF))
            }
          }
        }

        # Check altitude
        if(length(altitude) != nrow(standard)) stop("length of 'altitude' must be the same as the number of rows of 'standard'")
        if(altitude[j] > 1500){
          if(f == "G"){
            if(t %in% c("Mini", "Small", "Light", "Taxi", "Motorcycles",
                        "Moped")){
              df$EF <- ifelse(p == "CO", df$EF*1.58,
                              ifelse(p == "HC", df$EF*2.46,
                                     ifelse(p == "NOx", df$EF*3.15, df$EF)))
            } else if(!t %in% c("Mini", "Small", "Light", "Taxi", "Motorcycles",
                                "Moped")){
              df$EF <- ifelse(p == "CO", df$EF*3.95,
                              ifelse(p == "HC", df$EF*2.26,
                                     ifelse(p == "NOx", df$EF*0.88, df$EF)))
            }
          } else if(f == "D"){
            if(t %in% c("Small", "Light")){
              df$EF <- ifelse(p == "CO", df$EF*1.2,
                              ifelse(p == "HC", df$EF*1.32,
                                     ifelse(p == "NOx", df$EF*1.35, df$EF*1.35)))
            } else if(!t %in% c("Small", "Light")){
              df$EF <- ifelse(p == "CO", df$EF*2.46,
                              ifelse(p == "HC", df$EF*2.05,
                                     ifelse(p == "NOx", df$EF*1.02, df$EF)))
            }}
        } else {
          df$EF <- df$EF
        }

        # Check speed
        if(length(speed) != nrow(standard)) stop("length of 'speed' must be the same as the number of rows of 'standard'")
        efspeed <- speed_china[speed_china$FUEL == fcor &
                                 speed_china$STANDARD == standard[j,i][[1]] &
                                 speed_china$POLLUTANT == p, ]
        df$EF <- ifelse(
          speed[j] < 20, df$EF*efspeed$S20,
          ifelse(
            speed[j] >= 20 & speed[j] < 30, df$EF*efspeed$S20_30,
            ifelse(
              speed[j] == 30 , df$EF,
              ifelse(
                speed[j] > 30 & speed[j] < 40, df$EF*efspeed$S30_40,
                ifelse(
                  speed[j] >= 40 & speed[j] < 80, df$EF*efspeed$S40_80, df$EF*efspeed$S80)))))

        # Check deterioration
        if(f == "G" & p %in% c("CO", "HC", "NOx")){
          df$EF <- df$EF*get_det(standard[j,i][[1]])
        }
        # Check sulphur
        if(length(sulphur) != nrow(standard)) stop("length of 'sulphur' must be the same as the number of rows of 'standard'")
        sulphur_china <- sulphur_china[sulphur_china$FUEL == fcor &
                                         sulphur_china$STANDARD == standard[j,i][[1]] &
                                         sulphur_china$POLLUTANT == p, ]
        df$EF <- ifelse(
          sulphur[j] <= 10, df$EF*sulphur_china$S10,
          ifelse(
            sulphur[j] > 10 & sulphur[j] <= 50, df$EF*sulphur_china$S50,
            ifelse(
              sulphur[j] > 50 & sulphur[j] <= 150, df$EF*sulphur_china$S150,
              ifelse(sulphur[j] > 150 & sulphur[j] <= 350, df$EF*sulphur_china$S350,
                     ifelse(
                       sulphur[j] > 350 & sulphur[j] <= 500, df$EF*sulphur_china$S500,
                       df$EF*sulphur_china$S800)))))

        # Check load
        dfl <- fl[fl$POLLUTANT == p, ]
        df$EF <- load_cor(df$EF, dfl)

        df$EF
      }))
    }))
    if(correction_only){
      return(dff)
    } else {
      dff <- EmissionFactors(dff*k)
      dff$speed <- Speed(speed)
      dff$ta <- ta
      dff$humidity <- humidity
      dff$alt <- altitude
      dff$sulphur <- sulphur
      return(dff)
    }
    # standard and ta are data.frames ####
  } else if ((is.matrix(standard) | is.data.frame(standard)) & is.data.frame(ta)){
    standard <- as.data.frame(standard)

    if(ncol(ta) != 12) warning("This function was designed so that number of columns of ta is 12, one year")
    if(nrow(ta) != nrow(standard)) {
      stop("number of rows of 'ta' must be the same as the number of rows of 'standard'")
    }
    if(!is.data.frame(humidity)) {
      if(length(humidity) != 1) stop("when 'ta' is a data.frame, 'humidity' must be a data.frame with the same dimensions or length 1")
      humidity <- as.data.frame(matrix(humidity, nrow = nrow(ta), ncol = ncol(ta)))
    }
    dff <- do.call("rbind", lapply(1:ncol(ta), function(k){
      do.call("rbind", lapply(1:nrow(standard), function(j){
        do.call("cbind", lapply(1:ncol(standard), function(i){
          df <- get_ef(standard[j,i][[1]])
          # Check to return only correction
          if(correction_only) df$EF <- 1
          # Check correction gasoline - ta
          ta <- ta[j, k]
          if(!inherits(ta, "units")) stop("ta must be units in celsius, use celsius(ta)")
          ta <- as.numeric(ta)

          humidity <- humidity[j, k]

          if(f == "G"){
            if(t != "Motorcycles"){
              if(p == "CO"){
                df$EF <- ifelse(ta < 10, df$EF*1.36,
                                ifelse(ta > 25, df$EF*1.23, df$EF))
              } else if(p == "HC"){
                df$EF <- ifelse(ta < 10, df$EF*1.47,
                                ifelse(ta > 25, df$EF*1.08, df$EF))
              } else if(p == "NOx"){
                df$EF <- ifelse(ta < 10, df$EF*1.15,
                                ifelse(ta > 25, df$EF*1.31, df$EF))
              } else {
                df$EF <- df$EF
              }
            }}
          # Check correction diesel - ta
          if(f == "D"){
            if(p == "CO"){
              if(t %in% c("Small", "Light")){
                df$EF <- ifelse(ta > 25, df$EF*1.33, df$EF)
              } else if(!t %in% c("Small", "Light")){
                df$EF <- ifelse(ta > 25, df$EF*1.3, df$EF)
              }
            } else if(p == "HC"){
              if(t == c("Small")){
                df$EF <- ifelse(ta > 25, df$EF*1.07, df$EF)
              } else if(!t == c("Light")){
                df$EF <- ifelse(ta > 25, df$EF*1.06, df$EF)
              }
            } else if(p == "NOx"){
              if(t == c("Small")){
                df$EF <- ifelse(ta > 25, df$EF*1.17,
                                ifelse(ta < 10, df$EF*1.06, df$EF))
              } else if(t == c("Light")){
                df$EF <- ifelse(ta > 25, df$EF*1.17,
                                ifelse(ta < 10, df$EF*1.05, df$EF))
              } else {
                df$EF <- ifelse(ta > 25, df$EF*1.15,
                                ifelse(ta < 10, df$EF*1.06, df$EF))
              }
            } else if(p %in% c("PM2.5", "PM10")){
              if(t %in% c("Small", "Bus")){
                df$EF <- ifelse(ta > 25, df$EF*0.68,
                                ifelse(ta < 10, df$EF*1.87, df$EF))
              } else if(t == c("Light")){
                df$EF <- ifelse(ta > 25, df$EF*0.9,
                                ifelse(ta < 10, df$EF*1.27, df$EF))
              } else {
                df$EF <- ifelse(ta > 25, df$EF*0.74,
                                ifelse(ta < 10, df$EF*1.7, df$EF))
              }
            }
          }
          # Check correction NOx - humidity
          if(p == "NOx" & f == "G"){
            df$EF <- ifelse(humidity > 0.5, df$EF*0.92,
                            ifelse(humidity < 0.5, df$EF*1.06, df$EF))
          } else if(p == "NOx" & f == "D"){
            df$EF <- ifelse(humidity > 0.5, df$EF*0.94,
                            ifelse(humidity < 0.5, df$EF*1.04, df$EF))
          } else {
            df$EF <- df$EF
          }
          # Check correction ta and humidity - gasoline
          if(ta > 24){
            if(f == "G"){
              if(p == "CO"){
                df$EF <- ifelse(humidity > 0.5, df$EF*1.04,
                                ifelse(humidity < 0.5, df$EF*0.97, df$EF))
              } else if (p == "HC"){
                df$EF <- ifelse(humidity > 0.5, df$EF*1.01,
                                ifelse(humidity < 0.5, df$EF*0.99, df$EF))
              } else if(p == "NOx"){
                df$EF <- ifelse(humidity > 0.5, df$EF*0.87,
                                ifelse(humidity < 0.5, df$EF*1.13, df$EF))
              }
            } else if(f == "D"){
              if(p == "NOx"){
                df$EF <- ifelse(humidity > 0.5, df$EF*0.88,
                                ifelse(humidity < 0.5, df$EF*1.12, df$EF))
              }
            }
          }
          # Check altitude
          if(length(altitude) != nrow(standard)) stop("length of 'altitude' must be the same as the number of rows of 'standard'")
          if(altitude[j] > 1500){
            if(f == "G"){
              if(t %in% c("Mini", "Small", "Light", "Taxi", "Motorcycles",
                          "Moped")){
                df$EF <- ifelse(p == "CO", df$EF*1.58,
                                ifelse(p == "HC", df$EF*2.46,
                                       ifelse(p == "NOx", df$EF*3.15, df$EF)))
              } else if(!t %in% c("Mini", "Small", "Light", "Taxi", "Motorcycles",
                                  "Moped")){
                df$EF <- ifelse(p == "CO", df$EF*3.95,
                                ifelse(p == "HC", df$EF*2.26,
                                       ifelse(p == "NOx", df$EF*0.88, df$EF)))
              }
            } else if(f == "D"){
              if(t %in% c("Small", "Light")){
                df$EF <- ifelse(p == "CO", df$EF*1.2,
                                ifelse(p == "HC", df$EF*1.32,
                                       ifelse(p == "NOx", df$EF*1.35, df$EF*1.35)))
              } else if(!t %in% c("Small", "Light")){
                df$EF <- ifelse(p == "CO", df$EF*2.46,
                                ifelse(p == "HC", df$EF*2.05,
                                       ifelse(p == "NOx", df$EF*1.02, df$EF)))
              }}
          } else {
            df$EF <- df$EF
          }
          # Check speed
          if(length(speed) != nrow(standard)) stop("length of 'speed' must be the same as the number of rows of 'standard'")
          efspeed <- speed_china[speed_china$FUEL == fcor &
                                   speed_china$STANDARD == standard[j,i][[1]] &
                                   speed_china$POLLUTANT == p, ]
          df$EF <- ifelse(
            speed[j] < 20, df$EF*efspeed$S20,
            ifelse(
              speed[j] >= 20 & speed[j] < 30, df$EF*efspeed$S20_30,
              ifelse(
                speed[j] == 30 , df$EF,
                ifelse(
                  speed[j] > 30 & speed[j] < 40, df$EF*efspeed$S30_40,
                  ifelse(
                    speed[j] >= 40 & speed[j] < 80, df$EF*efspeed$S40_80, df$EF*efspeed$S80)))))

          # Check deterioration
          if(f == "G" & p %in% c("CO", "HC", "NOx")){
            df$EF <- df$EF*get_det(standard[j,i][[1]])
          }
          # Check sulphur
          if(length(sulphur) != nrow(standard)) stop("length of 'sulphur' must be the same as the number of rows of 'standard'")
          sulphur_china <- sulphur_china[sulphur_china$FUEL == fcor &
                                           sulphur_china$STANDARD == standard[j,i][[1]] &
                                           sulphur_china$POLLUTANT == p, ]
          df$EF <- ifelse(
            sulphur[j] <= 10, df$EF*sulphur_china$S10,
            ifelse(
              sulphur[j] > 10 & sulphur[j] <= 50, df$EF*sulphur_china$S50,
              ifelse(
                sulphur[j] > 50 & sulphur[j] <= 150, df$EF*sulphur_china$S150,
                ifelse(sulphur[j] > 150 & sulphur[j] <= 350, df$EF*sulphur_china$S350,
                       ifelse(
                         sulphur[j] > 350 & sulphur[j] <= 500, df$EF*sulphur_china$S500,
                         df$EF*sulphur_china$S800)))))
          # Check load
          dfl <- fl[fl$POLLUTANT == p, ]
          df$EF <- load_cor(df$EF, dfl)
          df$EF
        }))
      }))
    }))
    if(correction_only){
      return(dff)
    } else {
      dff <- EmissionFactors(dff*k)
      dff$speed <- Speed(speed)
      dff$ta <- celsius(unlist(ta))
      dff$humidity <- unlist(humidity)
      dff$alt <- altitude
      dff$sulphur <- sulphur
      return(dff)
    }
  } else {
    stop("when 'ta' is a data.frame, 'standard' must be a data.frame or matrix")
  }
}

#' @title Chinese emission factors by emissions standard
#' @family China
#' @name ef_china_long
#' @description Chinese emission factors in long format
#' @param v Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"
#' @param t Character; sub-category of of vehicle: PV Gasoline:  "Mini", "Small","Medium",
#' "Large", "Taxi", "Motorcycles", "Moped", PV Diesel: "Mediumbus", "Largebus",
#'  "3-Wheel". Trucks: "Mini", "Light" , "Medium", "Heavy"
#' @param standard Character vector; "PRE", "I", "II", "III", "IV", "V", "VI".
#' Standard "VI" is assumed as "V".
#' @param f Character;fuel: "G", "D", "CNG", "ALL"
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @return EmissionFactors with one value per standard
#' @importFrom data.table as.data.table
#' @export
#' @examples {
#' ef_china_long(standard = "I", p = "CO")
#' }
ef_china_long <- function(v = "PV",
                          t = "Small",
                          f = "G",
                          standard,
                          p){
  chi <- data.table::as.data.table(sysdata$ef_china)

  VEH <- TYPE <- FUEL <- POLLUTANT <- YEAR <- STANDARD <- NULL

  chi[VEH == v &
        TYPE == t &
        FUEL == f &
        POLLUTANT == p,
      c("STANDARD", "EF")] -> base

  std <- ifelse(standard == "VI", "V", standard)
  efb <- base$EF[match(std, base$STANDARD)]
  if (anyNA(efb)) {
    stop("No emission factor for v = '", v, "', t = '", t, "', f = '", f,
         "', p = '", p, "' and standard: ",
         paste(unique(standard[is.na(efb)]), collapse = ", "))
  }

  return(EmissionFactors(efb))
}



#' @title Correction of Chinese emission factors by sulfur
#' @family China
#' @name ef_china_s
#' @description Correction of Chinese emission
#' @param s Numeric sulfur content in ppm
#' @param f Character;fuel: "G", "D", "CNG", "ALL"
#' @param standard Character vector; "PRE", "I", "II", "III", "IV", "V".
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @return long data.frame
#' @importFrom data.table as.data.table melt.data.table
#' @export
#' @examples {
#' ef_china_s(s = 1000, standard = "I", p = "CO")
#' }
ef_china_s <- function(s,
                       f = "G",
                       standard,
                       p){

  f <- ifelse(f %in% c("CNG", "G HY", "G"), "G", "D")

  chi <- data.table::as.data.table(sysdata$sulphur_china)

  VEH <- TYPE <- FUEL <- POLLUTANT <- YEAR <- STANDARD <- NULL

  chi <-  data.table::melt.data.table(data = chi,
                                      id.vars = c("FUEL",
                                                  "STANDARD",
                                                  "POLLUTANT"),
                                      variable.name = "sulfur")
  # same sulphur classes as ef_china()
  xs <- ifelse(
    s <= 10, "S10",
    ifelse(
      s <= 50, "S50",
      ifelse(
        s <= 150, "S150",
        ifelse(
          s <= 350, "S350",
          ifelse(
            s <= 500, "S500",
            "S800")))))

  sulfur <- NULL
  chi[FUEL == f &
        POLLUTANT == p &
        sulfur == xs] -> base


  if(nrow(base) == 0) {
    efb <- rep(1, length(standard))
  } else {
    efb <- base$value[match(standard, base$STANDARD)]
    if (anyNA(efb)) stop("No sulphur correction for standard: ",
                         paste(unique(standard[is.na(efb)]), collapse = ", "))
  }

  return(efb)
}


#' @title Correction of Chinese emission factors by deterioration
#' @family China
#' @name ef_china_det
#' @description Correction of Chinese emission
#' @param v Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"
#' @param t Character; sub-category of of vehicle: PV Gasoline:  "Mini", "Small","Medium",
#' "Large", "Taxi", "Motorcycles", "Moped", PV Diesel: "Mediumbus", "Largebus",
#'  "3-Wheel". Trucks: "Mini", "Light" , "Medium", "Heavy"
#' @param standard Character vector; "PRE", "I", "II", "III", "IV", "V".
#' @param f Character;fuel: "G", "D", "CNG", "ALL"
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @param yeardet Integer; any of 2014, 2015, 2016, 2017, 2018
#' @return long data.frame
#' @importFrom data.table as.data.table
#' @export
#' @examples {
#' ef_china_det(standard = "I", p = "CO")
#' ef_china_det(standard = c("I", "III"),
#'              p = "CO",
#'              f = "D")
#' }
ef_china_det <- function(v = "PV",
                         t = "Small",
                         f = "G",
                         standard,
                         yeardet = 2015,
                         p){


  det <- data.table::as.data.table(sysdata$det_china_long)

  VEH <- TYPE <- FUEL <- POLLUTANT <- YEAR <- STANDARD <- NULL

  det[VEH == v &
        TYPE == t &
        FUEL == f &
        YEAR == yeardet &
        POLLUTANT == p,
      c("STANDARD",
        "DET")] -> basedet

  if(nrow(basedet) > 0) {
    if (anyDuplicated(basedet$STANDARD)) {
      stop("Duplicated deterioration factors for yeardet = ", yeardet,
           ". yeardet must be one of 2015, 2016, 2017, 2018")
    }
    efs <- basedet$DET[match(standard, basedet$STANDARD)]
    if (anyNA(efs)) stop("No deterioration factor for standard: ",
                         paste(unique(standard[is.na(efs)]), collapse = ", "))
  } else {
    efs <- rep(1, length(standard))
  }
  return(efs)
}


#' @title Correction of Chinese emission factors by speed
#' @family China
#' @name ef_china_speed
#' @description Correction of Chinese emission
#' @param speed numeric speed km/h
#' @param f Character;fuel: "G", "D", "CNG"
#' @param standard Character vector; "PRE", "I", "II", "III", "IV", "V".
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @param long Logical, to process long format of ef
#' @return long data.frame
#' @importFrom data.table as.data.table
#' @export
#' @examples {
#' data(net)
#' head(ef_china_speed(speed = net$ps, standard = "I", p = "CO"))
#' head(ef_china_speed(speed = net$ps,
#'                     standard = c("II", "I"),
#'                     p = "NOx"))
#' }
ef_china_speed <- function(speed,
                           f = "G",
                           standard,
                           p,
                           long = FALSE){
  efsp <- data.table::as.data.table(sysdata$speed_china)

  f <- ifelse(f %in% c("CNG", "G HY", "G"), "G", f)

  sp <- as.numeric(speed)

  VEH <- TYPE <- FUEL <- POLLUTANT <- YEAR <- STANDARD <- NULL

  efsp[FUEL == f &
         POLLUTANT == p] -> basesp
  if (any(!standard %in% basesp$STANDARD)) {
    stop("No speed correction for f = '", f, "', p = '", p, "' and standard: ",
         paste(unique(standard[!standard %in% basesp$STANDARD]), collapse = ", "))
  }

  if(long) {

    data.table::melt.data.table(
      basesp,
      id.vars = c("FUEL", "STANDARD", "POLLUTANT"),
      measure.vars = c("S20",
                       "S20_30",
                       "S30_40",
                       "S40_80",
                       "S80"),
      variable.name = "speed_label",
      value.name = "speed_cor") -> dft

    sp <- data.frame(speed = sp)

    sp$speed_label <- ifelse(
      sp < 20, "S20",
      ifelse(
        sp >= 20 & sp < 30, "S20_30",
        ifelse(
          sp >= 30 & sp < 40, "S30_40",
          ifelse(
            sp >= 40 & sp < 80, "S40_80",
            "S80"))))

    efs <- lapply(seq_along(standard), function(i) {
      dd <- dft[STANDARD == standard[i]]
      # match() keeps the order of speed
      dd$speed_cor[match(sp$speed_label, dd$speed_label)]
    })
    efs <- as.data.frame(do.call("cbind", efs))


  } else {



  efs <- lapply(seq_along(standard), function(i) {
    sp_std <- basesp[STANDARD == standard[i]]

    ifelse(
      sp < 20, sp_std$S20,
      ifelse(
        sp >= 20 & sp < 30, sp_std$S20_30,
        ifelse(
          sp >= 30 & sp < 40, sp_std$S30_40,
          ifelse(
            sp >= 40 & sp < 80, sp_std$S40_80,
            sp_std$S80
          )
        )

      ))

  })
  efs <- as.data.frame(do.call("cbind", efs))
  }
  return(efs)
}

#' @title Correction of Chinese emission factors by temperature
#' @family China
#' @name ef_china_te
#' @description Correction of Chinese emission
#' @param te numeric temperature in celsius
#' @param v Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"
#' @param t Character; sub-category of of vehicle: PV Gasoline:  "Mini", "Small","Medium",
#' "Large", "Taxi", "Motorcycles", "Moped", PV Diesel: "Mediumbus", "Largebus",
#'  "3-Wheel". Trucks: "Mini", "Light" , "Medium", "Heavy"
#' @param f Character;fuel: "G", "D", "CNG"
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @return long data.frame
#' @importFrom data.table as.data.table
#' @export
#' @examples {
#' data(net)
#' head(ef_china_te(te = net$ps,  p = "CO"))
#' head(ef_china_te(te = net$ps,
#'                  p = "NOx"))
#' }
ef_china_te <- function(te,
                        v = "PV",
                        t = "Small",
                        f = "G",
                        p){
  efte <- data.table::as.data.table(sysdata$te_china)

  f <- ifelse(f %in% c("CNG", "G HY", "G"), "G", "D")

  VEH <- TYPE <- FUEL <- POLLUTANT <- NULL

  efte[VEH == v &
         TYPE == t &
         FUEL == f &
         POLLUTANT == p, ] -> eftes
  if (nrow(eftes) != 1) stop("No unique temperature correction for v = '", v,
                             "', t = '", t, "', f = '", f, "', p = '", p, "'")

  te <- as.numeric(te)

  x <- ifelse(
    te < 10, eftes$T10,
    ifelse(
      te > 25, eftes$T25,
      1))

  return(x)
}


#' @title Correction of Chinese emission factors by humidity
#' @family China
#' @name ef_china_hu
#' @description Correction of Chinese emission
#' @param hu numeric relative humidity in percent (0-100)
#' @param v Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"
#' @param t Character; sub-category of of vehicle: PV Gasoline:  "Mini", "Small","Medium",
#' "Large", "Taxi", "Motorcycles", "Moped", PV Diesel: "Mediumbus", "Largebus",
#'  "3-Wheel". Trucks: "Mini", "Light" , "Medium", "Heavy"
#' @param f Character;fuel: "G", "D", "CNG"
#' @param standard Character vector; "PRE", "I", "II", "III", "IV", "V".
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @return long data.frame
#' @importFrom data.table as.data.table
#' @export
#' @examples {
#' ef_china_hu(hu = 60, standard = "I", p = "CO")
#' }
ef_china_hu <- function(hu,
                        v = "PV",
                        t = "Small",
                        f = "G",
                        standard,
                        p){
  efh <- data.table::as.data.table(sysdata$humidity_china)

  f <- ifelse(f %in% c("CNG", "G HY", "G"), "G", "D")

  VEH <- TYPE <- FUEL <- POLLUTANT <- NULL

  efh[VEH == v &
        TYPE == t &
        FUEL == f &
        POLLUTANT == p, ] -> efhs
  if (nrow(efhs) != 1) stop("No unique humidity correction for v = '", v,
                            "', t = '", t, "', f = '", f, "', p = '", p, "'")

  hu <- as.numeric(hu)

  x <- ifelse(
    hu < 50, efhs$L50,
    ifelse(
      hu > 50, efhs$H50,
      1))

  return(x)
}

#' @title Correction of Chinese factors by humidity when temperature > 24
#' @family China
#' @name ef_china_th
#' @description Correction of Chinese emission
#' @param hu numeric relative humidity in percent (0-100)
#' @param te numeric temperature in celsius
#' @param v Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"
#' @param t Character; sub-category of of vehicle: PV Gasoline:  "Mini", "Small","Medium",
#' "Large", "Taxi", "Motorcycles", "Moped", PV Diesel: "Mediumbus", "Largebus",
#'  "3-Wheel". Trucks: "Mini", "Light" , "Medium", "Heavy"
#' @param f Character;fuel: "G", "D", "CNG"
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @return long data.frame
#' @importFrom data.table as.data.table
#' @export
#' @examples {
#' ef_china_th(hu = 60, te = 25,  p = "CO")
#' }
ef_china_th <- function(hu,
                        te,
                        v = "PV",
                        t = "Small",
                        f = "G",
                        p){
  efth <- data.table::as.data.table(sysdata$tehu_china)

  f <- ifelse(f %in% c("CNG", "G HY", "G"), "G", "D")

  VEH <- TYPE <- FUEL <- POLLUTANT <- NULL

  efth[VEH == v &
         TYPE == t &
         FUEL == f &
         POLLUTANT == p, ] -> efhs
  if (nrow(efhs) != 1) stop("No unique temperature-humidity correction for v = '", v,
                            "', t = '", t, "', f = '", f, "', p = '", p, "'")

  hu <- as.numeric(hu)
  te <- as.numeric(te)

  x <- ifelse(
    te > 24 & hu < 50, efhs$TH24L50,
    ifelse(
      te > 24 & hu > 50, efhs$TH24H50,
      1))

  return(x)
}


#' @title Correction of Chinese factors by altitude
#' @family China
#' @name ef_china_h
#' @description Correction of Chinese emission
#' @param h numeric altitude
#' @param v Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"
#' @param t Character; sub-category of of vehicle: PV Gasoline:  "Mini", "Small","Medium",
#' "Large", "Taxi", "Motorcycles", "Moped", PV Diesel: "Mediumbus", "Largebus",
#'  "3-Wheel". Trucks: "Mini", "Light" , "Medium", "Heavy"
#' @param f Character;fuel: "G", "D", "CNG"
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @return long data.frame
#' @importFrom data.table as.data.table
#' @export
#' @examples {
#' ef_china_h(h = 1600, p = "CO")
#' }
ef_china_h <- function(h,
                       v = "PV",
                       t = "Small",
                       f = "G",
                       p){
  efhi <- data.table::as.data.table(sysdata$h_china)

  f <- ifelse(f %in% c("CNG", "G HY", "G"), "G", "D")

  VEH <- TYPE <- FUEL <- POLLUTANT <- NULL

  efhi[VEH == v &
         TYPE == t &
         FUEL == f &
         POLLUTANT == p, ] -> efhis
  if (nrow(efhis) != 1) stop("No unique altitude correction for v = '", v,
                             "', t = '", t, "', f = '", f, "', p = '", p, "'")

  h <- as.numeric(h)

  x <- ifelse( h > 1500, efhis$H, 1)

  return(x)
}

#' @title Estimation with Chinese factors
#' @family China
#' @name emis_china
#' @description Emissions estimates
#' @param x Vehicles data.frame
#' @param lkm Length of each link in km
#' @param tfs temporal factor
#' @param v Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"
#' @param t Character; sub-category of of vehicle: PV Gasoline:  "Mini", "Small","Medium",
#' "Large", "Taxi", "Motorcycles", "Moped", PV Diesel: "Mediumbus", "Largebus",
#'  "3-Wheel". Trucks: "Mini", "Light" , "Medium", "Heavy"
#' @param f Character;fuel: "G", "D", "CNG", "ALL"
#' @param standard Character vector; "PRE", "I", "II", "III", "IV", "V".
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @param s Sulhur in ppm
#' @param speed Speed in km/h, data.frame with nrow(x) rows and one column for
#' each element of tfs (a single column is repeated for every hour)
#' @param te Temperature (length tfs)
#' @param hu Humidity in percent (length tfs)
#' @param h Altitude (length nrow x)
#' @param yeardet Year, default 2016
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @param array Logical to return EmissionsArray or not
#' @param verbose Logical to show more info
#' @return long data.frame
#' @importFrom data.table as.data.table
#' @export
#' @examples {
#' ef_china_h(h = 1600, p = "CO")
#' }
emis_china <- function(x,
                       lkm,
                       tfs,
                       v = "PV",
                       t = "Small",
                       f = "G",
                       standard,
                       s,
                       speed,
                       te,
                       hu,
                       h,
                       yeardet = 2016,
                       p,
                       verbose = TRUE,
                       array = FALSE){

  # checks
  if(length(tfs) != length(te)) stop("tfs, te and hu must have equal length")
  if(length(tfs) != length(hu)) stop("tfs, te and hu must have equal length")

  speed <- as.data.frame(speed)
  # if(ncol(speed) != ncol(x)) stop("speed and x must have equal length of cols")

  if(length(h) != nrow(x)) stop("length h and nrow x must have equal")
  if(length(standard) != ncol(x)) stop("length of standard must be equal to the number of columns of x")
  if(ncol(speed) == 1 & length(tfs) > 1) {
    speed <- speed[, rep(1, length(tfs)), drop = FALSE]
  }
  if(ncol(speed) != length(tfs)) stop("speed must have one column or one column for each tfs")
  if(nrow(speed) != nrow(x)) stop("speed and x must have the same number of rows")

  # Vehicle
  if(verbose) cat("\nProcessing Vehicles\n")
  nr <- nrow(x)
  nc <- ncol(x)
  x <- temp_veh(x = x, tfs = tfs)

  if(verbose) cat("Estimating Base EF\n")
  # base
  std1 <- standard
  ef_base <- ef_china_long(v = v,
                           t = t,
                           f = f,
                           standard = std1,
                           p = p)

  if(verbose) cat("Correcting Base EF by sulfur\n")
  # sulfur
  ef_base_s <- ef_china_s(s = s,
                          f = f,
                          standard = std1,
                          p = p)

  if(verbose) cat("Correcting Base EF by deterioration\n")
  # det
  ef_base_det <- ef_china_det(v = v,
                              t = t,
                              f = f,
                              standard = std1,
                              yeardet = yeardet,
                              p = p)


  if(verbose) cat("Correcting Base EF by speed\n")
  # speed
  ef_base_speed <- rbindlist(lapply(seq_along(speed), function(k){
    ef_china_speed(speed = speed[[k]],
                   f = f,
                   standard = std1,
                   p = p)
  }))


  if(verbose) cat("Correcting Base EF by temperature\n")
  # temperature
  ef_base_temp <- ef_china_te(te = te,
                              v = v,
                              t = t,
                              f = f,
                              p = p)

  if(verbose) cat("Correcting Base EF by humidity\n")
  # humidity
  ef_base_hu <- ef_china_hu(hu = hu,
                            v = v,
                            t = t,
                            f = f,
                            p = p)

  if(verbose) cat("Correcting Base EF by humidity if T > 24\n")
  # temperaturehumidity
  ef_base_th <- ef_china_th(hu = hu,
                            te = te,
                            v = v,
                            t = t,
                            f = f,
                            p = p)

  efmet <- ef_base_temp * ef_base_hu * ef_base_th
  # one value per hour, rows are stacked by hour (nr rows each)
  efmet <- rep(efmet, each = nr)

  if(verbose) cat("Correcting Base EF by Altitude\n")
  # altitude
  ef_base_h <- ef_china_h(h = h,
                          v = v,
                          t = t,
                          f = f,
                          p = p)

  # per standard (column) factors
  f_std <- as.numeric(remove_units(ef_base)) * ef_base_det * ef_base_s
  # per row factors (street x hour)
  f_row <- rep(ef_base_h, length(tfs)) * efmet

  ef_base_speedv2 <- as.matrix(ef_base_speed) *
    matrix(f_std, nrow = nr*length(tfs), ncol = nc, byrow = TRUE) *
    f_row

  if(verbose) cat("Estimating emissions\n")
  xx <- as.matrix(as.data.frame(x)[, 1:nc])
  E <- Emissions(as.data.frame(
    ef_base_speedv2 * xx * rep(as.numeric(lkm), length(tfs))))
  # return(E)
  E$Hour <- rep(seq_along(tfs), each = nr)

  if(array) {
    lx <- split(E, E$Hour)
    lxx <- unlist(lapply(seq_along(lx), function(i) {
      unlist(lx[[i]][, 1:nc])
    }))
    a <- EmissionsArray(array(data = lxx,
                              dim = c(nr,
                                      nc,
                                      length(tfs))))
    return(a)
  } else {
    return(E)

  }


  return(x)
}
