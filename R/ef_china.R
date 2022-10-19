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
#' @param standard Character or data.frame; "PRE", "I", "II", "III", "IV", "V". When
#' it is a data.frame, it each row is a different region and ta, humidity,
#' altitud, speed, sulphur and load_factor lengths have the same as the number of
#' rows.
#' @param f Character;fuel: "G", "D", "CNG", "ALL"
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @param k Numeric; multiplication factor
#' @param ta Numeric; temperature of ambient in celcius degrees. When standard is a
#' data.frame, the length must be equal to the number of rows of standard.
#' @param humidity Numeric; relative humidity. When standard is a
#' data.frame, the length must be equal to the number of rows of standard.
#' @param altitude Numeric; altitude in meters. When standard is a
#' data.frame, the length must be equal to the number of rows of standard.
#' @param speed Numeric; altitude in km/h When standard is a
#' data.frame, the length must be equal to the number of rows of standard.
#' @param baseyear_det Integer; any of 2014, 2015, 2016, 2017, 2018
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
#' @seealso \code{\link{ef_ldv_speed}} \code{\link{emis_hot_td}}
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
  if(units(speed)$numerator != "km" | units(speed)$denominator != "h"){
    stop("Units of g must be 'km/h' ")
  }
  if(units(speed)$numerator == "km" & units(speed)$denominator == "h"){
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


  # fun starts ####
  if(!is.data.frame(standard) & !is.data.frame(ta)){
    dff <- lapply(1:length(standard), function(i){
      df <- ef_china[ef_china$VEH == v &
                       ef_china$TYPE == t &
                       ef_china$FUEL == f &
                       ef_china$STANDARD == standard[i] &
                       ef_china$POLLUTANT == p, ]

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
      efspeed <- speed_china[speed_china$FUEL == f &
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
                ifelse(speed > 80, df$EF*efspeed$S80, 0))))))
      # Check deterioration
      if(f == "G" & p %in% c("CO", "HC", "NOx")){
        ts <- ifelse(t %in% c("Mini", "Small"),"group1",
                     ifelse(t == "Taxi", "Taxi","group2"))
        detfac <- det_china[det_china$TYPE == ts &
                              det_china$FUEL == f &
                              det_china$STANDARD == standard[i] &
                              det_china$POLLUTANT == p &
                              det_china$YEAR == baseyear_det, ]
      }
      # Check sulphur
      sulphur_china <- sulphur_china[sulphur_china$FUEL == f &
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
      df$EF <- ifelse(
        load_factor == 0, df$EF*dfl$L0,
        ifelse(
          load_factor > 0 & load_factor < 0.5, df$EF*dfl$L50,
          ifelse(load_factor == 0.5, df$EF,
                 ifelse(
                   load_factor > 0.5 & sulphur <= 0.6, df$EF*dfl$L60,
                   ifelse(
                     load_factor > 0.6 & sulphur <= 0.75, df$EF*dfl$L75,
                     df$EF*dfl$L100)))))
    })

    if(correction_only){
      return(unlist(dff)*k)
    } else {
      return(EmissionFactors(unlist(dff)*k))
    }

    # standard is data.frames ####
  } else if(is.matrix(standard) | is.data.frame(standard) & !is.data.frame(ta)) {
    standard <- as.data.frame(standard)

    dff <- do.call("rbind", lapply(1:nrow(standard), function(j){
      do.call("cbind", lapply(1:ncol(standard), function(i){
        df <- ef_china[ef_china$VEH == v &
                         ef_china$TYPE == t &
                         ef_china$FUEL == f &
                         ef_china$STANDARD == standard[j,i][[1]] &
                         ef_china$POLLUTANT == p, ]
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
          df$EF <- ifelse(humidity[j] > 0.5, df$EF*0.92, df$EF*1.06)
        } else if(p == "NOx" & f == "D"){
          df$EF <- ifelse(humidity[j] > 0.5, df$EF*0.94, df$EF*1.04)
        } else {
          df$EF <- df$EF
        }
        # Check correction ta[j] and humidity - gasoline
        if(ta[j] > 24){
          if(f == "G"){
            if(p == "CO"){
              df$EF <- ifelse(humidity[j] > 0.5, df$EF*1.04, df$EF*0.97)
            } else if (p == "HC"){
              df$EF <- ifelse(humidity[j] > 0.5, df$EF*1.01, df$EF*0.99)
            } else if(p == "NOx"){
              df$EF <- ifelse(humidity[j] > 0.5, df$EF*0.87, df$EF*1.13)
            }
          } else if(f == "D"){
            if(p == "NOx"){
              df$EF <- ifelse(humidity[j] > 0.5, df$EF*0.88, df$EF*1.12)
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
                                     ifelse(p == "NOx", df$EF*3.15, df$EF)))
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
        efspeed <- speed_china[speed_china$FUEL == f &
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
          ts <- ifelse(t %in% c("Mini", "Small"),"group1",
                       ifelse(t == "Taxi", "Taxi","group2"))
          detfac <- det_china[det_china$TYPE == ts &
                                det_china$FUEL == f &
                                det_china$STANDARD == standard[j,i][[1]] &
                                det_china$POLLUTANT == p &
                                det_china$YEAR == baseyear_det, ]
          df$EF <- df$EF*detfac$DET
        }
        # Check sulphur
        if(length(sulphur) != nrow(standard)) stop("length of 'sulphur' must be the same as the number of rows of 'standard'")
        sulphur_china <- sulphur_china[sulphur_china$FUEL == f &
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
        df$EF <- ifelse(
          load_factor == 0, df$EF*dfl$L0,
          ifelse(
            load_factor > 0 & load_factor < 0.5, df$EF*dfl$L50,
            ifelse(load_factor == 0.5, df$EF,
                   ifelse(
                     load_factor > 0.5 & sulphur <= 0.6, df$EF*dfl$L60,
                     ifelse(
                       load_factor > 0.6 & sulphur <= 0.75, df$EF*dfl$L75,
                       df$EF*dfl$L100)))))

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
  } else if (is.matrix(standard) | is.data.frame(standard) & is.data.frame(ta)){
    standard <- as.data.frame(standard)

    if(ncol(ta) != 12) warning("This function was designed so that number of columns of ta is 12, one year")
    if(nrow(ta) != nrow(standard)) {
      stop("number of rows of 'ta' must be the same as the number of rows of 'standard'")
    }
    dff <- do.call("rbind", lapply(1:ncol(ta), function(k){
      do.call("rbind", lapply(1:nrow(standard), function(j){
        do.call("cbind", lapply(1:ncol(standard), function(i){
          df <- ef_china[ef_china$VEH == v &
                           ef_china$TYPE == t &
                           ef_china$FUEL == f &
                           ef_china$STANDARD == standard[j,i][[1]] &
                           ef_china$POLLUTANT == p, ]
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
            df$EF <- ifelse(humidity > 0.5, df$EF*0.92, df$EF*1.06)
          } else if(p == "NOx" & f == "D"){
            df$EF <- ifelse(humidity > 0.5, df$EF*0.94, df$EF*1.04)
          } else {
            df$EF <- df$EF
          }
          # Check correction ta and humidity - gasoline
          if(ta > 24){
            if(f == "G"){
              if(p == "CO"){
                df$EF <- ifelse(humidity > 0.5, df$EF*1.04, df$EF*0.97)
              } else if (p == "HC"){
                df$EF <- ifelse(humidity > 0.5, df$EF*1.01, df$EF*0.99)
              } else if(p == "NOx"){
                df$EF <- ifelse(humidity > 0.5, df$EF*0.87, df$EF*1.13)
              }
            } else if(f == "D"){
              if(p == "NOx"){
                df$EF <- ifelse(humidity > 0.5, df$EF*0.88, df$EF*1.12)
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
                                       ifelse(p == "NOx", df$EF*3.15, df$EF)))
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
          efspeed <- speed_china[speed_china$FUEL == f &
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
            ts <- ifelse(t %in% c("Mini", "Small"),"group1",
                         ifelse(t == "Taxi", "Taxi","group2"))
            detfac <- det_china[det_china$TYPE == ts &
                                  det_china$FUEL == f &
                                  det_china$STANDARD == standard[j,i][[1]] &
                                  det_china$POLLUTANT == p &
                                  det_china$YEAR == baseyear_det, ]
            df$EF <- df$EF*detfac$DET
          }
          # Check sulphur
          if(length(sulphur) != nrow(standard)) stop("length of 'sulphur' must be the same as the number of rows of 'standard'")
          sulphur_china <- sulphur_china[sulphur_china$FUEL == f &
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
          df$EF <- ifelse(
            load_factor == 0, df$EF*dfl$L0,
            ifelse(
              load_factor > 0 & load_factor < 0.5, df$EF*dfl$L50,
              ifelse(load_factor == 0.5, df$EF,
                     ifelse(
                       load_factor > 0.5 & sulphur <= 0.6, df$EF*dfl$L60,
                       ifelse(
                         load_factor > 0.6 & sulphur <= 0.75, df$EF*dfl$L75,
                         df$EF*dfl$L100)))))
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
#' @param standard Character vector; "PRE", "I", "II", "III", "IV", "V".
#' @param f Character;fuel: "G", "D", "CNG", "ALL"
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @return long data.frame
#' @importFrom data.table setDT
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }
#' }
ef_china_long <- function(v = "PV",
                          t = "Small",
                          f = "G",
                          standard,
                          p){
  chi <- sysdata$ef_china
  data.table::setDT(chi)

  VEH <- TYPE <- FUEL <- POLLUTANT <- YEAR <- STANDARD <- NULL

  chi[VEH == v &
        TYPE == t &
        FUEL == f &
        POLLUTANT == p,
      c("STANDARD", "EF")] -> base

  efb <- EmissionFactors(unlist(lapply(seq_along(standard), function(i) {
    base[STANDARD == standard[i]]$EF
  })))

  return(efb)
}


#' @title Correction of Chinese emission factors by speed
#' @family China
#' @name ef_china_long
#' @description Correction of Chinese emission
#' @param v Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"
#' @param t Character; sub-category of of vehicle: PV Gasoline:  "Mini", "Small","Medium",
#' "Large", "Taxi", "Motorcycles", "Moped", PV Diesel: "Mediumbus", "Largebus",
#'  "3-Wheel". Trucks: "Mini", "Light" , "Medium", "Heavy"
#' @param standard Character vector; "PRE", "I", "II", "III", "IV", "V".
#' @param f Character;fuel: "G", "D", "CNG", "ALL"
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @return long data.frame
#' @importFrom data.table setDT
#' @export
#' @examples {
#' ef_china_long(standard = "I", p = "CO")
#' }
ef_china_long <- function(v = "PV",
                          t = "Small",
                          f = "G",
                          standard,
                          p){
  chi <- sysdata$ef_china

  data.table::setDT(chi)

  VEH <- TYPE <- FUEL <- POLLUTANT <- YEAR <- STANDARD <- NULL

  chi[VEH == v &
        TYPE == t &
        FUEL == f &
        POLLUTANT == p,
      c("STANDARD", "EF")] -> base

  efb <- EmissionFactors(unlist(lapply(seq_along(standard), function(i) {
    base[STANDARD == standard[i]]$EF
  })))

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
#' @importFrom data.table setDT
#' @export
#' @examples {
#' ef_china_det(standard = "I", p = "CO")
#' }
ef_china_det <- function(v = "PV",
                         t = "Small",
                         f = "G",
                         standard,
                         yeardet = 2015,
                         p){


  det <- sysdata$det_china_long

  data.table::setDT(det)
  if(f == "G") {

    VEH <- TYPE <- FUEL <- POLLUTANT <- YEAR <- STANDARD <- NULL

    det[VEH == v &
          TYPE == t &
          FUEL == f &
          YEAR == yeardet &
          POLLUTANT == p,
        c("STANDARD",
          "DET")] -> basedet


    efs <- unlist(lapply(seq_along(standard), function(i) {
      basedet[STANDARD == standard[i]]$DET
    }))

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
#' @return long data.frame
#' @importFrom data.table setDT
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
                           p){
  efsp <- sysdata$speed_china

  cng <- efsp[efsp$FUEL == "G", ]
  cng$FUEL <- "CNG"
  efsp <- rbind(efsp, cng)

  data.table::setDT(efsp)

  sp <- as.numeric(speed)

  VEH <- TYPE <- FUEL <- POLLUTANT <- YEAR <- STANDARD <- NULL

  efsp[FUEL == f &
         POLLUTANT == p] -> basesp

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
#' @param standard Character vector; "PRE", "I", "II", "III", "IV", "V".
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @return long data.frame
#' @importFrom data.table setDT
#' @export
#' @examples {
#' data(net)
#' head(ef_china_te(te = net$ps, standard = "I", p = "CO"))
#' head(ef_china_te(te = net$ps,
#'                  standard = c("II", "I"),
#'                  p = "NOx"))
#' }
ef_china_te <- function(te,
                        v = "PV",
                        t = "Small",
                        f = "G",
                        standard,
                        p){
  efte <- sysdata$te_china

  cng <- efte[efte$FUEL == "G", ]
  cng$FUEL <- "CNG"
  efte <- rbind(efte, cng)

  data.table::setDT(efte)

  VEH <- TYPE <- FUEL <- POLLUTANT <- NULL

  efte[VEH == v &
         TYPE == t &
         FUEL == f &
         POLLUTANT == p, ] -> eftes

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
#' @param hu numeric humidity
#' @param v Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"
#' @param t Character; sub-category of of vehicle: PV Gasoline:  "Mini", "Small","Medium",
#' "Large", "Taxi", "Motorcycles", "Moped", PV Diesel: "Mediumbus", "Largebus",
#'  "3-Wheel". Trucks: "Mini", "Light" , "Medium", "Heavy"
#' @param f Character;fuel: "G", "D", "CNG"
#' @param standard Character vector; "PRE", "I", "II", "III", "IV", "V".
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @return long data.frame
#' @importFrom data.table setDT
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
  efh <- sysdata$humidity_china

  cng <- efh[efh$FUEL == "G", ]
  cng$FUEL <- "CNG"
  efh <- rbind(efh, cng)

  data.table::setDT(efh)

  VEH <- TYPE <- FUEL <- POLLUTANT <- NULL

  efh[VEH == v &
        TYPE == t &
        FUEL == f &
        POLLUTANT == p, ] -> efhs

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
#' @param hu numeric humidity
#' @param te numeric temperature in celsius
#' @param v Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"
#' @param t Character; sub-category of of vehicle: PV Gasoline:  "Mini", "Small","Medium",
#' "Large", "Taxi", "Motorcycles", "Moped", PV Diesel: "Mediumbus", "Largebus",
#'  "3-Wheel". Trucks: "Mini", "Light" , "Medium", "Heavy"
#' @param f Character;fuel: "G", "D", "CNG"
#' @param standard Character vector; "PRE", "I", "II", "III", "IV", "V".
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @return long data.frame
#' @importFrom data.table setDT
#' @export
#' @examples {
#' ef_china_th(hu = 60, te = 25, standard = "I", p = "CO")
#' }
ef_china_th <- function(hu,
                        te,
                        v = "PV",
                        t = "Small",
                        f = "G",
                        standard,
                        p){
  efth <- sysdata$tehu_china

  cng <- efth[efth$FUEL == "G", ]
  cng$FUEL <- "CNG"
  efth <- rbind(efth, cng)

  data.table::setDT(efth)

  VEH <- TYPE <- FUEL <- POLLUTANT <- NULL

  efth[VEH == v &
         TYPE == t &
         FUEL == f &
         POLLUTANT == p, ] -> efhs

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
#' @param h numeric aktitude
#' @param v Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"
#' @param t Character; sub-category of of vehicle: PV Gasoline:  "Mini", "Small","Medium",
#' "Large", "Taxi", "Motorcycles", "Moped", PV Diesel: "Mediumbus", "Largebus",
#'  "3-Wheel". Trucks: "Mini", "Light" , "Medium", "Heavy"
#' @param f Character;fuel: "G", "D", "CNG"
#' @param standard Character vector; "PRE", "I", "II", "III", "IV", "V".
#' @param p Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
#' or "Evaporative_parking"
#' @return long data.frame
#' @importFrom data.table setDT
#' @export
#' @examples {
#' ef_china_h(h = 1600, standard = "I", p = "CO")
#' }
ef_china_h <- function(h,
                       v = "PV",
                       t = "Small",
                       f = "G",
                       standard,
                       p){
  efhi <- sysdata$h_china

  cng <- efhi[efhi$FUEL == "G", ]
  cng$FUEL <- "CNG"
  efhi <- rbind(efhi, cng)

  data.table::setDT(efhi)

  VEH <- TYPE <- FUEL <- POLLUTANT <- NULL

  efhi[VEH == v &
         TYPE == t &
         FUEL == f &
         POLLUTANT == p, ] -> efhis

  h <- as.numeric(h)

  x <- ifelse( h > 1500, efhis$H, 1)

  return(x)
}
