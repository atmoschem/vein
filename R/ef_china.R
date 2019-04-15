#' Emissions factors from Chinese emissions guidelines
#'
#' \code{\link{ef_china}} returns emission factors as vector or data.frames.
#' The emission factors  comes from the chinese emission guidelines (v3) from the
#' Chinese Ministry of Ecology and Environment
#' http://www.mee.gov.cn/gkml/hbb/bgth/201407/W020140708387895271474.pdf
#'
#' @param v Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"
#' @param t Character; sub-category of of vehicle: PV:  "Mini", "Small","Medium",
#' "Large", "Taxi", "Motorcycles", "Moped", "Mediumbus", "Largebus", "3-Wheel" Trucks:
#' "Mini", "Light" , "Medium", "Heavy"
#' @param standard Character or data.frame; "PRE", "I", "II", "III", "IV", "V". When
#' it is a data.frame, it each row is a different region and ta, humidity,
#' altitud, speed, sulphur and load_factor lengths have the same as the number of
#' rows.
#' @param f Character;fuel: "G", "D"
#' @param p Character; pollutant: "CO", "NOx","HC", "PM"
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
#' @seealso \code{\link{ef_ldv_speed}} \code{\link{emis_hot_td}}
#' @export
#' @examples {
#' # when standard is 'character'
#' ef_china(standard = c("I"), p = "CO", details = TRUE)
#' ef_china(standard = c("PRE", "I"), p = "CO", correction_only = TRUE)
#' # when standard is 'data.frame'
#' df_st <- matrix(c("V", "IV", "III", "III", "II", "I", "PRE"), nrow = 2, ncol = 7, byrow = TRUE)
#' df_st <- as.data.frame(df_st)
#' a <- ef_china(standard = df_st, p = "PM10", ta = rep(celsius(20), 2),
#' altitude = rep(1501, 2), speed = rep(Speed(29), 2), sulphur = rep(50, 2))
#' dim(a)
#' dim(df_st)
#' ef_china(standard = df_st, p = "PM2.5", ta = rep(celsius(20), 2),
#' altitude = rep(1501, 2), speed = rep(Speed(29), 2), sulphur = rep(50, 2))
#' a
#' # when standard, temperature and humidity are data.frames
#' # assuming 10 regions
#' df_st <- matrix(c("V", "IV", "III", "III", "II", "I", "PRE"), nrow = 10, ncol = 7, byrow = TRUE)
#' df_st <- as.data.frame(df_st)
#' df_t <- matrix(21:30, nrow = 10, ncol = 12, byrow = TRUE)
#' df_t <- as.data.frame(df_t)
#' for(i in 1:12) df_t[, i] <- celsius(df_t[, i])
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
                     ta = celsius(25),
                     humidity = 0.5,
                     altitude = 1000,
                     speed = Speed(29),
                     baseyear_det = 2016,
                     sulphur = 50,
                     load_factor = 0.5,
                     details = FALSE,
                     correction_only = FALSE){
  ef_china <- sysdata$ef_china
  det_china <- sysdata$det_china
  sulphur_china <- sysdata$sulphur_china
  speed_china <- sysdata$speed_china
  ef_china <- ef_china[!ef_china$POLLUTANT %in% c("Evaporative_driving",
                                                  "Evaporative_parking"), ]
  ev <- ef_china[ef_china$POLLUTANT %in% c("Evaporative_driving",
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
  if(class(speed) != "units"){
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

      # Check correction gasoline - ta
      if(f == "G"){
        if(class(ta) != "units") stop("ta must be units in celsius, use celsius(ta)")
        ta <- as.numeric(ta)
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
        if(class(ta) != "units") stop("ta must be units in celsius, use celsius(ta)")
        ta <- as.numeric(ta)
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
            df$EF <- ifelse(ta > 25, df$EF*068,
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
            speed >= 30 & speed < 40, df$EF*efspeed$S30_40,
            ifelse(
              speed >= 40 & speed < 80, df$EF*efspeed$S40_80, df$EF*efspeed$S80))))
      # Check deterioration
      if(f == "G" & p %in% c("CO", "HC", "NOx")){
        ts <- ifelse(t %in% c("Mini", "Small"),"group1",
                     ifelse(t == "Taxi", "group2"))
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
            df$EF*sulphur_china$S500)))
      # Check load
      dfl <- fl[fl$POLLUTANT == p, ]
      df$EF <- ifelse(
        load_factor == 0, df$EF*dfl$L0,
        ifelse(
          load_factor > 0 & load_factor <= 0.5, df$EF*dfl$L50,
          ifelse(
            load_factor > 0.5 & sulphur <= 0.6, df$EF*dfl$L60,
            ifelse(
              load_factor > 0.6 & sulphur <= 0.75, df$EF*dfl$L75,
              df$EF*dfl$L100))))
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

        # Check correction gasoline - ta
        if(f == "G"){
          if(class(ta) != "units") stop("ta must be units in celsius, use celsius(ta)")
          ta <- as.numeric(ta)
          if(length(ta) != nrow(standard)) stop("length of 'ta' must be the same as the number of rows of 'standard'")
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
            } else {
              df$EF <- df$EF
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
              speed[j] >= 30 & speed[j] < 40, df$EF*efspeed$S30_40,
              ifelse(
                speed[j] >= 40 & speed[j] < 80, df$EF*efspeed$S40_80, df$EF*efspeed$S80))))

        # Check deterioration
        if(f == "G" & p %in% c("CO", "HC", "NOx")){
          ts <- ifelse(t %in% c("Mini", "Small"),"group1",
                       ifelse(t == "Taxi", "group2"))
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
              df$EF*sulphur_china$S500)))
        # Check load
        dfl <- fl[fl$POLLUTANT == p, ]
        df$EF <- ifelse(
          load_factor == 0, df$EF*dfl$L0,
          ifelse(
            load_factor > 0 & load_factor <= 0.5, df$EF*dfl$L50,
            ifelse(
              load_factor > 0.5 & sulphur <= 0.6, df$EF*dfl$L60,
              ifelse(
                load_factor > 0.6 & sulphur <= 0.75, df$EF*dfl$L75,
                df$EF*dfl$L100))))

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
          humidity <- humidity[j, k]
          if(f == "G"){
            if(class(ta) != "units") stop("ta must be units in celsius, use celsius(ta)")
            ta <- as.numeric(ta)
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
                df$EF <- ifelse(ta > 25, df$EF*068,
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
                speed[j] >= 30 & speed[j] < 40, df$EF*efspeed$S30_40,
                ifelse(
                  speed[j] >= 40 & speed[j] < 80, df$EF*efspeed$S40_80, df$EF*efspeed$S80))))

          # Check deterioration
          if(f == "G" & p %in% c("CO", "HC", "NOx")){
            ts <- ifelse(t %in% c("Mini", "Small"),"group1",
                         ifelse(t == "Taxi", "group2"))
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
                df$EF*sulphur_china$S500)))
          # Check load
          dfl <- fl[fl$POLLUTANT == p, ]
          df$EF <- ifelse(
            load_factor == 0, df$EF*dfl$L0,
            ifelse(
              load_factor > 0 & load_factor <= 0.5, df$EF*dfl$L50,
              ifelse(
                load_factor > 0.5 & sulphur <= 0.6, df$EF*dfl$L60,
                ifelse(
                  load_factor > 0.6 & sulphur <= 0.75, df$EF*dfl$L75,
                  df$EF*dfl$L100))))
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
