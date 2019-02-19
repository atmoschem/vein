#' Cold-Start Emissions factors for Light Duty Vehicles
#'
#' This function returns speed functions which depends on ambient temperature
#' average speed. The emission factors comes from the guidelines  EMEP/EEA air pollutant
#' emission inventory guidebook
#' http://www.eea.europa.eu/themes/air/emep-eea-air-pollutant-emission-inventory-guidebook
#'
#' @param v Character; Category vehicle:  "LDV"
#' @param ta Numeric; Ambient temperature. Monthly mean can be used
#' @param cc Character; Size of engine in cc: "<=1400",  "1400_2000" or ">2000"
#' @param f Character; Type of fuel: "G", "D" or "LPG"
#' @param eu Character; Euro standard: "PRE", "I", "II", "III",  "IV", "V", "VI" or "VIc"
#' @param p Character; Pollutant: "CO", "FC", "NOx", "HC" or "PM"
#' @param k Numeric; Multiplication factor
#' @param show.equation Option to see or not the equation parameters
#' @param speed Numeric; Speed to return Number of emission factor and not a function.
#' @param i ignore
#' @return an emission factor function which depends of the average speed V
#' and ambient temperature. g/km
#' @keywords cold emission factors
#' @export
#' @examples {
#' ef1 <- ef_ldv_cold(ta = 15, cc = "<=1400", f ="G", eu = "PRE", p = "CO",
#' show.equation = TRUE)
#' ef1(10)
#' speed <- Speed(10)
#' ef_ldv_cold(ta = 15, cc = "<=1400", f ="G", eu = "PRE", p = "CO", speed = speed)
#' # lets create a matrix of ef cold at different speeds and temperatures
#' te <- -50:50
#' lf <- sapply(1:length(te), function(i){
#' ef_ldv_cold(ta = te[i], cc = "<=1400", f ="G", eu = "I", p = "CO", speed = Speed(0:120))
#' })
#' filled.contour(lf, col= cptcity::lucky())
#' euros <- c("V", "V", "IV", "III", "II", "I", "PRE", "PRE")
#' ef_ldv_cold(ta = 10, cc = "<=1400", f ="G", eu = euros, p = "CO", speed = Speed(0))
#' lf <-  ef_ldv_cold(ta = 10, cc = "<=1400", f ="G", eu = euros, p = "CO", speed = Speed(0:120))
#' dt <- matrix(rep(2:25,5), ncol = 12) # 12 months
#' ef_ldv_cold(ta = dt, cc = "<=1400", f ="G", eu = "I", p = "CO", speed = Speed(0))
#' ef_ldv_cold(ta = dt, cc = "<=1400", f ="G", eu = euros, p = "CO", speed = Speed(0))
#' ef_ldv_cold(ta = dt, cc = "<=1400", f ="G", eu = euros, p = "CO", speed = Speed(0:120))
#' }
ef_ldv_cold <- function(v = "LDV",
                        ta, # can vary vertically, for each simple feature, and horizontally, for each month
                        cc, f,
                        eu, # can vary horizontally
                        p, k = 1,
                        show.equation = FALSE, speed, i){
  ef_ldv <- sysdata$cold
  eu = as.character(eu)
  # Check speed
  if(!missing(speed)){
    if(class(speed) != "units"){
      stop("speed neeeds to has class 'units' in 'km/h'. Please, check package '?units::set_units'")
    }
    if(units(speed)$numerator != "km" | units(speed)$denominator != "h"){
      stop("Units of g must be 'km/h' ")
    }
    if(units(speed)$numerator == "km" & units(speed)$denominator == "h"){
      speed <- as.numeric(speed)
    }
  }
  if(is.matrix(ta)){
    ta <- as.data.frame(ta)
  }
  # Check ta, eu and speed
  if(is.data.frame(ta) | length(eu) > 1){
    if(missing(speed)) stop("when 'ta' is data.frame or eu > 1, 'speed' is needed")
  }
  # go
  if (is.numeric(ta) & length(ta) == 1 & length(eu) == 1){
    df <- ef_ldv[ef_ldv$VEH == v &
                   ef_ldv$CC == cc &
                   ef_ldv$FUEL == f &
                   ef_ldv$EURO == eu &
                   ef_ldv$POLLUTANT == p, ]

    if (show.equation == TRUE) {
      cat(paste0("a = ", df$a, ", b = ", df$b, ", c = ", df$c, ", d = ", df$d,
                 ", e = ", df$e, ", f = ", df$f, "\n"))
      cat(paste0("Equation = ", "(",as.character(df$Y), ")", "*", k))
    }
    f1 <- function(V){
      a <- df$a; b <- df$b; c <- df$c;  d <- df$d; e <- df$e;  f <- df$f
      g <- df$g; h <- df$h; i <- df$i
      V <- ifelse(V<df$MINV,df$MINV,ifelse(V>df$MAXV,df$MAXV,V))
      ifelse(eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k))) < 0,
             0,
             eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k))))
    }
    if(!missing(speed)) {
      return(EmissionFactors(f1(speed)))
    } else {
      return(f1)
    }

  } else if(is.numeric(ta) & length(ta) == 1 & length(eu) > 1){
    dff <- do.call("cbind", lapply(1:length(eu), function(i){
      df <- ef_ldv[ef_ldv$VEH == v &
                     ef_ldv$CC == cc &
                     ef_ldv$FUEL == f &
                     ef_ldv$EURO == eu[i] &
                     ef_ldv$POLLUTANT == p, ]
      f1 <- function(V){
        a <- df$a; b <- df$b; c <- df$c;  d <- df$d; e <- df$e;  f <- df$f
        g <- df$g; h <- df$h; i <- df$i
        V <- ifelse(V<df$MINV,df$MINV,ifelse(V>df$MAXV,df$MAXV,V))
        ifelse(eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k))) < 0,
               0,
               eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k))))
      }
      f1(speed)
    }))
    dff <- EmissionFactors(dff)
    names(dff) <- paste0(eu)
    dff$speed <- speed
    dff$ta <- ta
    return(dff)

  } else if(is.data.frame(ta)){
    dff <- do.call("rbind", lapply(1:ncol(ta), function(k){
      do.call("rbind", lapply(1:nrow(ta), function(j){
        dff <- do.call("cbind", lapply(1:length(eu), function(i){
          df <- ef_ldv[ef_ldv$VEH == v &
                         ef_ldv$CC == cc &
                         ef_ldv$FUEL == f &
                         ef_ldv$EURO == eu[i] &
                         ef_ldv$POLLUTANT == p, ]
          f1 <- function(V){
            ta <- ta[j, k]
            a <- df$a; b <- df$b; c <- df$c;  d <- df$d; e <- df$e;  f <- df$f
            g <- df$g; h <- df$h; i <- df$i
            V <- ifelse(V<df$MINV,df$MINV,ifelse(V>df$MAXV,df$MAXV,V))
            ifelse(eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k))) < 0,
                   0,
                   eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k))))
          }
          f1(speed)
        }))
        dff <- EmissionFactors(dff)
        names(dff) <- paste0(eu, "_", 1:length(eu))
        dff$speed <- speed
        dff$ta <- ta[j,k]
        dff
      }))
    }))
    return(dff)
  }
}
