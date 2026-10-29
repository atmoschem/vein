#' Cold-Start Emissions factors for Light Duty Vehicles
#'
#' \code{\link{ef_ldv_cold}} returns speed functions or data.frames which depends on ambient temperature
#' average speed. The emission factors comes from the guidelines  EMEP/EEA air pollutant
#' emission inventory guidebook
#' https://www.eea.europa.eu/en/analysis/publications/emep-eea-guidebook-2023
#'
#' @param v Character; Category vehicle:  "LDV"
#' @param ta Numeric vector or data.frame; Ambient temperature. Monthly mean can be used. When
#' ta is a data.frame, one option is that the number of rows should be the number of
#' rows of your  Vehicles data.frame. This is convenient for top-down approach
#' when each simple feature can be a polygon, with a monthly average temperature for
#' each simple feature. In this case, the number of columns can be the 12 months.
#' A numeric vector with more than one value is treated as a data.frame with
#' one row and one column per value (e.g. 12 monthly means); \code{speed} is
#' then needed.
#' @param cc Character; Size of engine in cc: "<=1400",  "1400_2000" or ">2000"
#' @param f Character; Type of fuel: "G", "D" or "LPG"
#' @param eu Character or data.frame of Characters; Euro standard: "PRE", "I",
#'  "II", "III",  "IV", "V", "VI", "VIc", "VId" or "VII". When 'eu' is a data.frame and
#'  'ta' is also a data.frame both has to have the same number of rows. For instance,
#'  When you want that each simple feature or region has a different emission standard.
#' @param p Character; Pollutant: "CO", "FC", "NOx", "HC" or "PM"
#' @param k Numeric; Multiplication factor (ignored before vein 2.0.0)
#' @param show.equation Option to see or not the equation parameters
#' @param speed Numeric; Speed to return Number of emission factor and not a function.
#' @param fcorr Numeric; Correction by fuel properties by euro technology.
#' See \code{\link{fuel_corr}}. The order from first to last is
#' "PRE", "I", "II", "III", "IV", "V", VI, "VIc". Default is 1
#' @return an emission factor function which depends of the average speed V
#' and ambient temperature. g/km
#' @note The values are the cold/hot quotients eCOLD/eHOT of the EMEP/EEA
#' guidebook. For Euro 2 and later petrol vehicles the beta-reduction factors
#' (bc) are included as 1 + bc * (eCOLD/eHOT - 1), so that
#' \code{\link{emis_cold}}, which uses (eCOLD/eHOT - 1), applies bc to the
#' excess emission as in the guidebook.
#' @keywords cold emission factors
#' @seealso \code{\link{fuel_corr}}
#' @importFrom units as_units
#' @export
#' @examples \dontrun{
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
#' ef_ldv_cold(ta = dt, cc = "<=1400", f ="G", eu = euros, p = "CO", speed = Speed(34))
#' euros2 <- c("V", "V", "V", "IV", "IV", "IV", "III", "III")
#' dfe <- rbind(euros, euros2)
#' ef_ldv_cold(ta = 10, cc = "<=1400", f ="G", eu = dfe, p = "CO", speed = Speed(0))
#'
#' ef_ldv_cold(ta = dt[1:2,], cc = "<=1400", f ="G", eu = dfe, p = "CO", speed = Speed(0))
#' # Fuel corrections
#' fcorr <- c(0.5,1,1,1,0.9,0.9,0.9,0.9)
#' ef1 <- ef_ldv_cold(ta = 15, cc = "<=1400", f ="G", eu = "PRE", p = "CO",
#' show.equation = TRUE, fcorr = fcorr)
#' ef_ldv_cold(ta = 10, cc = "<=1400", f ="G", eu = dfe, p = "CO", speed = Speed(0),
#' fcorr = fcorr)
#' }
ef_ldv_cold <- function(v = "LDV",
                        ta, # can vary vertically, for each simple feature, and horizontally, for each month
                        cc, f,
                        eu, # can vary horizontally
                        p, k = 1,
                        show.equation = FALSE, speed, fcorr = rep(1, 8)) {
  ef_ldv <- sysdata$cold
  # Check eu
  if (is.matrix(eu) | is.data.frame(eu)) {
    eu <- as.data.frame(eu)
    for (i in 1:ncol(eu)) eu[, i] <- as.character(eu[, i])
  } else {
    eu = as.character(eu)
  }
  # Check speed
  if (!missing(speed)) {
    if (!inherits(speed, "units")) {
      stop("speed neeeds to has class 'units' in 'km/h'. Please, check package '?units::set_units'")
    }
    if (units(speed) != units(units::as_units("km/h"))) {
      stop("Units of speed must be 'km/h' ")
    }
    if (units(speed) == units(units::as_units("km/h"))) {
      speed <- as.numeric(speed)
    }
  }
  if (is.matrix(ta)) {
    ta <- as.data.frame(ta)
  }
  # numeric vector of temperatures (e.g. 12 monthly means): one row, one
  # column per value, as when ta is a data.frame
  if (!is.data.frame(ta) && length(ta) > 1) {
    ta <- as.data.frame(matrix(as.numeric(ta), nrow = 1))
  }
  cold_row <- function(e) {
    df <- ef_ldv[ef_ldv$VEH == v &
      ef_ldv$CC == cc &
      ef_ldv$FUEL == f &
      ef_ldv$EURO == e &
      ef_ldv$POLLUTANT == p, ]
    if (nrow(df) == 0) {
      stop("No cold-start emission factor for v = ", v, ", cc = ", cc,
        ", f = ", f, ", eu = ", e, ", p = ", p)
    }
    df[1, ]
  }
  # Function to case when
  lala <- function(x) {
    ifelse(x == "PRE", fcorr[1],
      ifelse(
        x == "I", fcorr[2],
        ifelse(
          x == "II", fcorr[3],
          ifelse(
            x == "III", fcorr[4],
            ifelse(
              x == "IV", fcorr[5],
              ifelse(
                x == "V", fcorr[6],
                ifelse(
                  x == "VI", fcorr[7],
                  fcorr[8])))))))
  }



  # When eu is not a data.frame!
  if (!is.data.frame(eu)) {
    # Check ta, eu and speed
    if (is.data.frame(ta) | length(eu) > 1) {
      if (missing(speed)) stop("when 'ta' is data.frame or eu > 1, 'speed' is needed")
    }

    if (is.numeric(ta) & length(ta) == 1 & length(eu) == 1) {
      df <- cold_row(eu)
      k2 <- lala(eu)

      if (show.equation == TRUE) {
        cat(paste0("a = ", df$a, ", b = ", df$b, ", c = ", df$c, ", d = ", df$d,
          ", e = ", df$e, ", f = ", df$f, "\n"))
        cat(paste0("Equation = ", "(", as.character(df$Y), ")", "*", k, "*", k2))
      }
      f1 <- function(V) {
        a <- df$a
        b <- df$b
        c <- df$c
        d <- df$d
        e <- df$e
        f <- df$f
        g <- df$g
        h <- df$h
        i <- df$i
        V <- ifelse(V < df$MINV, df$MINV, ifelse(V > df$MAXV, df$MAXV, V))
        ifelse(eval(parse(text = paste0("(", as.character(df$Y), ")", "*", k, "*", k2))) < 0,
          0,
          eval(parse(text = paste0("(", as.character(df$Y), ")", "*", k, "*", k2))))
      }
      if (!missing(speed)) {
        return(EmissionFactors(f1(speed)))
      } else {
        return(f1)
      }

    } else if (is.numeric(ta) & length(ta) == 1 & length(eu) > 1) {
      dff <- do.call("cbind", lapply(1:length(eu), function(i) {
        df <- cold_row(eu[i])

        f1 <- function(V) {
          k2 <- lala(eu[i])
          a <- df$a
          b <- df$b
          c <- df$c
          d <- df$d
          e <- df$e
          f <- df$f
          g <- df$g
          h <- df$h
          i <- df$i
          V <- ifelse(V < df$MINV, df$MINV, ifelse(V > df$MAXV, df$MAXV, V))
          ifelse(eval(parse(text = paste0("(", as.character(df$Y), ")", "*", k, "*", k2))) < 0,
            0,
            eval(parse(text = paste0("(", as.character(df$Y), ")", "*", k, "*", k2))))
        }
        f1(speed)
      }))
      dff <- EmissionFactors(dff)
      names(dff) <- paste0(eu)
      dff$speed <- speed
      dff$ta <- ta
      return(dff)

    } else if (is.data.frame(ta)) {
      dff <- do.call("rbind", lapply(1:ncol(ta), function(kk) {
        do.call("rbind", lapply(1:nrow(ta), function(j) {
          dff <- do.call("cbind", lapply(1:length(eu), function(i) {
            df <- cold_row(eu[i])

            f1 <- function(V) {
              ta <- ta[j, kk]
              k2 <- lala(eu[i])
              a <- df$a
              b <- df$b
              c <- df$c
              d <- df$d
              e <- df$e
              f <- df$f
              g <- df$g
              h <- df$h
              i <- df$i
              V <- ifelse(V < df$MINV, df$MINV, ifelse(V > df$MAXV, df$MAXV, V))
              ifelse(eval(parse(text = paste0("(", as.character(df$Y), ")", "*", k, "*", k2))) < 0,
                0,
                eval(parse(text = paste0("(", as.character(df$Y), ")", "*", k, "*", k2))))
            }
            f1(speed)
          }))
          dff <- EmissionFactors(dff)
          names(dff) <- paste0(eu, "_", 1:length(eu))
          dff$speed <- speed
          dff$ta <- ta[j, kk]
          dff
        }))
      }))
      return(dff)
    }
    # This is new!!!!!!!!
  } else if (is.data.frame(eu) | is.matrix(eu)) {
    # Check ta, eu and speed
    if (is.data.frame(ta) | length(eu) > 1) {
      if (missing(speed)) stop("when 'ta' is data.frame or eu > 1, 'speed' is needed")
    }

    if (is.numeric(ta) & length(ta) == 1) {
      dff <- do.call("rbind", lapply(1:nrow(eu), function(j) {
        dff <- do.call("cbind", lapply(1:ncol(eu), function(i) {
          df <- cold_row(eu[j, i][[1]])

          f1 <- function(V) {
            k2 <- lala(eu[j, i][[1]])
            a <- df$a
            b <- df$b
            c <- df$c
            d <- df$d
            e <- df$e
            f <- df$f
            g <- df$g
            h <- df$h
            i <- df$i
            V <- ifelse(V < df$MINV, df$MINV, ifelse(V > df$MAXV, df$MAXV, V))
            ifelse(eval(parse(text = paste0("(", as.character(df$Y), ")", "*", k, "*", k2))) < 0,
              0,
              eval(parse(text = paste0("(", as.character(df$Y), ")", "*", k, "*", k2))))
          }
          f1(speed)
        }))
        dff <- EmissionFactors(dff)
        dff$speed <- speed
        # dff$euros <- rep(paste0("Row eu ", 1:nrow(eu)), each = length(speed))
        dff$ta <- ta
        dff
      }))
      return(dff)

    } else if (is.data.frame(ta)) {
      # if(nrow(ta) != nrow(eu)) {
      #   cat("Number of rows of 'eu': ", nrow(eu), "\n")
      #   cat("Number of rows of 'ta': ", nrow(ta), "\n")
      #   stop("Number of rows of 'eu' and 'ta' must be the same")
      # }
      dff <- do.call("rbind", lapply(1:ncol(ta), function(kk) {
        do.call("rbind", lapply(1:nrow(ta), function(j) {
          dff <- do.call("cbind", lapply(1:ncol(eu), function(i) {
            df <- cold_row(eu[j, i][[1]])

            f1 <- function(V) {
              ta <- ta[j, kk]
              k2 <- lala(eu[j, i][[1]])
              a <- df$a
              b <- df$b
              c <- df$c
              d <- df$d
              e <- df$e
              f <- df$f
              g <- df$g
              h <- df$h
              i <- df$i
              V <- ifelse(V < df$MINV, df$MINV, ifelse(V > df$MAXV, df$MAXV, V))
              ifelse(eval(parse(text = paste0("(", as.character(df$Y), ")", "*", k, "*", k2))) < 0,
                0,
                eval(parse(text = paste0("(", as.character(df$Y), ")", "*", k, "*", k2))))
            }
            f1(speed)
          }))
          dff <- EmissionFactors(dff)
          dff$speed <- speed
          dff$ta <- ta[j, kk]
          dff$row_eu <- j
          dff
        }))
      }))
      return(dff)
    }


  }


}
