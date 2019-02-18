#' Estimation of evaporative emissions
#'
#' @description \code{emis_evap} estimates evaporative emissions from
#' EMEP/EEA emisison guidelines
#'
#' @param veh Numeric or data.frame of Vehicles with untis 'veh'.
#' @param x Numeric which can be either, daily mileage by age of use
#' with units 'lkm', number of trips or number of proc. When it
#' has units 'lkm', all the emission factors must be in 'g/km'.
#' When ed is in g/day, x it is the number of days (without units).
#' When hotfi, hotc or warmc are in g/trip, x it is the number of trips (without units).
#' When hotfi, hotc or warmc are in g/proced, x it is the number of proced (without units).
#' @param ed average daily evaporative emisisons. If x has units 'lkm', the units
#' of ed must be 'g/km', other case, this are simply g/day (without units)
#' @param hotfi average hot running losses or soak evaporative factor
#' for vehicles with fuel injection and returnless fuel systems.
#'  If x has units 'lkm', the units of ed must be 'g/km',
#'  other case, this are simply g/trip or g/proced
#' @param hotc average running losses or soak evaporative factor for vehicles with
#' carburator or fuel return system
#' for vehicles with fuel injection and returnless fuel systems.
#'  If x has units 'lkm', the units of ed must be 'g/km',
#' @param warmc average cold and warm running losses or soak evaporative factor
#' for vehicles with carburator or fuel return system
#' for vehicles with fuel injection and returnless fuel systems.
#'  If x has units 'lkm', the units of ed must be 'g/km',
#' @param carb fraction of gasoline vehicles with carburator or fuel return system.
#' @param p Fraction of trips finished with hot engine
#' @param params Character; Add columns with information to returning data.frame
#' @param pro_month Numeric; montly profile to distribuite annual mileage in each month.
#' @param verbose Logical; To show more information
#' @return numeric vector of emission estimation in grams
#' @importFrom units as_units
#' @references Mellios G and Ntziachristos 2016. Gasoline evaporation. In:
#' EEA, EMEP. EEA air pollutant emission inventory guidebook-2009. European
#' Environment Agency, Copenhagen, 2009
#' @export
#' @examples {
#' (a <- Vehicles(1:10))
#' (lkm <- units::as_units(1:10, "km"))
#' (ef <- EmissionFactors(1:10))
#' (ev <- emis_evap(veh = a, x = lkm, hotfi = ef))
#' }
emis_evap <- function(veh,
                      x,
                      ed,
                      hotfi, hotc, warmc,
                      carb = 0, p,
                      params,
                      pro_month,
                      verbose = FALSE) { # hot or warm soak
  # If months exists, it must the last column to be added
  # Check y
  if(!missing(x)){
    if(units(x)$numerator == "km"){
      message('Emission factors must have units g/km')
    }
  }
  # Checking sf
  if(any(class(veh) %in% "sf")){
    if(verbose) message("converting sf to data.frame")
    veh <- sf::st_set_geometry(veh, NULL)
  }

  # Check x for data.frame
  if(is.data.frame(veh) ){
    # DO I need check for 'Vehicles'?
    for(i in 1:ncol(veh)){
      veh[,i] <- as.numeric(veh[, i])
    }
    # in bottom-up approach, length of x is the number of streets
    # in top-down, length of x is the number of columns of veh
    # This is top-down
  } else {
    veh <- as.numeric(veh)
  }
 if(!missing(params)){
  if(verbose) message("Params will be added")
 }


  # ed
  if(!missing(ed)){
    if(is.data.frame(veh)){

      if(!is.data.frame(ed) & !missing(pro_month)) stop("as veh is a data.frame ed needs to be a data.frame qith the same dimensions")

      if(!missing(pro_month)){
        if(length(pro_month) != 12) stop("Length of pro_month must be 12")
        mes <- ifelse(nchar(1:12)<2, paste0(0, 1:12), 1:12)
        e <- do.call("rbind", lapply(1:12, function(j){
          e <- do.call("cbind", lapply(1:ncol(veh), function(i){
            veh[, i]*x[i]*ed[[j]][, i]*pro_month[j]
          }))
          e <- Emissions(e)
          if(!missing(params)){
            for (i in 1:length(params)){
              e <- as.data.frame(e)
              e[, params[i]] <- params[i]
            }
          }

          e$month <- mes[j]
          e
        }))
        if(verbose) cat("Sum of emissions:", sum(e[-"month"]), "\n")

      } else {
        if(!is.data.frame(ed)) stop("'ed' must be a data.frame with the same dimensions of 'veh'")
        e <- Emissions(do.call("cbind", lapply(1:ncol(veh), function(i){
          veh[, i]*x[i]*ed[, i]
        })))
        if(verbose) cat("Sum of emissions:", sum(e), "\n")
        if(!missing(params)){
          for (i in 1:length(params)){
            e <- as.data.frame(e)
            e[, params[i]] <- params[i]
          }
        }

      }
    } else {
      e <- Emissions(veh*x*ed)
      if(verbose) cat("Sum of emissions:", sum(e), "\n")
      if(!missing(params)){
        for (i in 1:length(params)){
          e <- as.data.frame(e)
          e[, params[i]] <- params[i]
        }
      }

    }

  } else {
    if(carb > 0){
      if(is.data.frame(veh)){

        if(!missing(pro_month)){
          if(length(pro_month) != 12) stop("Length of pro_month must be 12")
          mes <- ifelse(nchar(1:12)<2, paste0(0, 1:12), 1:12)
          e <- do.call("rbind", lapply(1:12, function(j){
            e <- do.call("cbind", lapply(1:ncol(veh), function(i){
              veh[, i]*x[i]*(carb*(p*hotc[[j]][, i]+(1-p)*warmc[[j]][, i])+(1-carb)*hotfi[[j]][, i])*pro_month[j]
            }))
            e <- Emissions(e)
            if(!missing(params)){
              for (i in 1:length(params)){
                e <- as.data.frame(e)
                e[, params[i]] <- params[i]
              }
            }

            e$month <- mes[j]
            e
          }))
          if(verbose) cat("Sum of emissions:", sum(e[-"month"]), "\n")

        } else {
          if(!is.data.frame(hotc)) stop("hotc' must be a data.frame with the same dimensions of 'veh'")
          if(!is.data.frame(warmc)) stop("warmc' must be a data.frame with the same dimensions of 'veh'")

          e <- Emissions(do.call("cbind",lapply(1:ncol(veh), function(i){
            veh[, i]*x[i]*(carb*(p*hotc[, i]+(1-p)*warmc[, i])+(1-carb)*hotfi[, i])
          })))
          if(verbose) cat("Sum of emissions:", sum(e), "\n")
          if(!missing(params)){
            for (i in 1:length(params)){
              e <- as.data.frame(e)
              e[, params[i]] <- params[i]
            }
          }

        }

      } else {
        e <- veh*x*(carb*(p*hotc+(1-p)*warmc)+(1-carb)*hotfi)
        if(!missing(params)){
          for (i in 1:length(params)){
            e <- as.data.frame(e)
            e[, params[i]] <- params[i]
          }
        }

      }
    } else if (carb < 0){
      stop("carb is a positive fraction or 0")

    } else {
      if(is.data.frame(veh)){
        if(!missing(pro_month)){
          if(length(pro_month) != 12) stop("Length of pro_month must be 12")
          mes <- ifelse(nchar(1:12)<2, paste0(0, 1:12), 1:12)
          e <- do.call("rbind", lapply(1:12, function(j){
            e <- do.call("cbind", lapply(1:ncol(veh), function(i){
              veh[, i]*x[i]*hotfi[[j]][, i]*pro_month[j]
            }))
            e <- Emissions(e)
            if(!missing(params)){
              for (i in 1:length(params)){
                e <- as.data.frame(e)
                e[, params[i]] <- params[i]
              }
            }
            e$month <- mes[j]
            e
          }))
          if(verbose) cat("Sum of emissions:", sum(e[-"month"]), "\n")

        } else {
          if(!is.data.frame(hotfi)) stop("'hotfi' must be a data.frame with the same dimensions of 'veh'")
          e <- Emissions(do.call("cbind",lapply(1:ncol(veh), function(i){
            veh[, i]*x[i]*hotfi[, i]
          })))
          if (verbose) cat("Sum of emissions:", sum(e), "\n")
          if(!missing(params)){
            for (i in 1:length(params)){
              e <- as.data.frame(e)
              e[, params[i]] <- params[i]
            }
          }

        }
      } else {
        e <- veh*x*hotfi
        if(!missing(params)){
          for (i in 1:length(params)){
            e <- as.data.frame(e)
            e[, params[i]] <- params[i]
          }
        }

      }

    }
  }
  return(e)
}
