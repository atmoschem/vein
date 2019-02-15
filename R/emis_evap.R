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
#' @param carb fraction of gasoline vehicles with carburator or fuel return system.
#' @param p Fraction of trips finished with hot engine
#' @param hotc average running losses or soak evaporative factor for vehicles with
#' carburator or fuel return system
#' for vehicles with fuel injection and returnless fuel systems.
#'  If x has units 'lkm', the units of ed must be 'g/km',
#' @param warmc average cold and warm running losses or soak evaporative factor
#' for vehicles with carburator or fuel return system
#' for vehicles with fuel injection and returnless fuel systems.
#'  If x has units 'lkm', the units of ed must be 'g/km',
#' @param pro_month Numeric; montly profile to distribuite annual mileage in each month.
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
                      pro_month) { # hot or warm soak
  # Check y
  if(!missing(x)){
    if(units(x)$numerator != "km"){
      stop("units needs to be km")
    }
    # ed
    if(!missing(ed)){
      if(units(ed)$numerator != "g" | units(ed)$denominator != "km"){
        stop("Emission factor must be g/km ")
      }
    }
    # hotfi
    if(!missing(x) & !missing(hotfi)){
      if(units(hotfi)$numerator != "g" | units(hotfi)$denominator != "km"){
        stop("Emission factor must be g/km ")
      }
    }
    # hotc
    if(!missing(x) & !missing(hotc)){
      if(units(hotc)$numerator != "g" | units(hotc)$denominator != "km"){
        stop("Emission factor must be g/km ")
      }
    }
    # warmc
    if(!missing(x) & !missing(warmc)){
      if(units(warmc)$numerator != "g" | units(warmc)$denominator != "km"){
        stop("Emission factor must be g/km ")
      }
    }
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



  # ed
  if(!missing(ed)){
    if(is.data.frame(veh)){

      if(!is.data.frame(ed)) stop("as veh is a data.frame ed needs to be a data.frame qith the same dimensions")

      if(!missing(pro_month)){
        if(length(pro_month) != 12) stop("Length of pro_month must be 12")
        mes <- ifelse(nchar(1:12)<2, paste0(0, 1:12), 1:12)
        e <- do.call("rbind", lapply(1:12, function(j){
          e <- do.call("cbind", lapply(1:ncol(veh), function(i){
            veh[, i]*x[i]*ed[[j]][, i]*pro_month[j]
          }))
          e <- Emissions(e)
          e$month <- mes[j]
          e
        }))
      } else {
        e <- Emissions(do.call("cbind", lapply(1:ncol(veh), function(i){
          veh[, i]*x[i]*ed[, i]
        })))

      }
    } else {
      e <- Emissions(veh*x*ed)
    }

  } else {
    if(carb > 0){
      if(is.data.frame(veh)){
        if(!is.data.frame(hotc)) stop("as hotc is a data.frame ed needs to be a data.frame qith the same dimensions")
        if(!is.data.frame(warmc)) stop("as warmc is a data.frame ed needs to be a data.frame qith the same dimensions")

        if(!missing(pro_month)){
          if(length(pro_month) != 12) stop("Length of pro_month must be 12")
          mes <- ifelse(nchar(1:12)<2, paste0(0, 1:12), 1:12)
          e <- do.call("rbind", lapply(1:12, function(j){
            e <- do.call("cbind", lapply(1:ncol(veh), function(i){
              veh[, i]*x[i]*(carb*(p*hotc[[j]][, i]+(1-p)*warmc[[j]][, i])+(1-carb)*hotfi[[j]][, i])*pro_month[j]
            }))
            e <- Emissions(e)
            e$month <- mes[j]
            e
          }))

        } else{
          e <- Emissions(do.call("cbind",lapply(1:ncol(veh), function(i){
            veh[, i]*x[i]*(carb*(p*hotc[, i]+(1-p)*warmc[, i])+(1-carb)*hotfi[, i])
          })))

        }

      } else {
        e <- veh*x*(carb*(p*hotc+(1-p)*warmc)+(1-carb)*hotfi)
      }
    } else if (carb < 0){
      stop("carb is a positive fraction or 0")

    } else {
      if(is.data.frame(veh)){
        if(!is.data.frame(hotfi)) stop("as hotfi is a data.frame ed needs to be a data.frame qith the same dimensions")
        if(!missing(pro_month)){
          if(length(pro_month) != 12) stop("Length of pro_month must be 12")
          mes <- ifelse(nchar(1:12)<2, paste0(0, 1:12), 1:12)
          e <- do.call("rbind", lapply(1:12, function(j){
            e <- do.call("cbind", lapply(1:ncol(veh), function(i){
              veh[, i]*x[i]*hotfi[[j]][, i]*pro_month[j]
            }))
            e <- Emissions(e)
            e$month <- mes[j]
            e
          }))

        } else {
          e <- Emissions(do.call("cbind",lapply(1:ncol(veh), function(i){
            veh[, i]*x[i]*hotfi[, i]
          })))

        }
      } else {
        e <- veh*x*hotfi
      }
    }
  }

  return(e)
}
