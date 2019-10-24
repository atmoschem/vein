#' Estimation of evaporative emissions
#'
#' @description \code{\link{emis_evap}} estimates evaporative emissions from
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
#' of ed must be 'g/km', other case, this are simply g/day (without units).
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
#' @seealso \code{\link{ef_evap}}
#' @importFrom units as_units
#' @references Mellios G and Ntziachristos 2016. Gasoline evaporation. In:
#' EEA, EMEP. EEA air pollutant emission inventory guidebook-2009. European
#' Environment Agency, Copenhagen, 2009
#' @note  When veh is a "Vehicles" data.frame, emission factors are evaluated till the
#' number of columns of veh. For instance, if the length of the emision factor is 20
#' but the number of columns of veh is 10, the 10 first emission factors are used.
#'
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
                      verbose = FALSE) {
  # Check y
  if(!missing(x)){
    if(units(x)$numerator == "km"){
      if(verbose) message('Emission factors must have units g/km')
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
  # pro_month
  if(!missing(pro_month)){
    if(is.data.frame(pro_month) | is.matrix(pro_month)){
      pro_month <- as.data.frame(pro_month)
      for(i in 1:nrow(pro_month)){
        pro_month[i, ] <- pro_month[i, ]/sum(pro_month[i, ])
      }
    } else if (is.numeric(pro_month)){
      pro_month <- pro_month/sum(pro_month)
    }
  }


  # ed
  if(!missing(ed)){
    if(is.data.frame(veh)){

      if(!is.data.frame(ed)) stop("as veh is a data.frame ed needs to be a data.frame")

      if(!missing(pro_month)){
        if(length(pro_month) != 12) stop("Length of pro_month must be 12")
        mes <- ifelse(nchar(1:12) < 2, paste0(0, 1:12), 1:12)

        ed$month <- ed$month <- rep(1:12, each = nrow(veh))
        ed <- split(ed, ed$month)

        if(is.data.frame(pro_month)){
          e <- do.call("rbind", lapply(1:12, function(j){
            e <- unlist(lapply(1:ncol(veh), function(i){
              veh[, i]*x[i]*ed[[j]][, i]*pro_month[, j]
            }))
            e <- as.data.frame(e)
            names(e) <- "emissions"
            e <- Emissions(e)
            e$rows <- row.names(veh)
            e$age <- rep(1:ncol(veh), each = nrow(veh))
            e$month <- (1:length(pro_month))[j]
            e
          }))

        } else if(is.numeric(pro_month)){
          e <- do.call("rbind", lapply(1:12, function(j){
            e <- unlist(lapply(1:ncol(veh), function(i){
              veh[, i]*x[i]*ed[[j]][, i]*pro_month[j]
            }))
            e <- as.data.frame(e)
            names(e) <- "emissions"
            e <- Emissions(e)
            e$rows <- row.names(veh)
            e$age <- rep(1:ncol(veh), each = nrow(veh))
            e$month <- (1:length(pro_month))[j]
            e
          }))

        }
        if(verbose) cat("Sum of emissions:", sum(e$emissions), "\n")

      } else {
        if(!is.data.frame(ed)) stop("'ed' must be a data.frame")
        e <- as.data.frame(unlist(lapply(1:ncol(veh), function(i){
          veh[, i]*x[i]*ed[, i]
        })))
        names(e) <- "emissions"
        e <- Emissions(e)
        e$rows <- row.names(veh)
        e$age <- rep(1:ncol(veh), each = nrow(veh))
        if(verbose) cat("Sum of emissions:", sum(e$emissions), "\n")
      }
    } else {
      e <- Emissions(veh*x*ed)
      if(verbose) cat("Sum of emissions:", sum(e), "\n")
    }

  } else {
    if(carb > 0){
      if(is.data.frame(veh)){

        if(!missing(pro_month)){
          if(length(pro_month) != 12) stop("Length of pro_month must be 12")
          mes <- ifelse(nchar(1:12)<2, paste0(0, 1:12), 1:12)

          warmc$month <- hotc$month <- rep(1:12, each = nrow(veh))
          warmc <- split(warmc, warmc$month)
          hotc <- split(hotc, warmc$month)

          if(is.data.frame(pro_month)){
            e <- do.call("rbind", lapply(1:12, function(j){
              e <- unlist(lapply(1:ncol(veh), function(i){
                veh[, i]*x[i]*(carb*(p*hotc[[j]][, i]+(1-p)*warmc[[j]][, i])+(1-carb)*hotfi[[j]][, i])*pro_month[, j]
              }))
              e <- as.data.frame(e)
              names(e) <- "emissions"
              e <- Emissions(e)
              e$rows <- row.names(veh)
              e$age <- rep(1:ncol(veh), each = nrow(veh))
              e$month <- (1:length(pro_month))[j]
              e
            }))

          } else if (is.numeric(pro_month)){
            e <- do.call("rbind", lapply(1:12, function(j){
              e <- unlist(lapply(1:ncol(veh), function(i){
                veh[, i]*x[i]*(carb*(p*hotc[[j]][, i]+(1-p)*warmc[[j]][, i])+(1-carb)*hotfi[[j]][, i])*pro_month[j]
              }))
              e <- as.data.frame(e)
              names(e) <- "emissions"
              e <- Emissions(e)
              e$rows <- row.names(veh)
              e$age <- rep(1:ncol(veh), each = nrow(veh))
              e$month <- (1:length(pro_month))[j]
              e
            }))

          }
          if(verbose) cat("Sum of emissions:", sum(e$emissions), "\n")

        } else {
          if(!is.data.frame(hotc)) stop("hotc' must be a data.frame")
          if(!is.data.frame(warmc)) stop("warmc' must be a data.frame")

          e <- unlist(lapply(1:ncol(veh), function(i){
            veh[, i]*x[i]*(carb*(p*hotc[, i]+(1-p)*warmc[, i])+(1-carb)*hotfi[, i])
          }))
          e <- as.data.frame(e)
          names(e) <- "emissions"
          e <- Emissions(e)
          e$rows <- row.names(veh)
          e$age <- rep(1:ncol(veh), each = nrow(veh))
          if(verbose) cat("Sum of emissions:", sum(e$emissions), "\n")
        }

      } else {
        e <- veh*as.numeric(x)*(carb*(p*hotc+(1-p)*warmc)+(1-carb)*hotfi)
        e <- Emissions(e)
      }
    } else if (carb < 0){
      stop("carb is a positive fraction or 0")

    } else {
      if(is.data.frame(veh)){
        if(!missing(pro_month)){
          if(length(pro_month) != 12) stop("Length of pro_month must be 12")
          # mes <- ifelse(nchar(1:12)<2, paste0(0, 1:12), 1:12)

          hotfi$month <- rep(1:12, each = nrow(veh))
          hotfi <- split(hotfi, hotfi$month)

          if(is.data.frame(pro_month)){
            e <- do.call("rbind", lapply(1:12, function(j){
              e <- unlist(lapply(1:ncol(veh), function(i){
                veh[, i]*x[i]*hotfi[[j]][, i]*pro_month[, j]
              }))
              e <- as.data.frame(e)
              names(e) <- "emissions"
              e <- Emissions(e)
              e$rows <- row.names(veh)
              e$age <- rep(1:ncol(veh), each = nrow(veh))
              e$month <- (1:length(pro_month))[j]
              e
            }))
          } else if (is.numeric(pro_month)){
            e <- do.call("rbind", lapply(1:12, function(j){
              e <- unlist(lapply(1:ncol(veh), function(i){
                veh[, i]*x[i]*hotfi[[j]][, i]*pro_month[j]
              }))
              e <- as.data.frame(e)
              names(e) <- "emissions"
              e <- Emissions(e)
              e$rows <- row.names(veh)
              e$age <- rep(1:ncol(veh), each = nrow(veh))
              e$month <- (1:length(pro_month))[j]
              e
            }))

          }
          if(verbose) cat("Sum of emissions:", sum(e$emissions), "\n")

        } else {
          if(!is.data.frame(hotfi)) stop("'hotfi' must be a data.frame")
          e <- as.data.frame(unlist("rbind",lapply(1:ncol(veh), function(i){
            veh[, i]*x[i]*hotfi[, i]
          })))
          names(e) <- "emissions"
          e <- Emissions(e)
          e$rows <- row.names(veh)
          e$age <- rep(1:ncol(veh), each = nrow(veh))
          if (verbose) cat("Sum of emissions:", sum(e$emissions), "\n")
        }
      } else {
        e <- veh*x*hotfi
      }
    }
  }
  if(!missing(params)){
    if(is.data.frame(e)){
      for (i in 1:length(params)){
        e[, names(params)[i]] <- params[[i]]
      }
    } else {
      for (i in 1:length(params)){
        e <- as.data.frame(e)
        e[, names(params)[i]] <- params[[i]]
      }

    }
  }

  return(e)
}
