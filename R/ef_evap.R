#' Evaporative emission factor
#'
#' @description \code{\link{ef_evap}} is a lookup table with tier 2 evaporative emission factors
#' from EMEP/EEA emisison guidelines
#'
#' @param ef Name of  evaporative emission factor as *eshotc*: mean hot-soak with
#' carburator, *eswarmc*: mean cold and warm-soak with carburator, eshotfi: mean
#' hot-soak with fuel injection, *erhotc*: mean hot running losses with
#' carburator, *erwarmc* mean cold and warm running losses, *erhotfi* mean hot
#' running losses with fuel injection. Length of ef 1.
#' @param v Type of vehicles, "PC", "Motorcycle", "Motorcycle_2S" and "Moped"
#' @param cc Size of engine in cc. PC "<=1400",  "1400_2000" and ">2000"
#' Motorcycle_2S:  "<=50". Motorcyces: ">50", "<=250", "250_750" and ">750".
#' Only engines of >750 has canister.
#' @param dt Character or Numeric: Average monthly temperature variation: "-5_10", "0_15", "10_25"
#' and "20_35". This argument can vector with several elements. dt can also be data.frame,
#' but it is recommended that the number of columns are each month. So that dt
#' varies in each row and each column.
#' @param ca Size of canister: "no" meaning no canister, "small", "medium" and
#' "large".
#' @param pollutant Character indicating any of the covered pollutants: "NMHC",
#' "ethane", "propane", "i-butane", "n-butane", "i-pentane", "n-pentane",
#' "2-methylpentane", "3-methylpentane", "n-hexane", "n-heptane", "propene",
#' "trans-2-butene", "isobutene", "cis-2-butene", "1,3-butadiene",
#' "trans-2-pentene", "cis-2-pentene", "isoprene", "propyne", "acetylene",
#' "benzene", "toluene", "ethylbenzene", "m-xylene", "o-xylene",
#' "1,2,4-trimethylbenzene" and "1,3,5-trimethylbenzene". Default is "NMHC"
#' @param ltrip Numeric; Length of trip. Experimental feature to conter g/trip
#' and g/proced (assuming proced similar to trip) in g/km.
#' @param kmday Numeric; average daily mileage. Experimental option
#' to convert g/day in g/km.
#' it is an information more solid than to know the average number of trips per day.
#' @param k multiplication factor
#' @param show when TRUE shows row of table with respective emission factor.
#' @note Diurnal loses occur with daily temperature variations. Running loses
#' occur during vehicles use. Hot soak emission occur following vehicles use.
#' @param verbose Logical; To show more information
#' @return emission factors in g/trip or g/proced. The object has class (g)
#' but it order to know it is g/trip or g/proceed the argument show must by T
#' @references Mellios G and Ntziachristos 2016. Gasoline evaporation. In:
#' EEA, EMEP. EEA air pollutant emission inventory guidebook-2009. European
#' Environment Agency, Copenhagen, 2009
#' @importFrom data.table rbindlist
#' @export
#' @examples \dontrun{
#' # Do not run
#' a <- ef_evap(ef = "eshotc", v = "PC", cc = "<=1400", dt = "0_15", ca = "no",
#' pollutant = "cis-2-pentene")
#' a <- ef_evap(ef = "ed", v = "PC", cc = "<=1400", dt = "0_15", ca = "no",
#' show = TRUE)
#' a <- ef_evap(ef = c("erhotc", "erhotc"), v = "PC", cc = "<=1400",
#' dt = "0_15", ca = "no",
#' show = TRUE)
#' a <- ef_evap(ef = c("erhotc", "erhotc"), v = "PC", cc = "<=1400",
#'  dt = "0_15", ca = "no",
#' show = FALSE)
#' a <- ef_evap(ef = "eshotc", v = "PC", cc = "<=1400", dt = "0_15", ca = "no",
#' show = TRUE)
#' ef_evap(ef = "erhotc", v = "PC", cc = "<=1400", dt = "0_15", ca = "no",
#' show = TRUE)
#' temps <- 10:20
#' a <- ef_evap(ef = "erhotc", v = "PC", cc = "<=1400", dt = temps, ca = "no",
#' show = TRUE)
#' dt <- matrix(rep(1:24,5), ncol = 12) # 12 months
#' dt <- celsius(dt)
#' a <- ef_evap(ef ="erhotc", v = "PC", cc = "<=1400",
#' dt = dt, ca = "no")
#' lkm <- units::set_units(10, km)
#' a <- ef_evap(ef ="erhotc", v = "PC", cc = "<=1400", ltrip = lkm,
#' dt = dt, ca = "no")
#' }
ef_evap <- function (ef, v, cc, dt, ca, pollutant = "NMHC",
                     k = 1, ltrip,  kmday, show = FALSE,
                     verbose = FALSE){
  a <- (-5+10)/2
  b <-   (0+15)/2
  c <- (10+25)/2
  d <- (20+35)/2
  ta <- "-5_10"
  tb <- "0_15"
  tc <- "10_25"
  td <- "20_35"
  ef_ev <- sysdata$ev
  efs <- as.character(unique(ef_ev$ef))
  if(any(!ef %in% efs)) {
    cat("Please, select any of: \n")
    print(efs)
    cat("\n")
    stop()
  }
  # Check ltrip
  if(!missing(ltrip) & !missing(kmday)){
    stop("You can convert to g/km runing losses and soak with ltrip OR diurnal with kmday. Not both at the same time")
  }
  if(is.matrix(dt)) dt <- as.data.frame(dt)
  # Checking length of ef and ltrip and kmday
  if(!missing(ltrip)){
    if(length(ltrip) > 1) stop("Please, enter one value of 'ltrip'")
    # Check units
    if(!inherits(ltrip, "units")){
      stop("ltrip neeeds to has class 'units' in 'km'. Please, check package '?units::set_units'")
    }
    if(units(ltrip)$numerator == "m" ){
      stop("Units of ltrip is 'm'. Please, check package '?units::set_units'")
    }
    if(units(ltrip)$numerator == "km" ){
      ltrip <- as.numeric(ltrip)
    }

  }
  # Check kmday
  if(!missing(kmday)){
    if(length(kmday) > 1) stop("Please, enter one value of 'kmday'")
    # Check units
    if(!inherits(kmday, "units")){
      stop("kmday neeeds to has class 'units' in 'km'. Please, check package '?units::set_units'")
    }
    if(units(kmday)$numerator == "m" ){
      stop("Units of kmday is 'm'. Please, check package '?units::set_units'")
    }
    if(units(kmday)$numerator == "km" ){
      kmday <- as.numeric(kmday)
    }
  }
  # check(dt)
    dt <- remove_units(dt)



  if(!is.data.frame(dt)){
    # if(class(dt) == "factor"){
    #   stop("Please, enter numeric or character")
    # }
    if(is.numeric(dt)){
      dt = ifelse(dt < a, ta,
                  ifelse(dt >=a & dt < b, tb,
                         ifelse(dt >= b & dt < c, tc, td
                         )))
    }

    if(length(dt) == 1 & length(ef) == 1){
      df <- ef_ev[ef_ev$ef == ef &
                    ef_ev$veh %in% v &
                    ef_ev$cc %in% cc &
                    ef_ev$dt == dt &
                    ef_ev$canister %in% ca &
                    ef_ev$pollutant == pollutant, ]

    } else if (length(dt) > 1 & length(ef) == 1){
      df <- do.call("rbind",
                    lapply(1:length(dt), function(i){
                      ef_ev[ef_ev$ef == ef &
                              ef_ev$veh %in% v &
                              ef_ev$cc %in% cc &
                              ef_ev$dt == dt[i] &
                              ef_ev$canister %in% ca &
                              ef_ev$pollutant == pollutant, ]

                    }))
    } else if(length(dt) == 1 & length(ef) > 1){
      df <- do.call("rbind",
                    lapply(1:length(ef), function(i){
                      ef_ev[ef_ev$ef == ef[i] &
                              ef_ev$veh %in% v &
                              ef_ev$cc %in% cc &
                              ef_ev$dt == dt &
                              ef_ev$canister %in% ca &
                              ef_ev$pollutant == pollutant, ]

                    }))


    } else if(length(dt) > 1 & length(ef) > 1){
      df <- do.call("rbind", lapply(1:length(dt), function(j){
        do.call("rbind",
                lapply(1:length(ef), function(i){
                  ef_ev[ef_ev$ef == ef[i] &
                          ef_ev$veh %in% v &
                          ef_ev$cc %in% cc &
                          ef_ev$dt == dt[j] &
                          ef_ev$canister %in% ca &
                          ef_ev$pollutant == pollutant, ]
                }))
      }))
    }



    if(!missing(ltrip) ){
      if (any(ef  %in% "ed")) stop ("ef must cannot be Diurnal Evaporative ed")
      df$g <- EmissionFactors(df$g*k/ltrip)
      df$units <- NULL
      if (show) {
        return(df)
      } else {
        return(df$g)
      }

    } else if (!missing(kmday)){
      if (any(!ef  %in%  c("eshotfi", "eswarmc", "eshotc",
                           "erhotfi", "erwarmc", "erhotc"))){
        stop ("ef must cannot be Diurnal Evaporative ed")
      }
      df$g <- EmissionFactors(df$g*k/kmday)
      df$units <- NULL
      if(show){
        return(df)
      } else {
        return(df)
      }
    }


    df$g <- Emissions(df$g)
    if (show == TRUE) {
      return(df)
    } else {
      return(df$g)
    }

  } else if (is.data.frame(dt)){
    if(is.numeric(dt[, 1])){
      for(i in 1:ncol(dt)){
        dt[, i] = ifelse(dt[, i] < a, ta,
                         ifelse(dt[, i] >=a & dt[, i] < b, tb,
                                ifelse(dt[, i] >= b & dt[, i] < c, tc, td
                                )))
      }
    }
    if(length(v) > 1 | length(cc) > 1 | length(ca) > 1){
      stop("When dt is data.frame, lengths of 'v', 'cc' and 'ca' must be 1, and perhaps only works with direct injection")
    }

    if(ncol(dt) == 1 & length(ef) == 1){
      df <- do.call("rbind",
                    lapply(1:nrow(dt), function(i){
                      ef_ev[ef_ev$ef == ef &
                              ef_ev$veh == v &
                              ef_ev$cc == cc &
                              ef_ev$dt == unlist(dt)[i] &
                              ef_ev$canister == ca, ]$g
                    }))
      if(!missing(ltrip)){
        return(EmissionFactors(df*k/ltrip))
      } else if(!missing(kmday)){
        return(EmissionFactors(df*k/kmday))
      } else if(missing(ltrip) & missing(kmday)){
        return(Emissions(df*k))
      }
    } else if (ncol(dt) > 1 & length(ef) == 1){
      df <- do.call("rbind", lapply(1:ncol(dt), function(j){
        df <- do.call("rbind",
                      lapply(1:nrow(dt), function(i){
                        ef_ev[ef_ev$ef == ef &
                                ef_ev$veh == v &
                                ef_ev$cc == cc &
                                ef_ev$dt == dt[i, j] &
                                ef_ev$canister == ca &
                                ef_ev$pollutant == pollutant, ]$g
                      }))
        df <- as.data.frame(df)
        names(df) <- "ef"
        df$cols_temp <- j
        df
      }))
      if(!missing(ltrip)){
        df$ef <- EmissionFactors(df$ef*k/ltrip)
        return(df)
      } else if(!missing(kmday)){
        df$ef <- EmissionFactors(df$ef*k/kmday)
        return(df)
      } else if(missing(ltrip) & missing(kmday)){
        df$ef <- Emissions(df$ef*k)
        return(df)
      }
    } else if(ncol(dt) == 1 & length(ef) > 1){
      df <- do.call("cbind", lapply(1:length(ef), function(j){
        do.call("rbind",
                lapply(1:nrow(dt), function(i){
                  ef_ev[ef_ev$ef == ef[j] &
                          ef_ev$veh == v &
                          ef_ev$cc == cc &
                          ef_ev$dt == unlist(dt)[i] &
                          ef_ev$canister == ca &
                          ef_ev$pollutant == pollutant, ]$g
                }))
      }))
      if(!missing(ltrip)){
        return(EmissionFactors(df*k/ltrip))
      } else if(!missing(kmday)){
        return(EmissionFactors(df*k/kmday))
      } else if(missing(ltrip) & missing(kmday)){
        return(Emissions(df*k))
      }
    } else if(length(dt) > 1 & length(ef) > 1){
      df <- do.call("rbind", lapply(1:ncol(dt), function(k){
        df <- do.call("cbind", lapply(1:length(ef), function(j){
          do.call("rbind",
                  lapply(1:nrow(dt), function(i){
                    ef_ev[ef_ev$ef == ef[j] &
                            ef_ev$veh == v &
                            ef_ev$cc == cc &
                            ef_ev$dt == dt[i, k] &
                            ef_ev$canister == ca &
                            ef_ev$pollutant == pollutant, ]$g
                  }))
        }))
      }))
      if(!missing(ltrip)){

        df <- EmissionFactors(df*k/ltrip)
        df$col_temp <- rep(1:ncol(dt), each = nrow(dt))
      } else if(!missing(kmday)){

        df <- EmissionFactors(df*k/kmday)
        df$col_temp <- rep(1:ncol(dt), each = nrow(dt))
      } else if(missing(ltrip) & missing(kmday)){
        df <- Emissions(df*k)
        df$col_temp <- rep(1:ncol(dt), each = nrow(dt))
      }

      return(df)
    }

  }


}
