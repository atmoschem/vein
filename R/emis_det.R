#' Determine deterioration factors for urban conditions
#'
#' @description \code{\link{emis_det}} returns deterioration factors. The emission
#' factors comes from the guidelines for developing emission factors of the
#' EMEP/EEA air pollutant emission inventory guidebook
#' http://www.eea.europa.eu/themes/air/emep-eea-air-pollutant-emission-inventory-guidebook
#' This function subset an internal database of emission factors with each argument
#'
#' @param po Character; Pollutant "CO", "NOx" or "HC"
#' @param cc Character; Size of engine in cc covering "<=1400", "1400_2000" or ">2000"
#' @param eu Character; Euro standard:  "I", "II", "III", "III", "IV", "V", "VI", "VIc"
#' @param speed Numeric; Speed to return Number of emission factor and not a function.
#' It needs units in km/h
#' @param km Numeric; accumulated mileage in km.
#' @param verbose Logical; To show more information
#' @param show.equation Option to see or not the equation parameters
#' @return It returns a numeric vector representing the increase in emissions due to normal deterioring
#' @keywords deterioration emission factors
#' @note The deterioration factors functions are available for technologies
#' euro "II", "III" and "IV". In order to cover all euro technologies, this
#' function assumes that the deterioration function of "III" and "IV" applies
#' for "V", "VI" and "VIc". However, as these technologies are relative
#' new,  accumulated milage is low and hence, deteerioration factors small.
#' @export
#' @examples \dontrun{
#' data(fkm)
#' pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)
#' km <- units::set_units(pckma[1:11], km)
#' # length eu = length km = 1
#' emis_det(po = "CO", cc = "<=1400", eu = "III", km = km[5], show.equation = TRUE)
#' # length eu = length km = 1, length speed > 1
#' emis_det(po = "CO", cc = "<=1400", eu = "III", km = km[5], speed = Speed(1:10))
#' # length km != length eu error
#' # (cod1 <- emis_det(po = "CO", cc = "<=1400", eu = c("III", "IV"), speed = Speed(30),
#' # km = km[4]))
#' # length eu = 1 length km > 1
#' emis_det(po = "CO", cc = "<=1400", eu = "III", km = km)
#' # length eu = 2, length km = 2 (if different length, error!)
#' (cod1 <- emis_det(po = "CO", cc = "<=1400", eu = c("III", "IV"), km = km[4:5]))
#' # length eu = 2, length km = 2, length speed > 1
#' (cod1 <- emis_det(po = "CO", cc = "<=1400", eu = c("III", "IV"), speed = Speed(0:130),
#' km = km[4:5]))
#' euros <- c("V","V","V", "IV", "IV", "IV", "III", "III", "III", "III")
#' # length eu = 2, length km = 2, length speed > 1
#' (cod1 <- emis_det(po = "CO", cc = "<=1400", eu = euros, speed = Speed(1:100),
#' km = km[1:10]))
#' cod1 <- as.matrix(cod1[, 1:11])
#' filled.contour(cod1, col = cptcity::cpt(6277, n = 20))
#' filled.contour(cod1, col = cptcity::lucky(n = 19))
#' euro <- c(rep("V", 5), rep("IV", 5), "III")
#' euros <- rbind(euro, euro)
#' (cod1 <- emis_det(po = "CO", cc = "<=1400", eu = euros, km = km))
#' }
emis_det <- function(po, cc, eu, speed = Speed(18.9), km, verbose = FALSE, show.equation = FALSE) {
  ldv_det <- sysdata$ldv_det

  # Check km
  if(!inherits(km, "units")){
    stop("km neeeds to has class 'units' in 'km'. Please, check package '?units::set_units'")
  }
  if(units(km)$numerator == "m" ){
    stop("Units of km is 'm' ")
  }
  if(units(km)$numerator == "km" ) {
    km <- as.numeric(km)
  }
  #Check cc
  if(is.numeric(cc)){
    cc <- ifelse(cc <= 1400, "<=1400",
                 ifelse(cc >= 1400 & cc < 2000,
                        "1400_2000", ">2000"
                 ))
  }
  #Check eu
  if(is.matrix(eu) | is.data.frame(eu)){
    eu <- as.data.frame(eu)
    for(i in 1:ncol(eu)) eu[, i] <- as.character(eu[, i])
  } else {
    eu = as.character(eu)
  }

  if(!inherits(speed, "units")){
    stop("speed neeeds to has class 'units' in 'km/h'. Please, check package '?units::set_units'")
  }
  if(units(speed)$numerator != "km" | units(speed)$denominator != "h"){
    stop("Units of g must be 'km/h' ")
  }
  if(units(speed)$numerator == "km" & units(speed)$denominator == "h"){
    speed <- as.numeric(speed)
  }

  if(!is.data.frame(eu)){
    if (any(eu %in% c("V", "VI", "VIc"))) {
      if (verbose) message("Assuming the same deterioration as euro III and IV")
    }
    if (length(eu) == 1 & length(km) == 1){
      df <- ldv_det[ldv_det$POLLUTANT == po &
                      ldv_det$CC == cc &
                      ldv_det$EURO == eu, ]
      if (show.equation) {
        cat(paste0("b = ", df$b, ", c = ", df$c, ", MAX MC URBAN = ", df$d,
                   ", \nf = ", df$f,", g = ", df$g , ", MAX MC ROAD = ", df$h, "\n"))
        cat("V < 19: MC_URBAN = b*km + c\n")
        cat("V >= 63: MC_ROAD = f*km + g\n")
        cat("19 <= V < 63: MC_URBAN + (V - 19)*(MC_ROAD - MC_URBAN)/44\n")
      }
      f1 <- function(V, km){
        a <- df$a; b <- df$b; c <- df$c; d <- df$d;
        e <- df$e; f <- df$f; g <- df$g; h <- df$h
        MC_URBAN <- ifelse(km<a, b*km+c, d)
        MC_ROAD <- ifelse(km<e, f*km+g, h)
        ifelse(
          V<19, MC_URBAN,
          ifelse(V>63, MC_ROAD,
                 MC_URBAN + (V - 19)*(MC_ROAD - MC_URBAN)/44))
        }
      mc <- f1(V = speed, km = km)
      if(length(speed) > 1) {
        if(verbose) message("As speed has many values, this a data.frame")
        mc <- as.data.frame(mc)
        mc$speed <- Speed(speed)
      }
      return(mc)

    } else if(length(km) > 1 & length(eu) == 1){
      if(length(eu) > length(km)) stop("length 'eu' cant be bigger than length 'km'")

      mc <- lapply(1:length(km), function(i){
        df <- ldv_det[ldv_det$POLLUTANT == po &
                        ldv_det$CC == cc &
                        ldv_det$EURO == eu, ]
        f1 <- function(V, km){
          a <- df$a; b <- df$b; c <- df$c; d <- df$d;
          e <- df$e; f <- df$f; g <- df$g; h <- df$h
          MC_URBAN <- ifelse(km<a, b*km+c, d)
          MC_ROAD <- ifelse(km<e, f*km+g, h)
          ifelse(
            V<19, MC_URBAN,
            ifelse(V>63, MC_ROAD,
                   MC_URBAN + (V - 19)*(MC_ROAD - MC_URBAN)/44))
        }
        ifelse(f1(V = speed, km = km[i]) < 1 , 1 ,
               f1(V = speed, km = km[i]))
      })
      if(length(speed) > 1) {
        if(verbose) message("As speed has many values, this a data.frame")
        mc <- do.call("cbind", mc)
        mc <- as.data.frame(mc)
        names(mc) <- paste0("km", 1:length(km))
        mc$speed <- Speed(speed)
      } else {
        if(verbose) message("As speed has 1 value, this a vector")
        mc <- unlist(mc)
      }
      return(mc)

    } else if(length(km) > 1 & length(eu) > 1){
      if(length(eu) != length(km)) stop("length 'eu' cant be bigger than length 'km'")
      mc <- lapply(1:length(km), function(i){
        df <- ldv_det[ldv_det$POLLUTANT == po &
                        ldv_det$CC == cc &
                        ldv_det$EURO == eu[i], ]
        f1 <- function(V, km){
          a <- df$a; b <- df$b; c <- df$c; d <- df$d;
          e <- df$e; f <- df$f; g <- df$g; h <- df$h
          MC_URBAN <- ifelse(km<a, b*km+c, d)
          MC_ROAD <- ifelse(km<e, f*km+g, h)
          ifelse(
            V<19, MC_URBAN,
            ifelse(V>63, MC_ROAD,
                   MC_URBAN + (V - 19)*(MC_ROAD - MC_URBAN)/44))
        }
        ifelse(f1(V = speed, km = km[i]) < 1 , 1 ,
               f1(V = speed, km = km[i]))
      })
      if(length(speed) > 1) {
        if(verbose) message("As speed has many values, this a data.frame")
        mc <- do.call("cbind", mc)
        mc <- as.data.frame(mc)
        names(mc) <- paste0("km", 1:length(km))
        mc$speed <- Speed(speed)
      } else {
        if(verbose) message("As speed has 1 value, this a vector")
        mc <- unlist(mc)
      }
      return(mc)

    } else {
      # print("aqui")
      f1 <- function(V, km){
        a <- df$a; b <- df$b; c <- df$c; d <- df$d;
        e <- df$e; f <- df$f; g <- df$g; h <- df$h
        MC_URBAN <- ifelse(km<a, b*km+c, d)
        MC_ROAD <- ifelse(km<e, f*km+g, h)
        ifelse(
          V<19, MC_URBAN,
          ifelse(V>63, MC_ROAD,
                 MC_URBAN + (V - 19)*(MC_ROAD - MC_URBAN)/44))
      }
      mc <- ifelse(f1(V = speed, km = km) <1, 1, f1(V = speed, km = km))
      return(mc)
      if(length(speed) > 1) {
        if(verbose) message("As speed has many values, this a data.frame")
        mc <- do.call("cbind", mc)
        mc <- as.data.frame(mc)
        mc$speed <- Speed(speed)
      } else {
        if(verbose) message("As speed has 1 value, this a vector")
        mc <- unlist(mc)
      }

        }
    return(mc)

  } else if(is.data.frame(eu) & !is.data.frame(km)){
    if(ncol(eu) != length(km)) stop("Length of km must be the same as number of columns of 'eu'")

    mc <- do.call("rbind", lapply(1:nrow(eu), function(j){
      dff <- do.call("cbind", lapply(1:ncol(eu), function(i){
        df <- ldv_det[ldv_det$POLLUTANT == po &
                        ldv_det$CC == cc &
                        ldv_det$EURO == eu[j,i], ]
        f1 <- function(V, km){
          a <- df$a; b <- df$b; c <- df$c; d <- df$d;
          e <- df$e; f <- df$f; g <- df$g; h <- df$h
          MC_URBAN <- ifelse(km<a, b*km+c, d)
          MC_ROAD <- ifelse(km<e, f*km+g, h)
          ifelse(
            V<19, MC_URBAN,
            ifelse(V>63, MC_ROAD,
                   MC_URBAN + (V - 19)*(MC_ROAD - MC_URBAN)/44))
          }
        ifelse(f1(V = speed, km = km[i]) < 1 , 1 ,
               f1(V = speed, km = km[i]))
      }))
      if(verbose) message("as 'eu' is a data.frame, this is a data.frame")
      dff <- as.data.frame(dff)
      dff$speed <- Speed(speed)
      dff
    }))

    return(mc)
  } else if(is.data.frame(eu) & is.data.frame(km)){

    mc <- do.call("rbind", lapply(1:nrow(eu), function(j){
      mc <- do.call("cbind", lapply(1:ncol(eu), function(i){
        df <- ldv_det[ldv_det$POLLUTANT == po &
                        ldv_det$CC == cc &
                        ldv_det$EURO == eu[j,i], ]
        f1 <- function(V, km){
          a <- df$a; b <- df$b; c <- df$c; d <- df$d;
          e <- df$e; f <- df$f; g <- df$g; h <- df$h
          MC_URBAN <- ifelse(km<a, b*km+c, d)
          MC_ROAD <- ifelse(km<e, f*km+g, h)
          ifelse(
            V<19, MC_URBAN,
            ifelse(V>63, MC_ROAD,
                   MC_URBAN + (V - 19)*(MC_ROAD - MC_URBAN)/44))
        }
        ifelse(f1(V = speed, km = km[i,j]) < 1 , 1 ,
               f1(V = speed, km = km[i,j]))
      }))
      if(verbose) message("as 'eu' is a data.frame, this is a data.frame")
      mc <- as.data.frame(mc)
      mc$speed <- Speed(speed)
    }))
    return(mc)
  }
}
