#' Emissions factors of N2O and NH3
#'
#' \code{\link{ef_nitro}} returns emission factors as a functions of accumulated mileage.
#' The emission factors comes from the guidelines  EMEP/EEA air pollutant
#' emission inventory guidebook
#' http://www.eea.europa.eu/themes/air/emep-eea-air-pollutant-emission-inventory-guidebook
#'
#' @param v Category vehicle: "PC", "LCV", "Motorcycles_2S", "Motorcycles",
#' "Trucks", "Trucks-A", "Coach" and "BUS"
#' @param t Type: "Cold" or "Hot"
#' @param cc "Urban", "Rural", "Highway"
#' @param f Type of fuel: "G", "D" or "LPG"
#' @param eu Euro standard: "PRE", "I", "II", "III", "IV", "V", "VI",
#' "VIc"
#' @param p Pollutant: "N2O", "NH3"
#' @param S Sulphur (ppm). Number.
#' @param k Multiplication factor
#' @param show.equation Option to see or not the equation parameters
#' @param cumileage Numeric; Accumulated mileage to return number of emission factor and not a function.
#' @param fcorr Numeric; Correction by by euro technology.
#' @return an emission factor function which depends on the accumulated mileage,
#' or an EmissionFactor
#' @keywords cumileage emission factors
#' @export
#' @examples {
#' efe10 <- ef_nitro(v = "PC", t = "Hot", cc = "Urban", f = "G",
#' eu = "III", p = "NH3", S = 10,
#' show.equation = FALSE)
#' efe50 <- ef_nitro(v = "PC", t = "Hot", cc = "Urban", f = "G",
#' eu = "III", p = "NH3", S = 50,
#' show.equation = TRUE)
#' efe10(10)
#' efe50(10)
#' }
ef_nitro <- function(v,
                     t = "Hot",
                     cc = "Urban",
                     f,
                     eu,
                     p = "NH3",
                     S = 10,
                     cumileage,
                     k = 1,
                     show.equation = TRUE,
                     fcorr = rep(1, 8)) {
  ef <- sysdata$nitro

  #Check eu
  if(is.matrix(eu) | is.data.frame(eu)){
    eu <- as.data.frame(eu)
    for(i in 1:ncol(eu)) eu[, i] <- as.character(eu[, i])
  } else {
    eu = as.character(eu)
  }

  # Check cumileage
  if(!missing(cumileage)){
    if(class(cumileage) != "units"){
      stop("cumileage neeeds to has class 'units' in 'km'. Please, check package '?units::set_units'")
    }
    if(units(cumileage)$numerator != "km"){
      stop("Units of g must be 'km' ")
    }
    if(units(cumileage)$numerator == "km"){
      cumileage <- as.numeric(cumileage)
    }
  }

  #Function to case when
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
                       fcorr[8])))))))}

  # fun starts
  if(!is.data.frame(eu)){
    if(length(eu) == 1){
      df <- ef[ef$VEH        == v &
                 ef$TYPE       == t &
                 ef$CONDITION  == cc &
                 ef$FUEL       == f &
                 ef$EURO       == eu &
                 ef$POLLUTANT  == p, ]
      k2 <- lala(eu)

      Equation = paste0("(",as.character(df$Y), ")", "*", k)
      if (show.equation == TRUE) {
        cat(paste0("a = ", df$a, ", b = ", df$b, ", c = ", df$c, ", d = ", df$d,
                   ", e = ", df$e, ", f = ", df$f,  ", ef = ", df$eff, "\n"))
        cat(paste0("Equation = ", Equation, "\n"))
      }

      f1 <- function(km){
        a  = df$a; b  = df$b; ab = df$ab; c  = df$c; d  = df$d
        cd = df$cd; e  = df$e; f  = df$f; ef = df$ef
        eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k)))
      }
      if(!missing(cumileage)){
        f1 <- EmissionFactors(f1(cumileage))
        return(f1)
      } else {
        return(f1)
      }

    } else if(length(eu) > 1){
      if(!missing(cumileage)){
        dff <- do.call("cbind", lapply(1:length(eu), function(i){
          df <- ef[ef$VEH        == v &
                     ef$TYPE       == t &
                     ef$CONDITION  == cc &
                     ef$FUEL       == f &
                     ef$EURO       == eu[i] &
                     ef$POLLUTANT  == p, ]

          k2 <- lala(eu[i])

          f1 <- function(km){
            a  = df$a; b  = df$b; ab = df$ab; c  = df$c; d  = df$d
            cd = df$cd; e  = df$e; f  = df$f; ef = df$ef
            eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k)))
          }
          f1(cumileage)
        }))
        dff <- EmissionFactors(dff)
        names(dff) <- paste0(eu, 1:length(eu))
        dff$cumileage <- cumileage
        return(dff)
      } else {
        dff <- lapply(1:length(eu), function(i){
          df <- ef[ef$VEH        == v &
                     ef$TYPE       == t &
                     ef$CONDITION  == cc &
                     ef$FUEL       == f &
                     ef$EURO       == eu[i] &
                     ef$POLLUTANT  == p, ]

          k2 <- lala(eu[i])

          f1 <- function(km){
            a  = df$a; b  = df$b; ab = df$ab; c  = df$c; d  = df$d
            cd = df$cd; e  = df$e; f  = df$f; ef = df$ef
            eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k)))
          }
          f1
        })
        names(dff) <- paste0(eu, 1:length(eu))
        return(EmissionFactorsList(dff))
      }
    }
    # New stuffs!
  } else if (is.data.frame(eu)){
    if(missing(cumileage)) stop("Add 'cumileage' please")
    dff <- do.call("rbind", lapply(1:nrow(eu), function(j){
      do.call("cbind", lapply(1:ncol(eu), function(i){
        df <- ef[ef$VEH        == v &
                   ef$TYPE       == t &
                   ef$CONDITION  == cc &
                   ef$FUEL       == f &
                   ef$EURO == eu[j,i][[1]] &
                   ef$POLLUTANT  == p, ]
        k2 <- lala(eu[j,i][[1]])

        f1 <- function(km){
          a  = df$a; b  = df$b; ab = df$ab; c  = df$c; d  = df$d
          cd = df$cd; e  = df$e; f  = df$f; ef = df$ef
          eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k)))
        }
        f1(cumileage)
      }))
    }))
    dff <- EmissionFactors(dff)
    dff$cumileage <- cumileage
    dff$row_eu <- rep(1:nrow(eu), each = length(cumileage))
    return(dff)
  }
}
