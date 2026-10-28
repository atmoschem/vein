#' Emissions factors of N2O and NH3
#'
#' \code{\link{ef_nitro}} returns emission factors as a functions of acondumulated mileage.
#' The emission factors comes from the guidelines  EMEP/EEA air pollutant
#' emission inventory guidebook
#' https://www.eea.europa.eu/en/analysis/publications/emep-eea-guidebook-2023
#'
#' @param v Category vehicle: "PC", "LCV", "Motorcycles_2S", "Motorcycles",
#' "Trucks", "Trucks-A", "Coach" and "BUS"
#' @param t Type: "Cold" or "Hot"
#' @param cond "Urban", "Rural", "Highway"
#' @param cc PC: "<=1400", "1400_2000", ">2000". LCV: "<3.5". Motorcycles: ">=50",
#' Motorcycles_2S, "<50", ">=50". Trucks: ">3.5", "7.5_12", "12_28", "28_34".
#' Trucks_A: ">34". BUS: "<=15", ">15 & <= 18". Coach:  "<=18", ">18"
#' @param f Type of fuel: "G", "D" or "LPG"
#' @param eu Euro standard: "PRE", "I", "II", "III", "IV", "V", "VI",
#' "VIc"
#' @param p Pollutant: "N2O", "NH3"
#' @param S Sulphur (ppm). Number.
#' @param k Multiplication factor
#' @param show.equation Option to see or not the equation parameters
#' @param cumileage Numeric; Acondumulated mileage to return number of emission factor and not a function.
#' @param fcorr Numeric; Correction by by euro technology.
#' @return an emission factor function which depends on the acondumulated mileage,
#' or an EmissionFactor
#' @keywords cumileage emission factors
#' @note if length of eu is bigger than 1, cumileage can have values  of length 1
#' or length equal to length of eu. When the length is equal to the length of
#' eu, each euro is evaluated at its own accumulated mileage and a data.frame
#' with one row is returned. Otherwise, each euro is evaluated at all values of
#' cumileage, with one row per value and a column `cumileage`.
#'
#' \code{fcorr} is applied (it was ignored before vein 2.0.0).
#' @importFrom units as_units
#' @export
#' @examples \dontrun{
#' efe10 <- ef_nitro(v = "PC", t = "Hot", cond = "Urban", f = "G", cc = "<=1400",
#' eu = "III", p = "NH3", S = 10,
#' show.equation = FALSE)
#' efe50 <- ef_nitro(v = "PC", t = "Hot", cond = "Urban", f = "G", cc = "<=1400",
#' eu = "III", p = "NH3", S = 50,
#' show.equation = TRUE)
#' efe10(10)
#' efe50(10)
#' efe10 <- ef_nitro(v = "PC", t = "Hot", cond = "Urban", f = "G", cc = "<=1400",
#' eu = "III", p = "NH3", S = 10, cumileage = units::set_units(25000, "km"))
#' }
ef_nitro <- function(v,
                     t = "Hot",
                     cond = "Urban",
                     cc,
                     f,
                     eu,
                     p = "NH3",
                     S = 10,
                     cumileage,
                     k = 1,
                     show.equation = FALSE,
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
    if(!inherits(cumileage, "units")){
      stop("cumileage neeeds to has class 'units' in 'km'. Please, check package '?units::set_units'")
    }
    if(units(cumileage) != units(units::as_units("km"))){
      stop("Units of cumileage must be 'km' ")
    }
    if(units(cumileage) == units(units::as_units("km"))){
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
                 ef$CONDITION  == cond &
                 ef$CC         == cc &
                 ef$FUEL       == f &
                 ef$EURO       == eu &
                 ef$POLLUTANT  == p, ]
      k2 <- lala(eu)

      Equation = paste0("(",as.character(df$Y), ")", "*", k, "*", k2)
      if (show.equation == TRUE) {
        cat(paste0("a = ", df$a, ", b = ", df$b, ", c = ", df$c, ", d = ", df$d,
                   ", e = ", df$e, ", f = ", df$f,  ", ef = ", df$ef, "\n"))
        cat(paste0("Equation = ", Equation, "\n"))
      }

      f1 <- function(km){
        a  = df$a; b  = df$b; ab = df$ab; c  = df$c; d  = df$d
        cd = df$cd; e  = df$e; f  = df$f; ef = df$ef
        S <- rep_len(S, length(km)) # ifelse(S...) must keep length(km)
        eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k, "*", k2)))
      }
      if(!missing(cumileage)){
        f1 <- EmissionFactors(rep_len(f1(cumileage), length(cumileage)))
        return(f1)
      } else {
        return(f1)
      }

    } else if(length(eu) > 1){
      if(!missing(cumileage)){
        # one mileage per euro (e.g. by age) or the same mileages for all
        per_eu <- length(cumileage) == length(eu)
        dff <- do.call("cbind", lapply(1:length(eu), function(i){
          df <- ef[ef$VEH        == v &
                     ef$TYPE       == t &
                     ef$CONDITION  == cond &
                     ef$CC         == cc &
                     ef$FUEL       == f &
                     ef$EURO       == eu[i] &
                     ef$POLLUTANT  == p, ]

          k2 <- lala(eu[i])

          f1 <- function(km){
            a  = df$a; b  = df$b; ab = df$ab; c  = df$c; d  = df$d
            cd = df$cd; e  = df$e; f  = df$f; ef = df$ef
            S <- rep_len(S, length(km)) # ifelse(S...) must keep length(km)
            eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k, "*", k2)))
          }
          if(per_eu) {
            rep_len(f1(cumileage[i]), 1)
          } else {
            rep_len(f1(cumileage), length(cumileage))
          }
        }))
        dff <- EmissionFactors(as.data.frame(dff))
        names(dff) <- paste0(eu, 1:length(eu))
        if(!per_eu) dff$cumileage <- cumileage
        return(dff)
      } else {
        dff <- lapply(1:length(eu), function(i){
          df <- ef[ef$VEH        == v &
                     ef$TYPE       == t &
                     ef$CONDITION  == cond &
                     ef$CC         == cc &
                     ef$FUEL       == f &
                     ef$EURO       == eu[i] &
                     ef$POLLUTANT  == p, ]

          k2 <- lala(eu[i])

          f1 <- function(km){
            a  = df$a; b  = df$b; ab = df$ab; c  = df$c; d  = df$d
            cd = df$cd; e  = df$e; f  = df$f; ef = df$ef
            S <- rep_len(S, length(km)) # ifelse(S...) must keep length(km)
            eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k, "*", k2)))
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
                   ef$CONDITION  == cond &
                   ef$CC         == cc &
                   ef$FUEL       == f &
                   ef$EURO == eu[j,i][[1]] &
                   ef$POLLUTANT  == p, ]
        k2 <- lala(eu[j,i][[1]])

        f1 <- function(km){
          a  = df$a; b  = df$b; ab = df$ab; c  = df$c; d  = df$d
          cd = df$cd; e  = df$e; f  = df$f; ef = df$ef
          S <- rep_len(S, length(km)) # ifelse(S...) must keep length(km)
          eval(parse(text = paste0("(",as.character(df$Y), ")", "*", k, "*", k2)))
        }
        if(length(cumileage) == 1) f1(cumileage) else f1(cumileage[i])
      }))
    }))
    dff <- EmissionFactors(dff)
    return(dff)
  }
}
