#' Determine deterioration factors for urban conditions
#'
#' @description \code{\link{emis_det}} returns deterioration factors. The emission
#' factors comes from the guidelines for developing emission factors of the
#' EMEP/EEA air pollutant emission inventory guidebook
#' https://www.eea.europa.eu/en/analysis/publications/emep-eea-guidebook-2023
#' This function subset an internal database of emission factors with each argument
#'
#' @param po Character; Pollutant "CO", "NOx" or "HC"
#' @param cc Character; Size of engine in cc covering "<=1400", "1400_2000" or
#' ">2000". If numeric, cc <= 1400 is "<=1400", 1400 < cc <= 2000 is
#' "1400_2000" and cc > 2000 is ">2000".
#' @param eu Character; Euro standard: "PRE", "I", "II", "III", "IV", "V",
#' "VI", "VIc", "VId" or "VII". It can be a data.frame (rows of streets,
#' columns of ages).
#' @param speed Speed in km/h (class "Speed" or "units"). Default 18.9 km/h,
#' which gives the urban curve.
#' @param km Accumulated mileage with class "units" (e.g. km), one value per
#' age (per element of \code{eu}, or per column when \code{eu} is a data.frame).
#' @param verbose Logical; To show more information
#' @param show.equation Option to see or not the equation parameters
#' @return A dimensionless multiplier (>= 1) for each element of \code{km},
#' i.e. one value per age. With more than one speed the result is a
#' data.frame with one column per age and a column speed.
#' @keywords deterioration emission factors
#' @note The deterioration functions of the guidebook cover petrol catalyst
#' passenger cars (and are commonly used for petrol light commercial
#' vehicles). Pre-Euro vehicles return 1. They should not be applied to
#' diesel vehicles, motorcycles or heavy-duty vehicles.
#'
#' The guidebook gives functions for Euro I to IV; "V", "VI", "VIc", "VId"
#' and "VII" use the functions of "III"/"IV".
#'
#' \strong{How to apply them}: the result has one value per age, and the
#' vehicle data.frame has one row per street and one column per age. Multiply
#' column by column, e.g. \code{sweep(veh, 2, det, "*")}. \code{veh * det}
#' recycles the factors down the rows (streets) and is wrong. Apply them only
#' to the pollutants they belong to (CO, HC/NMHC, NOx and NO/NO2), without
#' overwriting \code{veh} inside a loop over pollutants.
#' @export
#' @examples \dontrun{
#' data(fkm)
#' pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)
#' km <- units::set_units(pckma[1:11], km)
#' # length eu = length km = 1
#' emis_det(po = "CO", cc = "<=1400", eu = "III", km = km[5], show.equation = TRUE)
#' # length eu = length km = 1, length speed > 1
#' emis_det(po = "CO", cc = "<=1400", eu = "III", km = km[5], speed = Speed(1:10))
#' # length eu = 1 length km > 1
#' emis_det(po = "CO", cc = "<=1400", eu = "III", km = km)
#' # length eu = 2, length km = 2 (if different length, error!)
#' (cod1 <- emis_det(po = "CO", cc = "<=1400", eu = c("III", "IV"), km = km[4:5]))
#' # length eu = 2, length km = 2, length speed > 1
#' (cod1 <- emis_det(po = "CO", cc = "<=1400", eu = c("III", "IV"), speed = Speed(0:130),
#' km = km[4:5]))
#' euros <- c("V","V","V", "IV", "IV", "IV", "III", "III", "III", "III")
#' (cod1 <- emis_det(po = "CO", cc = "<=1400", eu = euros, speed = Speed(1:100),
#' km = km[1:10]))
#' cod1 <- as.matrix(cod1[, 1:10])
#' filled.contour(cod1, col = cptcity::cpt(6277, n = 20))
#' euro <- c(rep("V", 5), rep("IV", 5), "III")
#' euros <- rbind(euro, euro)
#' (cod1 <- emis_det(po = "CO", cc = "<=1400", eu = euros, km = km))
#'
#' # applying the factors to vehicles by age
#' veh <- age_veh(x = c(100, 200), agemax = 11)
#' det <- emis_det(po = "CO", cc = "<=1400", eu = euro, km = km)
#' veh_det <- sweep(remove_units(veh), 2, det, "*")
#' }
emis_det <- function(po,
                     cc,
                     eu,
                     speed = Speed(18.9),
                     km,
                     verbose = FALSE,
                     show.equation = FALSE) {

  ldv_det <- sysdata$ldv_det
  ldv_det$EURO <- trimws(ldv_det$EURO)
  ldv_det$CC <- trimws(ldv_det$CC)

  # Check km
  if (is.data.frame(km) || is.matrix(km)) {
    stop("'km' must be a vector with class 'units', one value per age")
  }
  if(!inherits(km, "units")){
    stop("km neeeds to has class 'units' in 'km'. Please, check package '?units::set_units'")
  }
  km <- as.numeric(units::set_units(km, "km", mode = "standard"))

  #Check cc
  if(is.numeric(cc)){
    cc <- ifelse(cc <= 1400, "<=1400",
                 ifelse(cc <= 2000, "1400_2000", ">2000"))
  }
  cc <- as.character(cc)
  if (length(cc) != 1) stop("'cc' must have length 1")
  if (!cc %in% ldv_det$CC) {
    stop("'cc' must be one of: ", paste(unique(ldv_det$CC), collapse = ", "))
  }
  if (!po %in% ldv_det$POLLUTANT) {
    stop("'po' must be one of: ", paste(unique(ldv_det$POLLUTANT), collapse = ", "))
  }

  #Check speed
  if(!inherits(speed, "units")){
    stop("speed neeeds to has class 'units' in 'km/h'. Please, check package '?units::set_units'")
  }
  speed <- as.numeric(units::set_units(speed, "km/h", mode = "standard"))

  # deterioration of one euro standard at mileage km
  det1 <- function(e, kmi) {
    e <- as.character(e)
    e0 <- if (e %in% c("VId", "VII")) "VIc" else if (e == "III+DPF") "III" else e
    df <- ldv_det[ldv_det$POLLUTANT == po & ldv_det$CC == cc &
                    ldv_det$EURO == e0, ]
    if (nrow(df) == 0) {
      stop("No deterioration factor for po = ", po, ", cc = ", cc, ", eu = ", e)
    }
    df <- df[1, ]
    MC_URBAN <- ifelse(kmi < df$a, df$b * kmi + df$c, df$d)
    MC_ROAD <- ifelse(kmi < df$e, df$f * kmi + df$g, df$h)
    mc <- ifelse(speed < 19, MC_URBAN,
                 ifelse(speed > 63, MC_ROAD,
                        MC_URBAN + (speed - 19) * (MC_ROAD - MC_URBAN) / 44))
    pmax(mc, 1)
  }

  if (!is.data.frame(eu) && !is.matrix(eu)) {
    eu <- as.character(eu)
    if (any(eu %in% c("V", "VI", "VIc", "VId", "VII")) && verbose) {
      message("Assuming the same deterioration as euro III and IV")
    }
    if (show.equation) {
      df <- ldv_det[ldv_det$POLLUTANT == po & ldv_det$CC == cc &
                      ldv_det$EURO == eu[1], ]
      cat(paste0("b = ", df$b, ", c = ", df$c, ", MAX MC URBAN = ", df$d,
                 ", \nf = ", df$f,", g = ", df$g , ", MAX MC ROAD = ", df$h, "\n"))
      cat("V < 19: MC_URBAN = b*km + c\n")
      cat("V >= 63: MC_ROAD = f*km + g\n")
      cat("19 <= V < 63: MC_URBAN + (V - 19)*(MC_ROAD - MC_URBAN)/44\n")
    }
    n <- max(length(eu), length(km))
    if (length(eu) > 1 && length(km) > 1 && length(eu) != length(km)) {
      stop("length of 'eu' and 'km' must be equal (or one of them 1)")
    }
    eu <- rep_len(eu, n)
    km <- rep_len(km, n)
    mc <- lapply(seq_len(n), function(i) det1(eu[i], km[i]))
    if (length(speed) > 1) {
      if(verbose) message("As speed has many values, this a data.frame")
      mc <- as.data.frame(do.call("cbind", mc))
      names(mc) <- paste0("km", seq_len(n))
      mc$speed <- Speed(speed)
    } else {
      if(verbose) message("As speed has 1 value, this a vector")
      mc <- unlist(mc)
    }
    return(mc)
  }

  eu <- as.data.frame(eu)
  for (i in seq_len(ncol(eu))) eu[, i] <- as.character(eu[, i])
  if (ncol(eu) != length(km)) {
    stop("Length of km must be the same as number of columns of 'eu'")
  }
  mc <- do.call("rbind", lapply(seq_len(nrow(eu)), function(j) {
    dff <- do.call("cbind", lapply(seq_len(ncol(eu)), function(i) {
      det1(eu[j, i], km[i])
    }))
    if(verbose) message("as 'eu' is a data.frame, this is a data.frame")
    dff <- as.data.frame(dff)
    dff$speed <- Speed(speed)
    dff
  }))
  mc
}
