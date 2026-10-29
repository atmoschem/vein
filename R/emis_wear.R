#' Emission estimation from tyre, brake and road surface wear
#'
#' @description \code{emis_wear} estimates wear emissions. The sources are tyres,
#' breaks and road surface.
#'
#' @param veh Object of class "Vehicles"
#' @param lkm Length of the road in km.
#' @param ef Emission factors from \code{\link{ef_wear}}: a data.frame with
#' one column per hour (hour x day columns), or a single value (e.g. road wear)
#' used for all hours.
#' @param what Character for indicating "tyre", "break" or "road". Kept for
#' compatibility; it is not used because the speed correction is applied by
#' \code{\link{ef_wear}}.
#' @param speed Speed data-frame with number of columns as hours. Kept for
#' compatibility; it is not used because the speed correction is applied by
#' \code{\link{ef_wear}}.
#' @param agemax Age of oldest vehicles for that category
#' @param profile Numerical or dataframe with nrows equal to 24 and ncol
#' 7 day of the week
#' @param hour Number of considered hours in estimation
#' @param day Number of considered days in estimation
#' @return emission estimation  g/h
#' @note \code{ef} must be the output of \code{\link{ef_wear}}, with one column
#' per hour (hour x day columns). The speed correction factor is part of
#' \code{ef}. Before vein 2.0.0, \code{emis_wear} applied the speed correction
#' a second time (using the brake curve for tyres and vice versa) and
#' always read the speed of the first day.
#' @references Ntziachristos and Boulter 2016. Automobile tyre and break wear
#' and road abrasion. In: EEA, EMEP. EEA air pollutant emission inventory
#' guidebook-2009. European Environment Agency, Copenhagen, 2016
#' @export
#' @examples \dontrun{
#' data(net)
#' data(pc_profile)
#' pc_week <- temp_fact(net$ldv[1:10] + net$hdv[1:10], pc_profile[, 1])
#' df <- netspeed(pc_week, net$ps[1:10], net$ffs[1:10],
#'               net$capacity[1:10], net$lkm[1:10], alpha = 1)
#' ef <- ef_wear(wear = "tyre", type = "PC", pol = "PM10", speed = df)
#' emi <- emis_wear(veh = age_ldv(net$ldv[1:10], name = "VEH"),
#'                  lkm = net$lkm[1:10], ef = ef, speed = df,
#'                  profile = pc_profile[, 1])
#' emi
#' }
emis_wear <- function(veh,
                      lkm,
                      ef,
                      what = "tyre",
                      speed,
                      agemax = ncol(veh),
                      profile,
                      hour = nrow(profile),
                      day = ncol(profile)) {
  if (inherits(lkm, "units") && units(lkm)$numerator == "m") {
    stop("Units of lkm is 'm'")
  }
  veh <- as.data.frame(veh)
  lkm <- as.numeric(lkm)
  for (i  in 1:ncol(veh)) {
    veh[, i] <- as.numeric(veh[, i])
  }
  # profile
  if (is.vector(profile)) {
    profile <- matrix(as.numeric(profile), ncol = 1)
  } # if it is data.frame or matrix, OK

  if (is.null(dim(ef)) && length(ef) == 1) {
    ef <- as.data.frame(matrix(as.numeric(ef), nrow = 1, ncol = hour * day))
  }
  ef <- as.data.frame(ef)
  if (ncol(ef) != hour * day) {
    stop("Number of days of ef and profile must be the same: ncol(ef) must be ",
      "nrow(profile) * ncol(profile)")
  }

  d <-  simplify2array(
    lapply(1:day, function(j) {
      simplify2array(
        lapply(1:hour, function(i) {
          efi <- as.numeric(ef[, hour * (j - 1) + i])
          simplify2array(
            lapply(1:agemax, function(k) {
              veh[, k] * profile[i, j] * lkm * efi
            }))
        }))
    }))
  return(EmissionsArray(d))
}
