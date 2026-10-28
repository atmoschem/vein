#' Emission calculation based on EMFAC emission factors
#'
#' @description \code{\link{emis_emfac}} estimates emissions based on
#' an emission factors database from EMFAC.You must download the
#' emission factors from EMFAC website.
#'
#' @param ef data.frame or character path to EMFAC ef (g/miles)
#' @param veh Vehicles data.frame
#' @param lkm Distance per street-link in miles
#' @param tfs vector to project activity by hour
#' @param speed Speed data.frame in miles/hour, with columns "S1", "S2", ...
#' one for each element of tfs. Each link uses the EF of the highest EMFAC speed
#' bin not above its speed (speeds below the first bin use the first bin).
#' @param vehname numeric vector for heavy good vehicles or trucks
#' @param pol character, "CO_RUNEX". When ef has a column `pollutant`, only
#' this pollutant is used.
#' @param modelyear numeric vector, 2021:1982
#' @param vkm logical, to return vkm
#' @param verbose logical, to show more information
#' @return data.table with emission estimation in long format
#' @note Emission factors must be in g/miles
#' @export
#' @examples \dontrun{
#' # do not run
#' }
emis_emfac <- function(ef,
                       veh,
                       lkm,
                       tfs,
                       speed,
                       vehname,
                       pol = "CO_RUNEX",
                       modelyear = 2021:1982,
                       vkm = TRUE,
                       verbose = TRUE){

  hours = paste0("S", seq_along(tfs))

  if(!inherits(lkm, "units")){
    stop("lkm neeeds to has class 'units' in 'miles'. Please, check package 'units'")
  }
  if(units(lkm) == units(units::as_units("km"))){
    stop("Units of lkm is 'km', change to 'miles'")
  }
  if(units(lkm) == units(units::as_units("miles"))) {
    lkm <- as.numeric(lkm)
  }

  # ef
  if(is.character(ef)) {
    ef <- ef_emfac(ef)
  }
  if(!any(names(ef) %in% "vehicles")) {
    stop("Add column `vehicles` with one category")
  }

  if(length(unique(ef$vehicles)) > 1) {
    stop("ef must one `vehicles` category. There is: ",
         unique(ef$vehicles))
  }
  ef <- data.table::as.data.table(ef)
  # keep only the pollutant pol (ef_emfac() returns all pollutants)
  if ("pollutant" %in% names(ef)) {
    if (!pol %in% ef$pollutant) {
      stop("pol '", pol, "' not found in ef$pollutant")
    }
    ef <- ef[as.character(ef$pollutant) == pol]
  }


  ModelYear <- NULL

  if(missing(speed) | is.character(ef$Speed)){

    # estimation
    if(verbose) cat("Estimating emissions of", pol, " for", vehname, "\n")
    data.table::rbindlist(lapply(seq_along(hours), function(l) {

      data.table::rbindlist(lapply(seq_along(modelyear), function(k) {

        efx <- ef[ModelYear == modelyear[k]]$gmiles
        if (length(efx) != 1) {
          stop("ef must have one value per ModelYear when speed is missing; found ",
               length(efx), " for ModelYear ", modelyear[k])
        }
        eeff <- Emissions(efx,
                          mass = "g",
                          dist = "miles")

        vv <- Vehicles(as.numeric(veh[[k]]*tfs[l]),
                       time = "1/h")

        emi <- eeff*lkm* vv
        dx <- data.table::data.table(id = seq_along(emi),
                                     emi = emi,
                                     age = k,
                                     vehicles = vehname,
                                     pollutant = pol,
                                     hour = l)
        dx
      }))
    }))   -> emii

  } else {

    if(!inherits(speed, "Speed")){
      stop("speed neeeds to has class 'Speed' with col-units'miles/h'")
    }
    if(units(speed[[1]]) != units(units::as_units("miles/h"))){
      stop("Units of speed must be 'miles/h' ")
    }
    if(units(speed[[1]]) == units(units::as_units("miles/h"))){
      speed <- remove_units(speed)
    }

    # estimation
    if(verbose) cat("Estimating emissions of", pol, " for", vehname, "\n")
    data.table::rbindlist(lapply(seq_along(hours), function(l) {

      data.table::rbindlist(lapply(seq_along(modelyear), function(k) {

        efx <- ef[ModelYear == modelyear[k]]
        if (anyDuplicated(efx$Speed)) {
          stop("ef has more than one value per Speed for ModelYear ", modelyear[k])
        }

        breaks <- sort(unique(efx$Speed))

        # speed bin of each link (speeds below the first bin use the first bin)
        interval <- pmax(findInterval(as.numeric(speed[[hours[l]]]),
                                      breaks), 1)

        # match() keeps the order of the links
        gm <- efx$gmiles[match(breaks[interval], efx$Speed)]

        eeff <- Emissions(gm,
                          mass = "g",
                          dist = "miles")

        vv <- Vehicles(as.numeric(veh[[k]]*tfs[l]),
                       time = "1/h")

        dx <- data.table::data.table(id = 1:length(eeff),
                                     emi = eeff*lkm* vv,
                                     age = k,
                                     vehicles = vehname,
                                     pollutant = pol,
                                     hour = l)
        dx
      }))
    }))   -> emii


  }
  return(emii)
}
