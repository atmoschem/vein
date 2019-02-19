#' Estimation of emissions Hot Exhaust top down
#'
#' @description \code{\link{emis}} estimates vehicular emissions as the product of the
#' vehicles on a road, length of the road, emission factor avaliated at the
#' respective speed. \eqn{E = VEH*LENGTH*EF(speed)}
#'
#' @param veh "Vehicles" data-frame or list of "Vehicles" data-frame. Each data-frame
#' as number of columns matching the age distribution of that ype of vehicle.
#' The number of rows is equal to the number of streets link
#' @param lkm Length of each link
#' @param ef List of functions of emission factors
#' @param speed Speed data-frame with number of columns as hours
#' @param agemax Age of oldest vehicles for that category
#' @param verbose Logical; To show more information
#' @return emission estimation  g/h
#' @note If the user apply a top-down approach, the resulting units will be
#' according its own data. For instance, if the vehicles are veh/day, the units
#' of the emissions implicitly will be g/day.
#' Hour and day will be deprecate because they can be infered from the profile
#' matrix.
#' @export
#' @importFrom sf st_set_geometry
#' @examples \dontrun{
#' # Do not run
#' }
emis_hot_td <- function (veh,
                  lkm,
                  ef,
                  speed = 34,
                  agemax = ifelse(is.data.frame(veh), ncol(veh),
                                  ncol(veh[[1]])),
                  verbose = FALSE) {
  # Check units
  if(class(lkm) != "units"){
    stop("lkm neeeds to has class 'units' in 'km'. Please, check package 'units'")
  }
  if(units(lkm)$numerator == "m" ){
    stop("Units of lkm is 'm'. Please, check package '?units::set_units'")
  }
  # At least, on e of these
  if(any(!class(ef) %in% c("list", "units", "EmissionFactorsList",
                           "EmissionFactors", "data.frame"))){
    stop("ef must be either of 'list', 'units', 'EmissionFactorsList', 'EmissionFactors' or 'data.frame'")
  }
  # Checking sf
  if(any(class(veh) %in% "sf")){
    if(verbose) message("converting sf to data.frame")
    veh <- sf::st_set_geometry(veh, NULL)
  }
  if(!missing(hour) | !missing(day)){
    warning("Arguments  hour and day will be deprecated, they will derived from profile")
  }

  lkm <- as.numeric(lkm)
  speed <- as.data.frame(speed)
  for (i  in 1:ncol(speed) ) {
    speed[, i] <- as.numeric(speed[, i])
  }
  # veh is "Vehicles" data-frame
  if (!inherits(x = veh, what = "list")) {
    veh <- as.data.frame(veh)
    for (i  in 1:ncol(veh) ) {
      veh[,i] <- as.numeric(veh[,i])
    }
    # top down
    if(missing(profile)){
   stop("For top down approach, use emis_hot_td")

    }
#TODO
  }
}
