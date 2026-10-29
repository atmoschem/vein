#' Return speed bins according to US/EPA MOVES model
#'
#' @description \code{speed_moves} return an object of average speed bins as defined by
#' US EPA MOVES. Speeds with units are converted to miles/h (mph); numeric
#' speeds are assumed in mph.
#'
#' @param x Object with class, "sf",  "data.frame", "matrix", "list", "units"
#' or "numeric" with speeds
#' @note Bins are lower inclusive: 1 for speeds below 2.5 mph, 2 for
#' [2.5, 7.5), ..., 16 for 72.5 mph or more, and 0 for speeds <= 0.1 mph.
#' @param net optional  spatial dataframe of class "sf".
#' it is transformed to "sf".
#' @importFrom sf st_sf st_set_geometry st_geometry
#' @importFrom data.table fifelse
#' @export
#' @examples {
#' data(net)
#' net$mph <- units::set_units(net$ps, "miles/h")
#' net$speed_bins <- moves_speed(net$mph)
#' head(net)
#' moves_speed(net["ps"])
#' }
moves_speed <- function(x, net) {
  # convert speeds with units to mph before removing units
  to_mph <- function(v) {
    if (inherits(v, "units")) {
      as.numeric(units::set_units(v, "miles/h", mode = "standard"))
    } else {
      v
    }
  }
  # MOVES avgSpeedBinID: 1 below 2.5 mph, k in [5(k-1) - 2.5, 5(k-1) + 2.5)
  # and 16 from 72.5 mph; 0 for speeds <= 0.1 mph
  fx <- function(sp) {
    sp <- as.numeric(sp)
    b <- as.numeric(findInterval(sp, seq(2.5, 72.5, 5)) + 1)
    b[!is.na(sp) & sp <= 0.1] <- 0
    b
  }

  if (inherits(x = x, what = "sf")) {
    net <- sf::st_geometry(x)
    x <- sf::st_set_geometry(x, NULL)
  }

  if  (is.matrix(x)) {
    spd <- as.data.frame(x)
    for (i in 1:ncol(spd)) {
      spd[[i]] <- fx(to_mph(spd[[i]]))
    }
  } else if (is.data.frame(x)) {
    spd <- as.data.frame(x)
    class(spd) <- "data.frame"
    for (i in 1:ncol(spd)) {
      spd[[i]] <- fx(to_mph(spd[[i]]))
    }
  } else if (is.list(x)) {
    spd <- lapply(x, function(v) fx(to_mph(v)))
  } else {
    spd <- fx(to_mph(x))
  }

  if (!missing(net)) {
    spd <- sf::st_sf(spd, geometry = sf::st_geometry(net))
  }

  return(spd)

}
