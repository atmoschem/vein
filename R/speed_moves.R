#' Return speed bins according to US/EPA MOVES model
#'
#' @description \code{speed_moves} return an object of average speed bins as defined by
#' US EPA MOVES. The input must be speed as miles/h (mph)
#'
#' @param x Object with class, "sf",  "data.frame", "matrix" or "numeric" with speeds
#' in miles/h (mph)
#' @param net optional  spatial dataframe of class "sf".
#' it is transformed to "sf".#' @importFrom sf st_sf st_set_geometry st_geometry
#' @export
#' @examples {
#' data(net)
#' net$mph <- units::set_units(net$ps, "miles/h")
#' net$speed_bins <- speed_moves(net$mph)
#' head(net)
#' speed_moves(net["ps"])
#' }
speed_moves <- function(x, net) {
  x <- remove_units(x)
  fx <- function(sp) {
    ifelse(
      sp <= 0.1, 0,
      ifelse(
        sp > 0.1 & sp <= 2.5, 1,
        ifelse(
          sp > 2.5 & sp <= 7.5, 2,
          ifelse(
            sp >= 7.5 & sp <= 12.5, 3,
            ifelse(
              sp >= 12.5 & sp <= 17.5, 4,
              ifelse(
                sp >= 17.5 & sp <= 22.5, 5,
                ifelse(
                  sp >= 22.5 & sp <= 27.5, 6,
                  ifelse(
                    sp >= 27.5 & sp <= 32.5, 7,
                    ifelse(
                      sp >= 32.5 & sp <= 37.5, 8,
                      ifelse(
                        sp >= 37.5 & sp <= 42.5, 9,
                        ifelse(
                          sp >= 42.5 & sp <= 47.5, 10,
                          ifelse(
                            sp >= 47.5 & sp <= 52.5, 11,
                            ifelse(
                              sp >= 52.5 & sp <= 57.5, 12,
                              ifelse(
                                sp >= 57.5 & sp <= 62.5, 13,
                                ifelse(
                                  sp >= 62.5 & sp <= 67.5, 14,
                                  ifelse(
                                    sp >= 67.5 & sp <= 72.5, 15,
                                    16))))))))))))))))

  }

  if(inherits(x = x, what = "sf")) {
    net <- sf::st_geometry(x)
    x <- sf::st_set_geometry(x, NULL)
  }

  if  ( is.matrix(x) ) {
    spd <- as.data.frame(x)
    for(i in 1:ncol(spd)){
      spd[[i]] <- fx(spd[[i]])
    }
  } else if ( is.data.frame(x) ) {
    spd <- x
    for(i in 1:ncol(spd)){
      spd[[i]] <- fx(spd[[i]])
    }
  } else if ( is.list(x) ) {
    for(i in 1:length(spd)){
      spd[[i]] <- fx(spd[[i]])
    }
  } else if(class(x) == "units"){
    message("Converting original units to mph")
  spd <- Speed(x, dist = "miles")
      spd <- fx(spd)
  } else {
    spd <- x
    spd <- fx(spd)
  }

  if(!missing(net)) {
   spd <- sf::st_sf(spd, geometry = sf::st_geometry(net))
  }

  return(spd)

}
