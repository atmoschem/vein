#' Split street emissions based on a grid
#'
#' @description \code{\link{split_emis}} split street emissions into a grid.
#'
#' @param net A spatial dataframe of class "sp" or "sf". When class is "sp"
#' it is transformed to "sf" with emissions.
#' @param distance Numeric distance or a grid with class "sf".
#' @param verbose Logical, to show more information.
#' @importFrom sf st_sf st_as_sf st_length  st_intersection st_set_geometry
#' @export
#' @examples {
#' data(net)
#' g <- make_grid(net, 1/102.47/2) #500m in degrees
#' names(net)
#' dim(net)
#' netsf <- sf::st_as_sf(net)[, "ldv"]
#' x <- split_emis(netsf, g)
#' dim(x)
#' }
split_emis <- function(net, distance, verbose = TRUE){
  net <- sf::st_as_sf(net)
  if(is.numeric(distance)){
    if(verbose) cat("Creating grid\n")
    g <- make_grid(spobj = net, width = distance)
  } else if (class(distance)[1] == "sf") {
    g <- distance
  }
  net$id <- NULL
  if(verbose){
    sumemis <- sum(sf::st_set_geometry(net, NULL), na.rm = T)/1000
    cat("Total Emissions", sumemis, " kg \n")
  }
  ncolnet <- names(sf::st_set_geometry(net, NULL))
  net$LKM <- sf::st_length(net)
  if(verbose) cat("Intersecting\n")
  gnet <- st_intersection(net, g)
  gnet$LKM2 <- sf::st_length(gnet)
  geo <- gnet$geometry
  gnet <- as.data.frame(sf::st_set_geometry(gnet, NULL))
  gnet[, ncolnet] <- gnet[, ncolnet] * as.numeric(gnet$LKM2/gnet$LKM)
  gnet <- as.data.frame(gnet[, ncolnet])
  if(verbose){
    sumemis <- sum(gnet, na.rm = T)/1000
    cat("Total Emissions", sumemis, " kg \n")
  }
  gnet$id <- 1:nrow(gnet)
  gnet <- sf::st_sf(gnet, geometry = geo)
  return(gnet)
}
