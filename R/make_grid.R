#' Creates rectangular grid for emission allocation
#'
#' @description \code{make_grid} creates a SpatialGridDataFrame. The spatial
#' reference is taken from the spatial object.
#'
#' @param spobj A spatial object of class sp or sf.
#' @param width Width of grid cell. It is recommended to use projected values.
#' @param height Height of grid cell. Deprecated!
#' @param polygon  Deprecated! \code{\link{make_grid}} returns only sf grid of
#' polygons.
#' @param  ... ignored
#' @param crs coordinate reference system in numeric format from
#' http://spatialreference.org/ to transform/project spatial data using sf::st_transform
#' @return A grid of polygons class 'sf'
#' @importFrom sp bbox GridTopology SpatialGridDataFrame proj4string
#' @importFrom sf st_as_sf st_make_grid st_sf
#' @export
#' @examples {
#' data(net)
#' grid <- make_grid(net, width = 0.5/102.47) #500 mts
#' plot(grid, axes = TRUE) #class sf
#' }
make_grid <- function(spobj, width, height,  polygon, crs, ...){
  if(!missing(polygon)){
    .Deprecated(msg = "'polygon' is deprecated")
  } else if(!missing(height)){
    .Deprecated(msg = "'height' is deprecated, use width only")
  }
net <- sf::st_as_sf(spobj)
    g <- sf::st_make_grid(x = net, cellsize = width, ...)
    gg <- sf::st_sf(id = 1:length(g), geometry = g)
    if(!missing(crs)){
      gg <- sf::st_transform(gg, crs)
    }
  return(gg)
}
