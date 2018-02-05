#' Creates rectangular grid for emission allocation
#'
#' @description \code{make_grid} creates a SpatialGridDataFrame. The spatial
#' reference is taken from the spatial object.
#'
#' @param spobj A spatial object of class sp or sf.
#' @param width Width of grid cell. It is recommended to use projected values.
#' @param height Height of grid cell. It is recommended to use projected values.
#' @param polygon whe TRUE return a polygon grid, when FALSE stops.
#' @return A grid of polygons class 'sf'
#' @importFrom sp bbox GridTopology SpatialGridDataFrame proj4string
#' @importFrom sf st_as_sf
#' @importFrom methods as
#' @export
#' @examples \dontrun{
#' #do not run
#' data(net)
#' grid <- make_grid(net, width = 0.5/102.47, height = 0.5/102.47) #500 mts
#' plot(grid, axes = T) #class f
#' }
make_grid <- function(spobj, width, height,  polygon = T){
  if(polygon == T){
    spobj <- as(spobj, "Spatial")
    sr <- spobj@proj4string
    bb <- sp::bbox(spobj)
    cs <- c(width, height)
    cc <- bb[, 1] + (cs/2)
    cd <- ceiling(diff(t(bb))/cs)
    grd <- sp::GridTopology(cellcentre.offset = cc,
                            cellsize = cs,
                            cells.dim = cd)
    print(grd)
    grade <- sp::SpatialGridDataFrame(grd,
                                      data = data.frame(id=1:prod(cd)),
                                      proj4string=sr)
    gg <- as(grade, "SpatialPolygonsDataFrame")
    gg <- sf::st_as_sf(gg)
    return(gg)
    } else {
      stop("Deprecated, use polygon = T")
    }
  }
