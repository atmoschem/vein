#' Creates rectangular grid for emission allocation
#'
#' It is created a SpatialGridDataFrame.The spatial reference is taken from
#' the spatial object.
#'
#' @param spobj A spatial object of class sp
#' @param width Width of grid cell, units according sr
#' @param height Height of grid cell, units according sr
#' @param polygon whe TRUE return a polygon grid, when FALSE a SpatialGridDataFrame
#' @export
#' @return grid
#' @examples \dontrun{
#' #do not run
#' data(net)
#' grid <- make_grid(net, width = 0.5/102.47, height = 0.5/102.47) #500 mts
#' spplot(net, scales=list(draw=T),
#' sp.layout = list("sp.polygons", grid, pch = 16, cex = 2, col = "black"))
#' }
make_grid <- function(spobj, width, height,  polygon = T){
  sr <- spobj@proj4string
  if (polygon == F) {

  bb <- sp::bbox(spobj)
  cs <- c(width, height)  # tamanho celda km
  cc <- bb[, 1] + (cs/2)  # cell offset
  cd <- ceiling(diff(t(bb))/cs)  # numero de celda por direcao
  grd <- sp::GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  grd
  grade <- sp::SpatialGridDataFrame(grd,
                                data=data.frame(id=1:prod(cd)),
                                proj4string=sr)
  return(grade)

  } else if (polygon == T) {
    bb <- sp::bbox(spobj)
    cs <- c(width, height)  # tamanho celda km
    cc <- bb[, 1] + (cs/2)  # cell offset
    cd <- ceiling(diff(t(bb))/cs)  # numero de celda por direcao
    grd <- sp::GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
    grd
    grade <- sp::SpatialGridDataFrame(grd,
                                  data=data.frame(id=1:prod(cd)),
                                  proj4string=sr)
    gg <- raster::rasterToPolygons(raster::raster(grade))
    return(gg)

  }
}
