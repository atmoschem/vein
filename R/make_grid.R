#' Creates rectangular grid for emission allocation
#'
#' @description \code{make_grid} creates a sf grid of polygons. The spatial
#' reference is taken from the spatial object.
#'
#' @param spobj A spatial object of class sp or sf.
#' @param width Width of grid cell. It is recommended to use projected values.
#' @param height Height of grid cell.
#' @param crs coordinate reference system in numeric format from
#' http://spatialreference.org/ to transform/project spatial data using sf::st_transform.
#' The default value is 3857, Pseudo Mercator
#' @return A grid of polygons class 'sf'
#' @importFrom sf st_as_sf st_sf st_crs st_bbox st_sfc st_as_sfc
#' @export
#' @examples \dontrun{
#' data(net)
#' grid <- make_grid(net, width = 0.5/102.47) #500 mts
#' plot(grid, axes = TRUE) #class sf
#' # make grid now returns warnings for crs with form +init...
#' #grid <- make_grid(net, width = 0.5/102.47) #500 mts
#'
#' }
make_grid <- function(spobj,
                      width,
                      height = width,
                      crs = 3857){

  if(substr(crs, 1, 5) == "+init"){
    warning("GDAL Message 1: +init=epsg:XXXX syntax is deprecated.
         It might return a CRS with a non-EPSG compliant axis order.")
  }
  if(!inherits(spobj, "character")){ #path for wrfinput?

    if(inherits(spobj, "bbox")) {
      spobj <- sf::st_as_sfc(spobj)
      spobj <- sf::st_sf(geometry = spobj)

    }

    net <- sf::st_as_sf(spobj)
    # from sf::st_make_grid
    # g <- sf::st_make_grid(x = net, cellsize = width, ...)
    makinggrid <- function (x,
                            cellsize = c(width, height),
                            offset = sf::st_bbox(x)[1:2],
                            # n = c(10, 10),
                            crs =  sf::st_crs(x),
                            what = "polygons") {
      bb = sf::st_bbox(x)
      nx = ceiling((bb[3] - offset[1])/cellsize[1])
      ny = ceiling((bb[4] - offset[2])/cellsize[2])
      cat(paste0("Number of lon points: ", nx, "\n",
                 "Number of lat points: ", ny, "\n\n"))
      xc = offset[1] + (0:nx) * cellsize[1]
      yc = offset[2] + (0:ny) * cellsize[2]

      if (what == "polygons") {
        ret = vector("list", nx * ny)
        square = function(x1, y1, x2, y2){
          sf::st_polygon(list(matrix(c(x1, x2, x2, x1, x1, y1, y1, y2, y2, y1),5)))
        }
        for (i in 1:nx) for (j in 1:ny)
          ret[[(j - 1) * nx + i]] = square(xc[i], yc[j], xc[i + 1], yc[j + 1])
      }
      sf::st_sfc(ret, crs = sf::st_crs(x))
    }
    g <- makinggrid(x = net, cellsize = c(width, height))
    gg <- sf::st_sf(id = 1:length(g), geometry = g)
    if(!missing(crs)){
      gg <- sf::st_transform(gg, crs)
    }
    return(gg)
  } else {
    cat("If you are reading wrfinput_d0x, try:\n
  g <- eixport::wrf_grid(spobj,type = 'wrfinput_d0x')\n")
  }
}
