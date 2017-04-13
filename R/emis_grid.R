#' Allocate emissions into a grid
#'
#' The allocation is proportionally to each grid cell. The process is
#' performed by intersection between geometries and the grid. Geometries
#' suported, so, far are lines with raster::intersect and points with
#' sp::over. The allocation of lines is by interaction, then update the
#' pollutant values according the new length of road inside each grid cell.
#' It means that requires "sr" according with your location for the projection.
#' It is assumed that soobj is a spatial*DataFrame with the pollutant in data.
#' Also, it is required that, when is a SpatialLinesDataFrame, there is a field
#' called lkm, with the length of the road, in this case, in km.
#'
#' @param spobj A spatial dataframe of class sp
#' @param g Grid
#' @param sr Spatial reference, default is "+init=epsg:4326"
#' @param type type of geometry: "lines" or "points"
#' @export
#' @examples \dontrun{
#' #do not run
#' data(net)
#' net@data$POL <- rnorm(n = nrow(net@data), mean = 1000, sd = 100)
#' net$highway <- NULL
#' net$lkm <-  gLength(spTransform(net,CRS("+init=epsg:29183")),byid = T)/1000
#' grid <- make_grid(net, width = 0.5/102.47, height = 0.5/102.47) #500 mts
#' gPOL <- emis_grid(spobj = net, g = grid, pol = "POL", sr = "+init=epsg:29183")
#' spplot(gPOL, "POL", scales=list(draw=T),
#'        sp.layout = list("sp.lines", net, pch = 16, cex = 2, col = "black"))
#' }

emis_grid <- function(spobj, g, sr, type="lines"){
  if (type == "lines") {
    spobj$lkm <-  rgeos::gLength(sp::spTransform(spobj,CRS(sr)),byid = T)/1000
    netg <- raster::intersect(spobj,g)
    netg$lkm2 <-  rgeos::gLength(sp::spTransform(netg,CRS(sr)),byid = T)/1000
    netg@data[,1:(ncol(netg@data)-3)] <-  netg@data[,1:(ncol(netg@data)-3)]*netg$lkm2/netg$lkm
    dfm <- aggregate(cbind(netg@data[,1:(ncol(netg@data)-3)]), by=list(netg$id),
                     sum, na.rm=TRUE)

    colnames(dfm)[1] <- "id"
    gg <- merge(g, dfm, by="id")
    return(gg)
  } else if (type == "points"){
      g@data <- sp::over(g,spobj, fn=sum)
      return(g)
    }
  }

