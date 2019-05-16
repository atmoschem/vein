#' Allocate emissions into a grid
#'
#' @description \code{\link{emis_grid}} allocates emissions proportionally to each grid
#'  cell. The process is performed by intersection between geometries and the grid.
#' It means that requires "sr" according with your location for the projection.
#' It is assumed that spobj is a Spatial*DataFrame or an "sf" with the pollutants
#' in data. This function returns an object of class "sf".
#'
#' @param spobj A spatial dataframe of class "sp" or "sf". When class is "sp"
#' it is transformed to "sf".
#' @param g A grid with class "SpatialPolygonsDataFrame" or "sf".
#' @param sr Spatial reference e.g: 31983. It is required if spobj and g are
#' not projected. Please, see http://spatialreference.org/.
#' @param type type of geometry: "lines" or "points".
#' @importFrom sf st_sf st_dimension st_transform st_length st_cast st_intersection
#' @importFrom data.table data.table .SD
#' @importFrom sp CRS
#' @export
#' @note When spobj is a 'Spatial' object (class of sp), they are converted
#'  into 'sf'. Also, The aggregation of data is done with data.table functions.
#' @examples {
#' data(net)
#' g <- make_grid(net, 1/102.47/2) #500m in degrees
#' names(net)
#' netsf <- sf::st_as_sf(net)
#' netg <- emis_grid(spobj = netsf[, c("ldv", "hdv")], g = g, sr= 31983)
#' plot(netg["ldv"], axes = TRUE)
#' plot(netg["hdv"], axes = TRUE)
#' }
emis_grid <- function(spobj, g, sr, type = "lines"){
  net <- sf::st_as_sf(spobj)
  net$id <- NULL
  netdata <- sf::st_set_geometry(net, NULL)
  for(i in 1:length(netdata)){
netdata[, i] <- as.numeric(netdata[, i])
  }
  net <- sf::st_sf(netdata, geometry = net$geometry)
  g <- sf::st_as_sf(g)
  g$id <- 1:nrow(g)

  if(!missing(sr)){
    if(class(sr)[1] == "character"){
      sr <- as.numeric(substr(sp::CRS(sr), 12, nchar(sr)))
    }
    message("Transforming spatial objects to 'sr' ")
  net <- sf::st_transform(net, sr)
  g <- sf::st_transform(g, sr)
  }

  if (type == "lines" ) {
    netdf <- sf::st_set_geometry(net, NULL)
    snetdf <- sum(netdf, na.rm = TRUE)
    cat(paste0("Sum of street emissions ", round(snetdf, 2), "\n"))
    ncolnet <- ncol(sf::st_set_geometry(net, NULL))

        # Filtrando solo columnas numericas
    net <- net[, grep(pattern = TRUE, x = sapply(net, is.numeric))]
    namesnet <- names(sf::st_set_geometry(net, NULL))
    net$LKM <- sf::st_length(sf::st_cast(net[sf::st_dimension(net) == 1,]))
    netg <- suppressMessages(suppressWarnings(sf::st_intersection(net, g)))
    netg$LKM2 <- sf::st_length(netg)
    xgg <- data.table::data.table(netg)
    xgg[, 1:ncolnet] <- xgg[, 1:ncolnet] * as.numeric(xgg$LKM2/xgg$LKM)
    xgg[is.na(xgg)] <- 0 # prevent 1+ NA = NA
    dfm <- xgg[, lapply(.SD, sum, na.rm=TRUE),
               by = "id",
               .SDcols = namesnet]
    id <- dfm$id
    dfm <- dfm*snetdf/sum(dfm, na.rm = TRUE)
    cat(paste0("Sum of gridded emissions ",
               round(sum(dfm, na.rm = T), 2), "\n"))
    dfm$id <- id
    names(dfm) <- c("id", namesnet)
    gx <- data.frame(id = g$id)
    gx <- merge(gx, dfm, by="id", all.x = TRUE)
    gx[is.na(gx)] <- 0
    gx <- sf::st_sf(gx, geometry = g$geometry)
  # if(array){
  #     return(GriddedEmissionsArray(gx, rows = , cols, times = ))
  #   } else{
      return(gx)
    # }
  } else if ( type == "points" ){
    netdf <- sf::st_set_geometry(net, NULL)
    snetdf <- sum(netdf, na.rm = TRUE)
    cat(paste0("Sum of point emissions ", round(snetdf, 2), "\n"))
    ncolnet <- ncol(sf::st_set_geometry(net, NULL))

    namesnet <- names(sf::st_set_geometry(net, NULL))
    xgg <- data.table::data.table(
      sf::st_set_geometry(suppressMessages(suppressWarnings(sf::st_intersection(net, g))), NULL)
      )
    xgg[is.na(xgg)] <- 0
    dfm <- xgg[, lapply(.SD, sum, na.rm=TRUE),
               by = "id",
               .SDcols = namesnet ]
    cat(paste0("Sum of gridded emissions ",
               round(sum(dfm, na.rm = T), 2), "\n"))
    names(dfm) <- c("id", namesnet)
    gx <- data.frame(id = g$id)
    gx <- merge(gx, dfm, by = "id", all.x = TRUE)
    gx[is.na(gx)] <- 0
    gx <- sf::st_sf(gx, geometry = g$geometry)
    return(gx)
  }
}

