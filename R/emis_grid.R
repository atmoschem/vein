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
#' @param type type of geometry: "lines", "points" or "polygons".
#' @param FN Character indicating the function. Default is "sum"
#' @importFrom sf st_sf st_dimension st_transform st_length st_cast st_intersection st_area
#' @importFrom data.table data.table .SD
#' @importFrom sp CRS
#' @export
#' @note \strong{1) Emissions are returned as flux = mass / area / time (implicit),}
#' \strong{time untis are not displayed because each use can have different time units}
#' \strong{for instance, year, month, hour second, etc.}
#'
#' \strong{2) Therefore, it is good practice to have time units in 'spobj'. }
#' \strong{This implies that spobj MUST include units!. }
#'
#' \strong{3) In order to check the sum of the emissions, you must calculate the grid-area}
#' \strong{in km^2 and multiply by each column of the resulting emissions grid, and then sum.}
#' @examples \dontrun{
#' data(net)
#' g <- make_grid(net, 1/102.47/2) #500m in degrees
#' names(net)
#' netsf <- sf::st_as_sf(net)
#' netg <- emis_grid(spobj = netsf[, c("ldv", "hdv")], g = g, sr= 31983)
#' round(sum(netg$ldv)) == round(as.numeric(sum(net$ldv)))
#' round(sum(netg$hdv)) == round(as.numeric(sum(net$hdv)))
#' plot(netg["ldv"], axes = TRUE)
#' plot(netg["hdv"], axes = TRUE)
#' netg <- emis_grid(spobj = netsf[, c("ldv", "hdv")], g = g, sr= 31983, FN = "mean")
#' plot(netg["ldv"], axes = TRUE)
#' plot(netg["hdv"], axes = TRUE)
#' \dontrun{
#' library(sf)
#' library(data.table)
#' library(raster)
#' library(stars)
#' g <- readRDS("/media/sergio/ext5/gd03.rds")
#' net <- fread("/media/sergio/ext5/edgar_ecb05_opt1/2012/extracted/ALD.csvy")
#' net$layer <- net$layer*1e12
#' net <- rasterFromXYZ(net)
#' net <- crop(net, as_Spatial(g))
#' net <- st_as_stars(net)
#' net <- st_as_sf(net, crs = 4326)
#' st_crs(net) <- 4326
#' netg <- emis_grid(net, g, type = "polygons")
#' }
#' }
emis_grid <- function (spobj = net,
                       g,
                       sr,
                       type = "lines",
                       FN = "sum"){
  net <- sf::st_as_sf(spobj)
  net$id <- NULL
  # add as.data.frame qhen net comes from data.table
  netdata <- as.data.frame(sf::st_set_geometry(net, NULL))
  message("Your units are: ")
  uninet <- units(netdata[[1]])
  message(uninet)
  for (i in 1:length(netdata)) {
    netdata[, i] <- as.numeric(netdata[, i])
  }
  net <- sf::st_sf(netdata, geometry = net$geometry)
  g <- sf::st_as_sf(g)
  g$id <- 1:nrow(g)
  if (!missing(sr)) {
    if (class(sr)[1] == "character") {
      sr <- as.numeric(substr(sp::CRS(sr), 12, nchar(sr)))
    }
    message("Transforming spatial objects to 'sr' ")
    net <- sf::st_transform(net, sr)
    g <- sf::st_transform(g, sr)
  }
  if (type %in% c("lines", "line")) {
    net <- net[, grep(pattern = TRUE, x = sapply(net, is.numeric))]

    netdf <- sf::st_set_geometry(net, NULL)
    snetdf <- sum(netdf, na.rm = TRUE)
    ncolnet <- ncol(netdf)
    namesnet <- names(netdf)

    cat(paste0("Sum of street emissions ", round(snetdf, 2), "\n"))

    net$LKM <- sf::st_length(net)

    netg <- suppressMessages(suppressWarnings(sf::st_intersection(net, g)))

    netg$LKM2 <- sf::st_length(netg)

    xgg <- data.table::data.table(netg)

    xgg[, 1:ncolnet] <- xgg[, 1:ncolnet] * as.numeric(xgg$LKM2/xgg$LKM)

    xgg[is.na(xgg)] <- 0

    dfm <- xgg[,
               lapply(.SD, eval(parse(text = FN)), na.rm = TRUE),
               by = "id",
               .SDcols = namesnet]

    id <- dfm$id

    dfm$id <- NULL

    area <- sf::st_area(g)

    area <- units::set_units(area, "km^2")

    dfm <- dfm * snetdf/sum(dfm, na.rm = TRUE)

    dfm$id <- id

    gx <- data.frame(id = g$id)

    gx <- merge(gx, dfm, by = "id", all = TRUE)

    gx[is.na(gx)] <- 0

    for(i in seq_along(namesnet)) {
      units(gx[[namesnet[i]]]) <- uninet
      gx[[namesnet[i]]] <- gx[[namesnet[i]]]/area  # !
    }
    cat(paste0("Sum of gridded emissions ",
               round(sum(gx[namesnet]*remove_units(area)),2), "\n"))

    gx <- sf::st_sf(gx, geometry = g$geometry)
    return(gx)
  } else if (type %in% c("points", "point")) {
    netdf <- sf::st_set_geometry(net, NULL)
    snetdf <- sum(netdf, na.rm = TRUE)
    cat(paste0("Sum of point emissions ", round(snetdf, 2),
               "\n"))
    ncolnet <- ncol(netdf)
    namesnet <- names(netdf)
    xgg <- data.table::data.table(sf::st_set_geometry(suppressMessages(suppressWarnings(sf::st_intersection(net,
                                                                                                            g))), NULL))
    xgg[is.na(xgg)] <- 0
    dfm <- xgg[, lapply(.SD, eval(parse(text = FN)), na.rm = TRUE), by = "id",
               .SDcols = namesnet]
    id <- dfm$id
    dfm$id <- NULL

    area <- units::set_units(sf::st_area(g), "km^2")
    dfm <- dfm * snetdf/sum(dfm, na.rm = TRUE)
    dfm$id <- id
    gx <- data.frame(id = g$id)
    gx <- merge(gx, dfm, by = "id", all = TRUE)
    gx[is.na(gx)] <- 0
    for(i in seq_along(namesnet)) {
      units(gx[[namesnet[i]]]) <- uninet
      gx[[namesnet[i]]] <- gx[[namesnet[i]]]/area  # !
    }
    cat(paste0("Sum of gridded emissions ",
               round(sum(matvect(gx[namesnet],
                                 x = remove_units(area),
                                 by = "row")),
                     2), "\n"))
    gx <- sf::st_sf(gx, geometry = g$geometry)
    return(gx)
  } else if(type %in% c("polygons", "polygon", "area")){
    netdf <- sf::st_set_geometry(net, NULL)
    snetdf <- sum(netdf, na.rm = TRUE)
    cat(paste0("Sum of point emissions ", round(snetdf, 2), "\n"))
    ncolnet <- ncol(netdf)
    net <- net[, grep(pattern = TRUE, x = sapply(net, is.numeric))]
    namesnet <- names(netdf)
    net$area1 <- sf::st_area(net)
    netg <- suppressMessages(suppressWarnings(sf::st_intersection(net,
                                                                  g)))
    netg$area2 <- sf::st_area(netg)


    xgg <- data.table::data.table(netg)
    xgg[, 1:ncolnet] <- xgg[, 1:ncolnet] * as.numeric(xgg$area2/xgg$area1)
    xgg[is.na(xgg)] <- 0
    dfm <- xgg[, lapply(.SD, eval(parse(text = FN)), na.rm = TRUE), by = "id",
               .SDcols = namesnet]
    id <- dfm$id
    dfm$id <- NULL
    dfm <- dfm * snetdf/sum(dfm, na.rm = TRUE) # !
    cat(paste0("Sum of gridded emissions ", round(sum(dfm,
                                                      na.rm = T), 2), "\n"))
    dfm$id <- id
    gx <- data.frame(id = g$id)
    gx <- merge(gx, dfm, by = "id", all = TRUE)
    gx[is.na(gx)] <- 0
    gx <- sf::st_sf(gx, geometry = g$geometry)
    return(gx)
  }
}
