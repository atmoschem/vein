#' Allocate emissions into a grid returning point emissions or flux
#'
#' @description \code{\link{emis_grid}} allocates emissions proportionally to each grid
#'  cell. The process is performed by the intersection between geometries and the grid.
#' It means that requires "sr" according to your location for the projection.
#' It is assumed that spobj is a Spatial*DataFrame or an "sf" with the pollutants
#' in data. This function returns an object of class "sf".
#'
#' It is
#'
#' @param spobj A spatial dataframe of class "sp" or "sf". When class is "sp"
#' it is transformed to "sf".
#' @param g A grid with class "SpatialPolygonsDataFrame" or "sf".
#' @param sr Spatial reference e.g: 31983, "EPSG:31983" or "+init=epsg:31983".
#' It is required if spobj and g are not projected. When missing and the crs of
#' spobj and g are different, spobj is transformed to the crs of g.
#' Please, see http://spatialreference.org/.
#' @param type type of geometry: "lines", "points" or "polygons".
#' @param FN Character indicating the function. Default is "sum"
#' @param flux Logical, if TRUE, it return flux (mass / area / time (implicit))
#' in a polygon grid, if false,  mass / time (implicit) as points, in a similar fashion
#' as EDGAR provide data.
#' @param k Numeric to multiply emissions, applied after the aggregation
#' with FN.
#' @importFrom sf st_sf st_dimension st_transform st_length st_cast st_intersection st_area st_geometry
#' @importFrom data.table data.table .SD
#' @export
#' @note \strong{1) If flux = TRUE (default), emissions are flux = mass / area / time (implicit), as polygons.}
#' \strong{If flux = FALSE, emissions are mass / time (implicit), as points.}
#' \strong{Time untis are not displayed because each use can have different time units}
#' \strong{for instance, year, month, hour second, etc.}
#'
#' \strong{2) Therefore, it is good practice to have time units in 'spobj'. }
#' \strong{This implies that spobj MUST include units!. }
#'
#' \strong{3) In order to check the sum of the emissions, you must calculate the grid-area}
#' \strong{in km^2 and multiply by each column of the resulting emissions grid, and then sum.}
#'
#'
#' \strong{4) If FN = "sum", is mass conservative for each column: the}
#' \strong{emissions of the parts of spobj outside g are not allocated.}
#' \strong{Only numeric columns of spobj are gridded.}
#' @examples \dontrun{
#' data(net)
#' g <- make_grid(net, 1/102.47/2) #500m in degrees
#' names(net)
#' netsf <- sf::st_as_sf(net)
#' netg <- emis_grid(spobj = netsf[, c("ldv", "hdv")], g = g, sr= 31983)
#' plot(netg["ldv"],
#'      axes = TRUE,
#'      graticule = TRUE,
#'      bg = "black",
#'      lty = 0)
#' g <- sf::st_make_grid(net, 1/102.47/2, square = FALSE) #500m in degrees
#' g <- st_sf(i  =1, geometry = g)
#' netg <- emis_grid(spobj = netsf[, c("ldv", "hdv")], g = g, sr= 31983)
#' plot(netg["ldv"],
#'      axes = TRUE,
#'      graticule = TRUE,
#'      bg = "black",
#'      lty = 0)
#' plot(netg["hdv"], axes = TRUE)
#' netg <- emis_grid(spobj = netsf[, c("ldv", "hdv")], g = g, sr= 31983, FN = "mean")
#' plot(netg["ldv"], axes = TRUE)
#' plot(netg["hdv"], axes = TRUE)
#' netg <- emis_grid(spobj = netsf[, c("ldv", "hdv")], g = g, sr= 31983, flux = FALSE)
#' plot(netg["ldv"],
#'      axes = TRUE,
#'      pch = 16,
#'      pal = cptcity::cpt(colorRampPalette= TRUE,
#'                         rev = TRUE),
#'      cex = 3)
#' }
emis_grid <- function (spobj = net,
                       g,
                       sr,
                       type = "lines",
                       FN = "sum",
                       flux = TRUE,
                       k = 1){
  net <- sf::st_as_sf(spobj)
  net$id <- NULL
  # add as.data.frame when net comes from data.table
  netdata <- as.data.frame(sf::st_set_geometry(net, NULL))

  # only numeric columns (with or without units) are gridded
  isnum <- vapply(netdata, is.numeric, logical(1))
  if (!any(isnum)) stop("'spobj' must have numeric columns")
  if (any(!isnum)) {
    message("Ignoring non numeric columns: ", paste(names(netdata)[!isnum], collapse = ", "))
  }
  netdata <- netdata[, isnum, drop = FALSE]
  namesnet <- names(netdata)

  # units of each column
  uninet <- lapply(netdata, function(x) if (inherits(x, "units")) units(x) else NULL)
  hasunits <- !vapply(uninet, is.null, logical(1))
  if(!any(hasunits)) {
    message("Your data has no units")
  } else {
    message("Your units are: ")
    message(paste(unique(vapply(uninet[hasunits], as.character, character(1))),
                  collapse = ", "))
  }
  for (i in seq_along(netdata)) {
    netdata[[i]] <- as.numeric(netdata[[i]])
  }
  net <- sf::st_sf(netdata, geometry = sf::st_geometry(net))
  net$.vein_sid <- seq_len(nrow(net))

  g <- sf::st_as_sf(g)
  g <- sf::st_sf(id = seq_len(nrow(g)), geometry = sf::st_geometry(g))

  if (!missing(sr)) {
    sr <- vein_crs(sr)
    message("Transforming spatial objects to 'sr' ")
    net <- sf::st_transform(net, sr)
    g <- sf::st_transform(g, sr)
  } else if (sf::st_crs(net) != sf::st_crs(g)) {
    message("Transforming 'spobj' to the crs of 'g'")
    net <- sf::st_transform(net, sf::st_crs(g))
  }

  snetdf <- colSums(netdata, na.rm = TRUE)

  # size of each feature and of each piece after the intersection with g.
  # Each piece receives the fraction size(piece) / size(feature), so the
  # emissions of features outside the grid are not allocated and the
  # emissions of each column are conserved. When the pieces of one feature sum
  # more than the feature (e.g. a street over the border of two cells),
  # the fraction is relative to the sum of the pieces.
  if (type %in% c("lines", "line")) {
    cat(paste0("Sum of street emissions ", round(sum(snetdf), 2), "\n"))
    net$.vein_size1 <- as.numeric(sf::st_length(net))
    netg <- suppressMessages(suppressWarnings(sf::st_intersection(net, g)))
    size2 <- as.numeric(sf::st_length(netg))
  } else if (type %in% c("points", "point")) {
    cat(paste0("Sum of point emissions ", round(sum(snetdf), 2), "\n"))
    net$.vein_size1 <- 1
    netg <- suppressMessages(suppressWarnings(sf::st_intersection(net, g)))
    size2 <- rep(1, nrow(netg))
  } else if(type %in% c("polygons", "polygon", "area")){
    cat(paste0("Sum of polygon emissions ", round(sum(snetdf), 2), "\n"))
    net$.vein_size1 <- as.numeric(sf::st_area(net))
    netg <- suppressMessages(suppressWarnings(sf::st_intersection(net, g)))
    size2 <- as.numeric(sf::st_area(netg))
  } else {
    stop("'type' must be 'lines', 'points' or 'polygons'")
  }
  if (nrow(netg) > 0) {
    totsize <- stats::ave(size2, netg$.vein_sid, FUN = sum)
    den <- pmax(netg$.vein_size1, totsize)
    w <- ifelse(den > 0, size2 / den, 0)
  } else {
    w <- numeric(0)
  }

  xgg <- data.table::as.data.table(sf::st_set_geometry(netg, NULL))
  for (i in seq_along(namesnet)) {
    x <- xgg[[namesnet[i]]] * w
    x[is.na(x)] <- 0
    data.table::set(xgg, j = namesnet[i], value = x)
  }

  fn <- match.fun(FN)
  dfm <- xgg[,
             lapply(.SD, fn, na.rm = TRUE),
             by = "id",
             .SDcols = namesnet]
  dfm <- as.data.frame(dfm)

  for(i in seq_along(namesnet)) dfm[[namesnet[i]]] <- dfm[[namesnet[i]]]*k

  gx <- data.frame(id = g$id)

  gx <- merge(gx, dfm, by = "id", all = TRUE)

  gx[is.na(gx)] <- 0

  cat(paste0("Sum of gridded emissions ",
             round(sum(vapply(namesnet, function(n) sum(as.numeric(gx[[n]])), numeric(1))), 2), "\n"))

  for(i in seq_along(namesnet)) {
    if(hasunits[i]) units(gx[[namesnet[i]]]) <- uninet[[i]]
  }

  if(flux) {
    area <- sf::st_area(g)
    area <- units::set_units(area, "km^2")
    for(i in seq_along(namesnet)) {
      gx[[namesnet[i]]] <- gx[[namesnet[i]]]/area  # !
    }
    gx <- sf::st_sf(gx, geometry = sf::st_geometry(g))
    return(gx)
  } else {
    gx <- sf::st_sf(gx, geometry = sf::st_geometry(g))
    gx <- suppressMessages(suppressWarnings(sf::st_centroid(gx)))
    return(gx)
  }
}

#' Parses a spatial reference: numbers, "+init=epsg:XXXX" or anything
#' accepted by sf::st_crs
#' @noRd
vein_crs <- function(sr) {
  if (inherits(sr, "character") && grepl("^\\+init=epsg:[0-9]+$", sr, ignore.case = TRUE)) {
    sr <- as.numeric(sub("^\\+init=epsg:", "", sr, ignore.case = TRUE))
  }
  sr
}
