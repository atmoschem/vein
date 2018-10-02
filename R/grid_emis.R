#' Allocate emissions gridded emissions into streets (grid to emis street)
#'
#' @description \code{\link{grid_emis}} it is sort of the opposite of
#' \code{\link{emis_grid}}. It allocates gridded emissions into streets.
#' This function applies \code{\link{emis_dist}} into each grid cell using
#' lapply. This function is in development and pull request are welcome.
#'
#' @param spobj A spatial dataframe of class "sp" or "sf". When class is "sp"
#' it is transformed to "sf".
#' @param g A grid with class "SpatialPolygonsDataFrame" or "sf". This grid
#' includes the total emissions with the column "emission". If profile is going
#' to be used, the column 'emission' must include the sum of the emissions
#' for each profile. For instance, if profile covers the hourly emissions,
#' the column 'emission' bust be the sum of the hourly emissions.
#' @param sr Spatial reference e.g: 31983. It is required if spobj and g are
#' not projected. Please, see http://spatialreference.org/.
#' @param pro Numeric, Matrix or data-frame profiles, for instance, pc_profile.
#' @param osm Numeric; vector of length 5, for instance, c(5, 3, 2, 1, 1).
#' The first element covers 'motorway' and 'motorway_link.
#' The second element covers 'trunk' and 'trunk_link'.
#' The third element covers 'primary' and 'primary_link'.
#' The fourth element covers 'secondary' and 'secondary_link'.
#' The fifth element covers 'tertiary' and 'tertiary_link'.
#' @param verbose Logical; to show more info.
#' @importFrom sf st_sf st_as_sf st_transform st_set_geometry st_length  st_intersection
#' @export
#' @note When spobj is a 'Spatial' object (class of sp), they are converted
#'  into 'sf'.
#' @examples {
#' data(net)
#' data(pc_profile)
#' data(fkm)
#' PC_G <- c(33491,22340,24818,31808,46458,28574,24856,28972,37818,49050,87923,
#' 133833,138441,142682,171029,151048,115228,98664,126444,101027,
#'        84771,55864,36306,21079,20138,17439, 7854,2215,656,1262,476,512,
#' 1181, 4991, 3711, 5653, 7039, 5839, 4257,3824, 3068)
#' pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
#' # Estimation for morning rush hour and local emission factors
#' lef <- EmissionFactorsList(ef_cetesb("CO", "PC_G"))
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef,
#'             profile = 1)
#' E_CO_STREETS <- emis_post(arra = E_CO, by = "streets_wide", net = net)
#'
#' g <- make_grid(net, 1/102.47/2) #500m in degrees
#'
#' gCO <- emis_grid(spobj = E_CO_STREETS, g = g)
#' gCO$emission <- gCO$V1
#' #
#' \dontrun{
#' #do not run
#' library(osmdata)
#' library(sf)
#' q0 <- opq(bbox = st_bbox(gCO))
#' q1 <- add_osm_feature(q0, key = 'name')
#' x <- osmdata_sf(q1)
#' osm <- x$osm_lines[, "highway"]
#' st <- c("motorway", "motorway_link", "trunk", "trunk_link",
#' "primary", "primary_link", "secondary", "secondary_link",
#' "tertiary", "tertiary_link")
#' osm <- osm[osm$highway %in% st, ]
#' plot(osm, axes = T)
#' xnet <- grid_emis(osm, gCO)
#' plot(xnet, axes = T)
#' }
#' }
grid_emis <- function(spobj, g, sr, pro, osm, verbose = TRUE){
  if(!any(grepl(pattern = "emission", x = names(g)))){
    stop("The column 'emission' is not present in grid 'g'")
  }
  net <- sf::st_as_sf(spobj)
  net$id <- NULL
  netdata <- sf::st_set_geometry(net, NULL)
  g <- sf::st_as_sf(g)
  g$id <- NULL
  sumg <- sum(g$emission)
  g$id <- 1:nrow(g)
  net$lkm1 <- as.numeric(sf::st_length(net))

  geo <- sf::st_geometry(net)
  xg <- st_intersection(net, g)

  if(sf::st_crs(g) != sf::st_crs(net)){
    if(verbose) message("Changing CRS of 'spobj' to match 'g'")
    net <- sf::st_transform(net, st_crs(g))
  }
  for(i in 1:length(netdata)){
    netdata[, i] <- as.numeric(netdata[, i])
  }
  net <- sf::st_sf(netdata, geometry = net$geometry)

  if(!missing(sr)){
    if(class(sr)[1] == "character"){
      sr <- as.numeric(substr(sp::CRS(sr), 12, nchar(sr)))
    }
    if(verbose) message("Transforming spatial objects to 'sr' ")
    net <- sf::st_transform(net, sr)
    g <- sf::st_transform(g, sr)
  }

  if(missing(pro) & missing(osm)){
    lxxy <- do.call("rbind",
                    lapply(1:length(g$id),
                           function(i) {
                             emis_dist(gy = g[g$id == i,]$emission,
                                       spobj = xg[xg$id == i, ])
                           }))
    df <- st_set_geometry(lxxy, NULL)
    fx <- sumg/sum(df, na.rm = TRUE)
    df <- df*fx
    if(verbose) {
      cat(paste0("Sum of gridded emissions ",
                 round(sumg, 2), "\n"))
    }
    if(verbose) {
      cat(paste0("Sum of street emissions ",
                 round(df, 2), "\n"))
    }
    df <- sf::st_sf(df, geometry = sf::st_geometry(lxxy))
    return(df)
  }

  if(!missing(pro) & missing(osm)){
    lxxy <- do.call("rbind",
                    lapply(1:length(g$id),
                           function(i) {
                             emis_dist(gy = g[g$id == i,]$emission,
                                       spobj = xg[xg$id == i, ],
                                       pro = pro)
                           }))
    df <- st_set_geometry(lxxy, NULL)
    fx <- sum(sumg)/sum(df, na.rm = TRUE)
    df <- df*fx
    if(verbose) {
      cat(paste0("Sum of gridded emissions ",
                 round(sumg, 2), "\n"))
    }
    if(verbose) {
      cat(paste0("Sum of street emissions ",
                 round(df, 2), "\n"))
    }
    df <- sf::st_sf(df, geometry = sf::st_geometry(lxxy))
    return(df)
  }
  if(missing(pro) & !missing(osm)){
    lxxy <- do.call("rbind",
                    lapply(1:length(g$id),
                           function(i) {
                             emis_dist(gy = g[g$id == i,]$emission,
                                       spobj = xg[xg$id == i, ],
                                       osm = osm)
                           }))
    df <- st_set_geometry(lxxy, NULL)
    fx <- sum(sumg)/sum(df, na.rm = TRUE)
    df <- df*fx
    if(verbose) {
      cat(paste0("Sum of gridded emissions ",
                 round(sumg, 2), "\n"))
    }
    if(verbose) {
      cat(paste0("Sum of street emissions ",
                 round(df, 2), "\n"))
    }
    df <- sf::st_sf(df, geometry = sf::st_geometry(lxxy))
    return(df)
  }
  if(!missing(pro) & !missing(osm)){
    lxxy <- do.call("rbind",
                    lapply(1:length(g$id),
                           function(i) {
                             emis_dist(gy = g[g$id == i,]$emission,
                                       spobj = xg[xg$id == i, ],
                                       osm = osm, pro = pro)
                           }))
    df <- st_set_geometry(lxxy, NULL)
    fx <- sum(sumg)/sum(df, na.rm = TRUE)
    df <- df*fx
    if(verbose) {
      cat(paste0("Sum of gridded emissions ",
                 round(sumg, 2), "\n"))
    }
    if(verbose) {
      cat(paste0("Sum of street emissions ",
                 round(df, 2), "\n"))
    }
    df <- sf::st_sf(df, geometry = sf::st_geometry(lxxy))
    return(df)
  }
}

