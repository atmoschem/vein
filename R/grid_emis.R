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
#' @param top_down Logical; requires emissions named `emissions` and allows
#' to apply profile factors. If your data is hourly emissions or a a spatial
#' grid with several emissions at different hours, being each hour a column,
#' it is better to use top_down = FALSE. In this way all the hourly emissions
#' are considered, however, eah hourly emissions has to have the name "V" and the
#'number of the hour like "V1"
#' @param sr Spatial reference e.g: 31983. It is required if spobj and g are
#' not projected. Please, see http://spatialreference.org/.
#' @param pro Numeric, Matrix or data-frame profiles, for instance, pc_profile.
#' @param char Character, name of the first letter of hourly emissions. New variables in R
#' start with letter "V", for your hourly emissions might start with letter "h". This option
#' applies when top_down is FALSE. For instance, if your hourly emissions are: "h1", "h2",
#' "h3"... `char`` can be "h"
#'
#' @param verbose Logical; to show more info.
#' @importFrom sf st_sf st_as_sf st_transform st_set_geometry st_length  st_intersection st_geometry
#' @importFrom data.table as.data.table ':='
#' @export
#' @note \strong{Your gridded emissions might have flux units (mass / area / time(implicit))}
#' \strong{You must multiply your emissions with the area to return to the original units.}
#' @examples \dontrun{
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
#'             profile = 1, speed = Speed(1))
#' E_CO_STREETS <- emis_post(arra = E_CO, by = "streets", net = net)
#'
#' g <- make_grid(net, 1/102.47/2) #500m in degrees
#'
#' gCO <- emis_grid(spobj = E_CO_STREETS, g = g)
#' gCO$emission <- gCO$V1
#' area <- sf::st_area(gCO)
#' area <- units::set_units(area, "km^2") #Check units!
#' gCO$emission <- gCO$emission*area
#' #
#' \dontrun{
#' #do not run
#' library(osmdata)
#' library(sf)
#' osm <- osmdata_sf(
#' add_osm_feature(
#' opq(bbox = st_bbox(gCO)),
#' key = 'highway'))$osm_lines[, c("highway")]
#' st <- c("motorway", "motorway_link", "trunk", "trunk_link",
#' "primary", "primary_link", "secondary", "secondary_link",
#' "tertiary", "tertiary_link")
#' osm <- osm[osm$highway %in% st, ]
#' plot(osm, axes = T)
#' # top_down requires name `emissions` into gCO`
#' xnet <- grid_emis(osm, gCO, top_down = TRUE)
#' plot(xnet, axes = T)
#' # bottom_up requires that emissions are named `V` plus the hour like `V1`
#' xnet <- grid_emis(osm, gCO,top_down= FALSE)
#' plot(xnet["V1"], axes = T)
#' }
#' }
grid_emis <- function(spobj, g,  top_down = FALSE,
                      sr, pro, char, verbose = FALSE){
  net <- sf::st_as_sf(spobj)
  net$id <- NULL
  netdata <- sf::st_set_geometry(net, NULL)
  g <- sf::st_as_sf(g)
  g$id <- NULL
  g$id <- 1:nrow(g)
  net$lkm1 <- as.numeric(sf::st_length(net))

  geo <- sf::st_geometry(net)
  xg <- suppressMessages(suppressWarnings(sf::st_intersection(net, g)))

  if(sf::st_crs(g) != sf::st_crs(net)){
    if(verbose) message("Changing CRS of 'spobj' to match 'g'") #nocov
    net <- sf::st_transform(net, st_crs(g))
  }
  for(i in 1:length(netdata)){
    netdata[, i] <- as.numeric(netdata[, i])
  }
  net <- sf::st_sf(netdata, geometry = sf::st_geometry(net))

  if(!missing(sr)){
    if(class(sr)[1] == "character"){
      sr <- as.numeric(substr(sr, 12, nchar(sr)))
    }
    if(verbose) message("Transforming spatial objects to 'sr' ") #nocov
    net <- sf::st_transform(net, sr)
    g <- sf::st_transform(g, sr)
  }

  if(!top_down) {
    net$id <- NULL
    a <- suppressMessages(suppressWarnings(sf::st_intersection(net, g)))
    a$lm <- st_length(a)
    df <- data.table::as.data.table(a)
    total_lkm <- lm <- id <- NULL
    df[, total_lkm := sum(lm), by = id]

    vars <- names(g)[grep(pattern = char, x = names(g))]
    cdf <- colSums(sf::st_set_geometry(g, NULL)[vars], na.rm = T)
    for(i in 1:length(cdf)) {
      df[[vars[i]]] <- df[[vars[i]]]*df$lm/df$total_lkm
      df[[vars[i]]] <- df[[vars[i]]]*cdf[i]/sum(df[[vars[i]]], na.rm = TRUE)
    }
    data.table::setDF(df)
    df <- sf::st_as_sf(df, geometry = df$geometry)
    return(df)

  } else {
    sumg <- sum(g$emission)


    if(missing(pro)){
      lxxy <- do.call("rbind",
                      lapply(1:length(g$id),
                             function(i) {
                               emis_dist(gy = g[g$id == i,]$emission,
                                         spobj = xg[xg$id == i, ],
                                         verbose = FALSE)
                             }))
      df <- st_set_geometry(lxxy, NULL)
      fx <- as.numeric(sumg/sum(df, na.rm = TRUE))
      df <- df*fx
      if(verbose) { #nocov
        cat(paste0("Sum of gridded emissions ",
                   round(sumg, 2), "\n"))
      }
      if(verbose) { #nocov
        cat(paste0("Sum of street emissions ",
                   round(sum(df), 2), "\n"))
      }
      df <- sf::st_sf(df, geometry = sf::st_geometry(lxxy))
      # if(verbose) cat("Columns:", names(df), "\n")
      return(df)
    }
    if(!missing(pro) ){
      lxxy <- do.call("rbind",
                      lapply(1:length(g$id),
                             function(i) {
                               emis_dist(gy = g[g$id == i,]$emission,
                                         spobj = xg[xg$id == i, ],
                                         pro = pro,
                                         verbose = FALSE)
                             }))
      df <- st_set_geometry(lxxy, NULL)
      fx <- as.numeric(sumg/sum(df, na.rm = TRUE))
      df <- df*fx
      if(verbose) {
        cat(paste0("Sum of gridded emissions ",
                   round(sum(df), 2), "\n"))
      }
      if(verbose) { #nocov
        cat(paste0("Sum of street emissions ",
                   round(sum(df), 2), "\n"))
      }
      df <- sf::st_sf(df, geometry = sf::st_geometry(lxxy))
      return(df)
    }
  }
}
