#' Allocate emissions into spatial objects (street emis to grid)
#'
#' @description \code{\link{emis_dist}} allocates emissions proportionally to
#' each feature. "Spatial" objects are converter to "sf" objects. Currently,
#' 'LINESTRING' or 'MULTILINESTRING' supported. The emissions are distributed
#' in each street.
#'
#' @param gy Numeric; a unique total (top-down) emissions (grams)
#' @param spobj A spatial dataframe of class "sp" or "sf". When class is "sp"
#' it is transformed to "sf".
#' @param pro Matrix or data-frame profiles, for instance, pc_profile.
#' @param osm Numeric; vector of length 5, for instance, c(5, 3, 2, 1, 1).
#' The first element covers 'motorway' and 'motorway_link.
#' The second element covers 'trunk' and 'trunk_link'.
#' The third element covers 'primary' and 'primary_link'.
#' The fourth element covers 'secondary' and 'secondary_link'.
#' The fifth element covers 'tertiary' and 'tertiary_link'.
#' @param verbose Logical; to show more info.
#' @importFrom sf st_sf st_as_sf  st_length st_geometry_type st_set_geometry
#' @importFrom units as_units
#' @export
#' @note When spobj is a 'Spatial' object (class of sp), they are converted
#'  into 'sf'.
#' @examples {
#' data(net)
#' data(pc_profile)
#' po <- 1000
#' t1 <- emis_dist(gy = po, spobj = net)
#' head(t1)
#' sum(t1$gy)
#' #t1 <- emis_dist(gy = po, spobj = net, osm = c(5, 3, 2, 1, 1) )
#' t1 <- emis_dist(gy = po, spobj = net, pro = pc_profile)
#' }
emis_dist <- function(gy,
                      spobj,
                      pro,
                      osm,
                      verbose = TRUE){
  net <- sf::st_as_sf(spobj)
  if(any(
    !unique(as.character(
      sf::st_geometry_type(net))) %in% c("LINESTRING", "MULTILINESTRING"))){
    stop("Currently, geometries supported are 'LINESTRING' or 'MULTILINESTRING'")
  }
  net$lkm1 <- as.numeric(sf::st_length(net))
  geo <- sf::st_geometry(net)
  lkm <- net$lkm1/sum(net$lkm1)
  e_street <- lkm*gy * units::as_units("g")

  # PROFILE SECTION
  if(missing(pro) & missing(osm)){
    net <- net[, "geometry"]
    net$emission <- e_street
    if(verbose) cat("Columns:", names(net), "\n")
    return(net)
  }
  if(!missing(pro) & missing(osm)){
    pro <- pro/sum(pro)
    df <- as.data.frame(as.matrix(e_street) %*% matrix(unlist(pro), nrow = 1))
    net <- sf::st_sf(df, geometry = geo)
    if(verbose) {
      cat("Columns: ")
      cat(names(net))
      cat("\n")
    }

    return(net)
  }
  if(missing(pro) & !missing(osm)){
    net <- net[,c("highway", "lkm1")]
    st <-  c("motorway", "motorway_link",
             "trunk", "trunk_link",
             "primary", "primary_link",
             "secondary", "secondary_link",
             "tertiary", "tertiary_link")
    if(verbose) cat("Selecting:", st, "\n")
    net <- net[net$highway %in% st, ]
    if(length(osm) != 5) stop("length of osm must be 5")
    if(!"highway" %in% names(net)) stop("Need OpenStreetMap network with colum highway")
    osm <- osm/sum(osm)
    #motorway
    net_m <- net[net$highway %in% st[1:2], ]
    net_m$gy <- net_m$lkm1 / sum(net_m$lkm1) * gy * osm[1]
    #trunk
    net_t <- net[net$highway %in% st[3:4], ]
    net_t$gy <- net_t$lkm1 / sum(net_t$lkm1) * gy * osm[2]
    #primary
    net_p <- net[net$highway %in% st[5:6], ]
    net_p$gy <- net_p$lkm1 / sum(net_p$lkm1) * gy * osm[3]
    #secondary
    net_s <- net[net$highway %in% st[7:8], ]
    net_s$gy <- net_s$lkm1 / sum(net_s$lkm1) * gy * osm[4]
    #tertiary
    net_te <- net[net$highway %in% st[9:10], ]
    net_te$gy <- net_te$lkm1 / sum(net_te$lkm1) * gy * osm[5]
    net_all <- rbind(net_m, net_t, net_p, net_s, net_te)
    net_all <- net_all[, c("gy", "highway")]
    names(net_all) <- c("emission", "highway", "geometry")
    if(verbose) cat("Columns:", names(net_all), "\n")
    return(net_all)
  }
  if(!missing(pro) & !missing(osm)){
    net <- net[,c("highway", "lkm1")]
    st <-  c("motorway", "motorway_link",
             "trunk", "trunk_link",
             "primary", "primary_link",
             "secondary", "secondary_link",
             "tertiary", "tertiary_link")
    if(verbose) cat("Selecting:", st, "\n")
    net <- net[net$highway %in% st, ]

    pro <- pro/sum(pro)

    if(length(osm) != 5) stop("length of osm must be 5")
    if(!"highway" %in% names(net)) stop("Need OpenStreetMap network with colum highway")
    osm <- osm/sum(osm)
    #motorway
    net_m <- net[net$highway %in% st[1:2], ]
    net_m$gy <- net_m$lkm1 / sum(net_m$lkm1) * gy * osm[1]
    #trunk
    net_t <- net[net$highway %in% st[3:4], ]
    net_t$gy <- net_t$lkm1 / sum(net_t$lkm1) * gy * osm[2]
    #primary
    net_p <- net[net$highway %in% st[5:6], ]
    net_p$gy <- net_p$lkm1 / sum(net_p$lkm1) * gy * osm[3]
    #secondary
    net_s <- net[net$highway %in% st[7:8], ]
    net_s$gy <- net_s$lkm1 / sum(net_s$lkm1) * gy * osm[4]
    #tertiary
    net_te <- net[net$highway %in% st[9:10], ]
    net_te$gy <- net_te$lkm1 / sum(net_te$lkm1) * gy * osm[5]
    net_all <- rbind(net_m, net_t, net_p, net_s, net_te)
    df <- as.data.frame(as.matrix(net_all$gy) %*% matrix(unlist(pro), nrow = 1))
    net_all <- sf::st_sf(df, geometry = sf::st_geometry(net_all))
    net_all <- net_all[, c("gy", "highway")]
    names(net_all) <- c("emission", "highway", "geometry")
    if(verbose) {
      cat("Columns: ")
      cat(names(net_all))
      cat("\n")
      }
    return(net_all)
  }
}
