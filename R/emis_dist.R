#' Allocate emissions into spatial objects
#'
#' @description \code{\link{emis_dist}} allocates emissions proportionally to
#' each feature. "Spatial" objects are converter to "sf" objects. Currently,
#' 'LINESTRING' or 'MULTILINESTRING' supported. The emissions are distributed
#' in each street.
#'
#' @param gy Numeric; total (top-down) emissions (grams)
#' @param spobj A spatial dataframe of class "sp" or "sf". When class is "sp"
#' it is transformed to "sf".
#' @param pro Matrix or data-frame profiles, for instance, pc_profile.
#' @param dyear Numeric, Number of days of period of time, default is 365.
#' @param osm Numeric; vector of length 5, for instance, c(5, 3, 2, 1, 1).
#' The first element covers 'motorway' and 'motorway_link.
#' The second element covers 'trunk' and 'trunk_link'.
#' The third element covers 'primary' and 'primary_link'.
#' The fourth element covers 'secondary' and 'secondary_link'.
#' The fifth element covers 'tertiary' and 'tertiary_link'.
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
emis_dist <- function(gy, spobj, pro, dyear = 365, osm){
  spobj <- sf::st_as_sf(spobj)
  if(any(
    !unique(as.character(
      sf::st_geometry_type(spobj))) %in% c("LINESTRING", "MULTILINESTRING"))){
    stop("Currently, geometries supported are 'LINESTRING' or 'MULTILINESTRING'")
  }
  net <- spobj
  net$lkm1 <- as.numeric(sf::st_length(net))
  lkm <- net$lkm1/sum(net$lkm1)
  if(!missing(osm)){
    if(length(osm) != 5) stop("length of osm must be 5")
    if(!"highway" %in% names(net)) stop("Need OpenStreetMap network with colum highway")
    net <- net[,c("highway", "lkm1")]
    net <- net[net$highway %in% c("motorway",
                                  "motorway_link",
                                  "trunk",
                                  "trunk_link",
                                  "primary",
                                  "primary_link",
                                  "secondary",
                                  "secondary_link",
                                  "tertiary",
                                  "tertiary_link",
                                  "residential"), ]
    osm <- osm/sum(osm)
    #motorway
    net_m <- net[net$highway %in% c("motorway", "motorway_link"), ]
    net_m$gy <- net_m$lkm1 / sum(net_m$lkm1) * gy * osm[1]
    #trunk
    net_t <- net[net$highway %in% c("trunk", "trunk_link"), ]
    net_t$gy <- net_t$lkm1 / sum(net_t$lkm1) * gy * osm[2]
    #primary
    net_p <- net[net$highway %in% c("primary","primary_link"), ]
    net_p$gy <- net_p$lkm1 / sum(net_p$lkm1) * gy * osm[3]
    #secondary
    net_s <- net[net$highway %in% c("secondary", "secondary_link"), ]
    net_s$gy <- net_s$lkm1 / sum(net_s$lkm1) * gy * osm[4]
    #tertiary
    net_te <- net[net$highway %in% c("tertiary", "tertiary_link"), ]
    net_te$gy <- net_te$lkm1 / sum(net_te$lkm1) * gy * osm[5]
    net_all <- rbind(net_m, net_t, net_p, net_s, net_te)
  }
  if(missing(pro)){
    if(missing(osm)){
      spobj$gy <- lkm*gy * units::as_units("g")
      return(spobj)
    } else if(!missing(osm)){
      net_all$lkm1 <- NULL
      return(net_all)
    }
  } else if(!missing(pro)){
    prop <- sapply(pro, sum)/sapply(pro, sum)[1]
    pro <- unlist(lapply(1:ncol(pro), function(i){
      pro[, i] <- pro[,i]/sum(pro[, i])
    })) * prop
    emispro <- gy/dyear*pro
    if(missing(osm)){
      mx <- as.matrix(lkm) %*% matrix(emispro, nrow = 1)
      mx <- Emissions(as.data.frame(mx))
      names(mx) <- paste0("h", 1:ncol(mx))
      mx <- cbind(sf::st_set_geometry(net, NULL), mx)
      net_all <- sf::st_sf(mx, geometry = net$geometry)
      return(net_all)
    } else if (!missing(osm)){
      mx <- as.matrix(net_all$lkm1 / sum(net_all$lkm1)) %*% matrix(emispro,
                                                                   nrow = 1)
      mx <- Emissions(as.data.frame(mx))
      names(mx) <- paste0("h", 1:ncol(mx))
      mx <- cbind(sf::st_set_geometry(spobj, NULL), mx)
      spobj <- sf::st_sf(mx, geometry = spobj$geometry)
      spobj$lkm1 <- NULL
      return(spobj)
    }
  }
}
