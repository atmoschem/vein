#' Allocate emissions into a grid
#'
#' @description \code{emis_grid} allocates emissions proportionally to each grid
#'  cell. The process is performed by intersection between geometries and the grid.
#' It means that requires "sr" according with your location for the projection.
#' It is assumed that spobj is a spatial*DataFrame or an "sf" with the pollutants
#' in data. This function return an object class "sf".
#'
#' @param spobj A spatial dataframe of class "sp" or "sf". When class is "sp"
#' it is transformed to "sf".
#' @param g A grid with class "SpatialPolygonsDataFrame" or "sf".
#' @param sr Spatial reference e.g: 31983. It is required if spobj and g are
#' not projected. Please, see http://spatialreference.org/.
#' @param type type of geometry: "lines" or "points".
#' @importFrom sf st_sf st_dimension st_transform st_length st_cast st_intersection
#' @importFrom data.table data.table .SD
#' @export
#' @note When spobj is a 'Spatial' object (class of sp), they are converted
#'  into 'sf'. Also, The aggregation of data ise done with data.table functions.
#' @examples \dontrun{
#' data(net)
#' data(pc_profile)
#' data(fe2015)
#' data(fkm)
#' PC_G <- c(33491,22340,24818,31808,46458,28574,24856,28972,37818,49050,87923,
#'           133833,138441,142682,171029,151048,115228,98664,126444,101027,
#'           84771,55864,36306,21079,20138,17439, 7854,2215,656,1262,476,512,
#'           1181, 4991, 3711, 5653, 7039, 5839, 4257,3824, 3068)
#' veh <- data.frame(PC_G = PC_G)
#' pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
#' pcw <- temp_fact(net$ldv+net$hdv, pc_profile)
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)
#' cod1 <- emis_det(po = "CO", cc = 1000, eu = "III", km = pckma[1:11])
#' cod2 <- emis_det(po = "CO", cc = 1000, eu = "I", km = pckma[12:24])
#' #vehicles newer than pre-euro
#' co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
#' cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
#' lef <- ef_ldv_scaled(co1, cod, v = "PC", t = "4S", cc = "<=1400",
#'                      f = "G",p = "CO", eu=co1$Euro_LDV)
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed, agemax = 41,
#'              profile = pc_profile, hour = 24, day = 7, array = T)
#' # arguments required: arra, pollutant ad by
#' E_CO_STREETS <- emis_post(arra = E_CO, pollutant = "CO", by = "streets_wide")
#' net@data <- cbind(net@data, E_CO_STREETS)
#' head(net@data)
#' g <- make_grid(net, 1/102.47/2, 1/102.47/2) #500m in degrees
#'
#' net@data <- net@data[,- c(1:9)]
#' names(net)
#' E_CO_g <- emis_grid(spobj = net, g = g, sr= 31983)
#' head(E_CO_g) #class sf
#' E_CO_g$V138 <- as.numeric(E_CO_g$V138)
#' E_CO_g <- as(E_CO_g, "Spatial")
#' spplot(E_CO_g, "V138", scales=list(draw=T),cuts=8,
#' colorkey = list(space = "bottom", height = 1),
#' col.regions = rev(bpy.colors(9)),
#' sp.layout = list("sp.lines", net, pch = 16, cex = 2, col = "black"))
#' }
emis_grid <- function(spobj, g, sr, type = "lines"){
  net <- sf::st_as_sf(spobj)
  net$id <- NULL
  g <- sf::st_as_sf(g)

  if(exists("sr")){
  net <- sf::st_transform(net, sr)
  g <- sf::st_transform(g, sr)
  }

  if (type == "lines" ) {
    ncolnet <- ncol(sf::st_set_geometry(net, NULL))
    namesnet <- names(sf::st_set_geometry(net, NULL))
    net$LKM <- sf::st_length(sf::st_cast(net[sf::st_dimension(net) == 1,]))
    netg <- suppressWarnings(st_intersection(net, g))
    netg$LKM2 <- sf::st_length(sf::st_cast(netg[sf::st_dimension(netg) == 1,]))
    xgg <- data.table::data.table(netg)
    xgg[, 1:ncolnet] <- xgg[, 1:ncolnet] * as.numeric(xgg$LKM2/xgg$LKM)
    dfm <- xgg[, lapply(.SD, sum, na.rm=TRUE),
               by = "id",
               .SDcols = namesnet]
    names(dfm) <- c("id", namesnet)
    gx <- data.frame(id = g$id)
    gx <- merge(gx, dfm, by="id", all.x = T)
    gx[is.na(gx)] <- 0
    gx <- sf::st_sf(gx, geometry = g$geometry)
  # if(array){
  #     return(GriddedEmissionsArray(gx, rows = , cols, times = ))
  #   } else{
      return(gx)
    # }
  } else if ( type == "points" ){
    xgg <- data.table::data.table(
      sf::st_set_geometry(sf::st_intersection(net, g), NULL)
      )
    dfm <- xgg[, lapply(.SD, sum, na.rm=TRUE),
               by = "id",
               .SDcols = namesnet ]
    names(dfm) <- c("id", namesnet)
    gx <- data.frame(id = g$id)
    gx <- merge(gx, dfm, by = "id", all.x = T)
    gx[is.na(gx)] <- 0
    gx <- sf::st_sf(gx, geometry = g$geometry)
    return(gx)
  }
}

