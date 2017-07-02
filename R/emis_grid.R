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
#' This function accepts data with "units" but they are converted internally
#' to numeric and then return SpatialPolygonsDataFrame with numeric data.frame
#'
#' @param spobj A spatial dataframe of class sp
#' @param g A grid with class SpatialPolygonsDataFrame
#' @param sr Spatial reference, default is "+init=epsg:4326"
#' @param type type of geometry: "lines" or "points"
#' @export
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
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1,
#' isList = T)
#' pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)
#' cod1 <- emis_det(po = "CO", cc = 1000, eu = "III", km = pckma[1:11])
#' cod2 <- emis_det(po = "CO", cc = 1000, eu = "I", km = pckma[12:24])
#' #vehicles newer than pre-euro
#' co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
#' cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
#' lef <- ef_ldv_scaled(co1, cod, v = "PC", t = "ALL", cc = "ALL",
#'                      f = "G",p = "CO", eu=co1$Euro_LDV)
#' lef <- c(lef,lef[length(lef)],lef[length(lef)],lef[length(lef)],
#'          lef[length(lef)],lef[length(lef)])
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed, agemax = 41,
#'              profile = pc_profile, hour = 24, day = 7, array = T)
#' # arguments required: arra, pollutant ad by
#' E_CO_STREETS <- emis_post(arra = E_CO, pollutant = "CO", by = "streets_wide")
#' net@data <- cbind(net@data, E_CO_STREETS)
#' head(net@data)
#' g <- make_grid(net, 1/102.47/2, 1/102.47/2, polygon = T) #500m in degrees
#' net@data <- net@data[,- c(1:9)]
#' names(net)
#' E_CO_g <- emis_grid(spobj = net, g = g, sr= "+init=epsg:31983")
#' head(E_CO_g@data)
#' library(RColorBrewer)
#' spplot(E_CO_g, "V138", scales=list(draw=T),cuts=8,
#' colorkey = list(space = "bottom", height = 1),
#' col.regions=brewer.pal(9, "Blues"),
#' sp.layout = list("sp.lines", net, pch = 16, cex = 2, col = "black"))
#' }
emis_grid <- function(spobj, g, sr, type="lines"){
  if ( type == "lines" ) {
    net <- spobj
    for( i in 1:ncol(net@data) ){
      net@data[,i] <- as.numeric(net@data[,i])
    }
    net$lkm <-  rgeos::gLength(sp::spTransform(net,CRS(sr)),byid = T)/1000
    netg <- raster::intersect(net,g)
    netg$lkm2 <-  rgeos::gLength(sp::spTransform(netg,CRS(sr)),byid = T)/1000
    netg@data[,1:(ncol(netg@data)-3)] <-  netg@data[,1:(ncol(netg@data)-3)] * netg$lkm2/netg$lkm
    dfm <- stats::aggregate(cbind(netg@data[,1:(ncol(netg@data)-3)]),
                            by=list(netg$id), sum, na.rm=TRUE)
    colnames(dfm)[1] <- "id"
    gg <- merge(g, dfm, by="id")
    # for(i in 2:ncol(gg)){
    #   gg@data[,i] <- gg@data[,i] * units::parse_unit("g h-1")
    # } # spplot does not work with units
    return(gg)
  } else if ( type == "points" ){
      g@data <- sp::over(g,spobj, fn=sum)
      #Add units
      return(g)
    }
  }

