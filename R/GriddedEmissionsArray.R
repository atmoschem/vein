#' Construction function for class "GriddedEmissionsArray"
#'
#' @description \code{GriddedEmissionsArray} returns a tranformed object with class
#' "EmissionsArray" with 4 dimensios.
#'
#' @return Objects of class "GriddedEmissionsArray"
#'
#' @param x Object with class "SpatialPolygonDataFrame", "sf" "data.frame" or
#' "matrix"
#' @param object object with class "EmissionsArray'
#' @param ... ignored
#' @param rows Number of rows
#' @param cols Number of columns
#' @param times Number of times
#' @param rotate Logical to rotate TRUE or not FALSE Array
#' @rdname GriddedEmissionsArray
#' @aliases GriddedEmissionsArray print.GriddedEmissionsArray
#' summary.GriddedEmissionsArray plot.GriddedEmissionsArray
#' @importFrom sf st_set_geometry
#' @importFrom utils head
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
#' class(E_CO)
#' E_CO_STREETS <- emis_post(arra = E_CO, pollutant = "CO", by = "streets_wide")
#' net@data <- cbind(net@data, E_CO_STREETS)
#' head(net@data)
#' g <- make_grid(net, 1/102.47/2, 1/102.47/2) #500m in degrees
#' net@data <- net@data[,- c(1:9)]
#' names(net)
#' E_CO_g <- emis_grid(spobj = net, g = g, sr= 31983)
#' head(E_CO_g) #class sf
#' library(mapview)
#' mapview(E_CO_g, zcol= "V1", legend = T, col.regions = cptcity::cptcity(1))
#' gr <- GriddedEmissionsArray(E_CO_g, rows = 19, cols = 23, times = 168, T)
#' plot(gr)
#'
#' # For some cptcity color gradients:
#' devtools::install_github("ibarraespinosa/cptcity")
#' plot(gr, col = cptcity::cptcity(1))
#' }
#' @export
GriddedEmissionsArray <- function(x, ..., cols, rows, times = ncol(x),
                                  rotate = FALSE) {
  x$id <- NULL
  if(inherits(x, "Spatial")){
  df <- sf::st_as_sf(x)
  df <- sf::st_set_geometry(x, NULL)
  } else if(inherits(x, "sf")){
    df <- sf::st_set_geometry(x, NULL)
  }
  for (i in 1:ncol(df)) {
    df[, i] <- as.numeric(df[, i])
  }
  if (rotate) {
    e <- simplify2array(lapply(1:ncol(df), function(i){
      t(apply(matrix(data = df[, i],
                     nrow = rows,
                     ncol = cols,
                     byrow = T),
              2,
              rev))
    }))
  } else {
    e <- array(unlist(df), c(cols, rows, times))
  }

  class(e) <- c("GriddedEmissionsArray",class(e))
  cat("This GriddedEmissionsArray has:\n",
      rows, "lat points\n",
      cols, "lon points\n",
      times,  "hours\n")
  return(e)
}

#' @rdname GriddedEmissionsArray
#' @method print GriddedEmissionsArray
#' @export
print.GriddedEmissionsArray <- function(x,  ...) {
  e <- x
if (is.array(e)) {
    cat("This GriddedEmissionsArray has:\n",
        dim(e)[1], "lat points\n",
        dim(e)[2], "lon points\n",
        dim(e)[3], "hours\n\n")
  print(utils::head(e))
  }
}

#' @rdname GriddedEmissionsArray
#' @method summary GriddedEmissionsArray
#' @export
summary.GriddedEmissionsArray <- function(object, ...) {
  e <- object
  summary(e[ , ,  ])
  }

#' @rdname GriddedEmissionsArray
#' @method plot GriddedEmissionsArray
#' @export
plot.GriddedEmissionsArray <- function(x, ..., times = 1) {
  e <- x
  graphics::image(e[ , , times], ...)
  graphics::par(mfrow = c(1, 1))
}
