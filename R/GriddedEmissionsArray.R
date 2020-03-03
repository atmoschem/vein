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
#' @param rotate Character, rotate array to "left" or "right"
#' @param flip Logical, To flip vertically the array or not
#' @rdname GriddedEmissionsArray
#' @aliases GriddedEmissionsArray print.GriddedEmissionsArray
#' summary.GriddedEmissionsArray plot.GriddedEmissionsArray
#' @importFrom sf st_set_geometry
#' @importFrom utils head
#' @examples {
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
#' pckm <- units::set_units(fkm[[1]](1:24), "km")
#' pckma <- cumsum(pckm)
#' cod1 <- emis_det(po = "CO", cc = 1000, eu = "III", km = pckma[1:11])
#' cod2 <- emis_det(po = "CO", cc = 1000, eu = "I", km = pckma[12:24])
#' #vehicles newer than pre-euro
#' co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
#' cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
#' lef <- ef_ldv_scaled(co1, cod, v = "PC", t = "4S", cc = "<=1400",
#'                      f = "G",p = "CO", eu=co1$Euro_LDV)
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed, agemax = 41,
#'               profile = pc_profile, simplify = TRUE)
#' class(E_CO)
#' E_CO_STREETS <- emis_post(arra = E_CO, pollutant = "CO", by = "streets",
#'                           net = net, k = units::set_units(1, "1/h"))
#' g <- make_grid(net, 1/102.47/2, 1/102.47/2) #500m in degrees
#' E_CO_g <- emis_grid(spobj = E_CO_STREETS, g = g, sr= 31983)
#' plot(E_CO_g["V9"])
#' gr <- GriddedEmissionsArray(E_CO_g, rows = 19, cols = 23, times = 168, flip = FALSE)
#' plot(gr)
#' # For some cptcity color gradients:
#' plot(gr, col = cptcity::lucky())
#' }
#' @export
GriddedEmissionsArray <- function(x, ..., cols, rows, times = ncol(x),
                                  rotate, flip = TRUE) {
  x$id <- NULL
  if(inherits(x, "Spatial")){
  df <- sf::st_as_sf(x)
  df <- sf::st_set_geometry(df, NULL)
  } else if(inherits(x, "sf")){
    df <- sf::st_set_geometry(x, NULL)
  } else {
    df <- x
  }
  if(times > ncol(df)) stop("`times` cannot be higher than number of columns of `x` without x$id")
  for (i in 1:ncol(df)) {
    df[, i] <- as.numeric(df[, i])
  }
  # array
  # https://stackoverflow.com/a/42882677/2418532
  #first reverse, then transpose, it's the same as rotate 90 degrees
  rotate_clockwise         <- function(x) { t(     apply(x, 2, rev))}
  #first transpose, then reverse, it's the same as rotate -90 degrees:
  rotate_counter_clockwise <- function(x) { apply(     t(x),2, rev)}

  if (!missing(rotate)) {
    if(rotate == "left") {
      l <- lapply(1:times, function(i) {
        m <- t(matrix(df[, i],
                      ncol = cols,
                      nrow = rows,
                      byrow = TRUE))
        rotate_counter_clockwise(m)
      })
    } else {
      l <- lapply(1:times, function(i) {
        m <- t(matrix(df[, i],
                      ncol = cols,
                      nrow = rows,
                      byrow = TRUE))
        rotate_clockwise(m)
      })
    }

    if(flip) l <- lapply(seq_along(l), function(i) {l[[i]][, rows:1]})

    e <- simplify2array(l)

  } else {
    l <- lapply(1:times, function(i) {
      m <- t(matrix(df[, i],
               ncol = cols,
               nrow = rows,
               byrow = TRUE))

    })

    if(flip) l <- lapply(seq_along(l), function(i) {l[[i]][, rows:1]})

    e <- simplify2array(l)
  }

  class(e) <- c("GriddedEmissionsArray",class(e))
  return(e)
}

#' @rdname GriddedEmissionsArray
#' @method print GriddedEmissionsArray
#' @export
print.GriddedEmissionsArray <- function(x,  ...) {
  e <- x
  cat(paste0("This GriddedEmissionsArray has:\n",
                 dim(e)[1], " lat points\n",
                 dim(e)[2], " lon points\n",
                 dim(e)[3],  " hours\n"))
  print(utils::head(e))
}

#' @rdname GriddedEmissionsArray
#' @method summary GriddedEmissionsArray
#' @export
summary.GriddedEmissionsArray <- function(object, ...) {
  e <- object
  summary(e[, , ])
  }

#' @rdname GriddedEmissionsArray
#' @method plot GriddedEmissionsArray
#' @export
plot.GriddedEmissionsArray <- function(x, ..., times = 1) {
  e <- x
  graphics::image(e[ , , times], ...)
  graphics::par(mfrow = c(1, 1))
}
