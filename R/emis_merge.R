#' Merge several emissions files returning data-frames or 'sf' of lines
#'
#' @description \code{\link{emis_merge}} reads rds files and returns a data-frame
#'  or an object of 'spatial feature' of streets, merging several files.
#'
#' @param path Character. Path where emissions are located
#' @param pol Character. Pollutant.
#' @param what Character. Word to search the emissions names, "STREETS", "DF" or
#' whatever name. It is important to include the extension .'rds'
#' @param net 'Spatial feature' or 'SpatialLinesDataFrame' with the streets.
#' It is expected #' that the number of rows is equal to the number of rows of
#' street emissions. If #' not, the function will stop.
#' @param streets Logical. If true, \code{\link{emis_merge}} will read the street
#' emissions created with \code{\link{emis_post}} by "streets_wide", returning an
#' object with class 'sf'. If false, it will read the emissions data-frame and
#' rbind them.
#' @return 'Spatial feature' of lines or a dataframe of emissions
#' @importFrom data.table rbindlist .SD
#' @importFrom sf st_set_geometry st_sf st_geometry st_as_sf
#' @importFrom utils glob2rx
#' @export
#' @examples \dontrun{
#' # Do not run
#'
#' }
emis_merge <- function (pol = "CO",
                        what = "STREETS.rds",
                        streets = T,
                        net,
                        path = "emi"){
  x <- list.files(path = path,
                  pattern = glob2rx(paste0(pol, "_*", what)),
                  all.files = T,
                  full.names = T,
                  recursive = T)
  xx <- list.files(path = path,
                   pattern = glob2rx(paste0(pol, "_*", what)),
                   recursive = T)
  cat("\nReading emissions from:\n")
  print(xx)
  x_rds <- lapply(x, readRDS)

    nombres <- names(x_rds[[1]])

    if(streets){
    for (i in 1:length(x_rds)){
      x_rds[[i]]$id <- 1:nrow(x_rds[[i]])
    }
    x_st <- data.table::rbindlist(x_rds)
    x_st <- as.data.frame(x_st[, lapply(.SD, sum, na.rm=TRUE),
                               by = "id",
                               .SDcols = nombres ])
    if(nrow(x_st) != nrow(net)){
      stop("Number of rows of net must be equal to number of rows of estimates")
    }
    net <- sf::st_as_sf(net)
    netx <- st_sf(x_st, geometry = sf::st_geometry(net))
    return(netx)
  } else{
    x_st <- as.data.frame(data.table::rbindlist(x_rds))
    return(x_st)
  }
}

