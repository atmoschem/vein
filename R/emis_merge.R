#' Merge several emissions files returning data-frames or 'sf' of lines
#'
#' @description \code{\link{emis_merge}} reads rds files and returns a data-frame
#'  or an object of 'spatial feature' of streets, merging several files.
#'
#' @param path Character. Path where emissions are located
#' @param pol Character. Pollutant.
#' @param what Character. Word to search the emissions names, "STREETS", "DF" or
#' whatever name. It is important to include the extension .'rds'. For instance,
#' If you have several files "XX_CO_STREETS.rds", what should be "STREETS.rds"
#' @param net 'Spatial feature' or 'SpatialLinesDataFrame' with the streets.
#' It is expected #' that the number of rows is equal to the number of rows of
#' street emissions. If #' not, the function will stop.
#' @param streets Logical. If true, \code{\link{emis_merge}} will read the street
#' emissions created with \code{\link{emis_post}} by "streets_wide", returning an
#' object with class 'sf'. If false, it will read the emissions data-frame and
#' rbind them.
#' @param crs coordinate reference system in numeric format from
#' http://spatialreference.org/ to transform/project spatial data using sf::st_transform
#' @param under "Character"; "after" when you stored your pollutant x as 'X_'
#' "before" when '_X' and "none" for merging directly the files.
#' @param as_list "Logical"; for returning the results as list or not.
#' @return 'Spatial feature' of lines or a dataframe of emissions
#' @importFrom data.table rbindlist .SD
#' @importFrom sf st_set_geometry st_sf st_geometry st_as_sf st_transform
#' @export
#' @examples \dontrun{
#' # Do not run
#'
#' }
emis_merge <- function (pol = "CO",
                        what = "STREETS.rds",
                        streets = T,
                        net,
                        path = "emi",
                        crs,
                        under = "after",
                        as_list = FALSE){
  x <- list.files(path = path,
                  pattern = what,
                  all.files = T,
                  full.names = T,
                  recursive = T)
  if(under == "after"){
    x <- x[grep(pattern = paste0(pol, "_"), x = x)]
  } else if (under == "before"){
    x <- x[grep(pattern = paste0("_", pol), x = x)]
  } else {
    x <-  x[grep(pattern = pol, x = x)]
  }

nx <- gsub(pattern = paste0(getwd(), '/', path),
           replacement = "", x = x)
kk <- substr(x = nx, start = 11, stop = 50)
kk <- gsub(pattern = "/", replacement = "", kk)
kk <- gsub(pattern = ".rds", replacement = "", kk)
nx <- kk

  cat("\nReading emissions from:\n")
  print(x)
  x_rds <- lapply(x, readRDS)
  names(x_rds) <- nx
  if(as_list) return(x_rds)

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
    if(!missing(crs)) {
      net <- sf::st_transform(net, crs)
    }
    netx <- st_sf(x_st, geometry = sf::st_geometry(net))
    return(netx)
  } else{
    x_st <- as.data.frame(data.table::rbindlist(x_rds))
    return(x_st)
  }
}

