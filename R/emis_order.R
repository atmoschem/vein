#' Re-order the emission to match specific hours and days
#'
#' @description returns the emission array matching with corresponding weekdays and with
#' the desired number of hours, recycling or droping hours from the emission array.
#'
#' @param EMISSION one of the following:
#'
#' 1) GriddedEmissionsArray or array with characteristics of GriddedEmissionsArray
#' 2) Spatial object of class "Spatial". Columns are hourly emissions.
#' 3) Spatial Object of class "sf". Columns are hourly emissions.
#' 4) "data.frame", "matrix" or "Emissions". Columns are hourly emissions.
#'
#' @param start Date or the start weekday or first 3 letters
#' @param hours Numeric; number of hours needed to the simualation
#' @param utc Integer; transform local into UTC emissions. For instance,
#' utc = -3 means that the first hour of emissions is at 21:00 of the previous
#' day.
#' @param verbose Logical; display additional information
#' @aliases weekly
#' @importFrom sp CRS
#' @importFrom sf st_as_sf st_geometry st_set_geometry st_sf st_crs
#' @format Emissions
#'
#' @author Daniel Schuch& Sergio Ibarra
#' @return GriddedEmissionsArray, sf or data.frame, depending on the class of
#' EMISSION
#' @export
#' @note This function assumes that the emissions have hours with length of
#' factor of 24, e.g: 24 hours, 24*2 hours etc. Then, it re-order the emissions
#' by the hours of estimations to match another length of emissions. For
#' instance, if the input covers 168 hours and it is desired an object of 241
#' hours that start saturday, this function can do that. It is useful when you
#' are going to start a air quality simulation for specific periods of time.
#' @examples \dontrun{
#' wCO <- emis_order(CO, start = "sat", hours = 24, verbose = TRUE)
#' wCO <- emis_order(CO, start = as.Date("2016-04-06"), hours = 241, verbose = TRUE)
#' }
#'
emis_order <- function(EMISSION, start = "mon", hours = 168,
                       utc, verbose = FALSE){
  if(verbose){
    cat(paste0("Dimensions of emissions :\n"))
    cat(dim(EMISSION))
  }
  seg <- 1:24
  ter <- 25:48
  qua <- 49:72
  qui <- 73:96
  sex <- 97:120
  sab <- 121:144
  dom <- 145:168
  if(class(start)[1] == "Date"){
    if(verbose)
      cat("using date:", paste(start),"\n")
    s <- as.numeric(strftime(start), fomat = "%d")
    if(s == 1) start = "mon"
    if(s == 2) start = "tue"
    if(s == 3) start = "wed"
    if(s == 4) start = "thu"
    if(s == 5) start = "fri"
    if(s == 6) start = "sat"
    if(s == 7) start = "sun"
  }
  if(is.character(start)){
    if(start == "mon")
      index <- suppressWarnings(matrix( c(seg,ter,qua,qui,sex,sab,dom),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(start == "tue")
      index <- suppressWarnings(matrix( c(ter,qua,qui,sex,sab,dom,seg),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(start == "wed")
      index <- suppressWarnings(matrix( c(qua,qui,sex,sab,dom,seg,ter),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(start == "thu")
      index <- suppressWarnings(matrix( c(qui,sex,sab,dom,seg,ter,qua),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(start == "fri")
      index <- suppressWarnings(matrix( c(sex,sab,dom,seg,ter,qua,qui),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(start == "sat")
      index <- suppressWarnings(matrix( c(sab,dom,seg,ter,qua,qui,sex),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(start == "sun")
      index <- suppressWarnings(matrix( c(dom,seg,ter,qua,qui,sex,sab),
                                        ncol = 1, nrow = hours, byrow = T ))
    if(!missing(utc) & utc > 0){
      index <- c(index[abs(-3):length(index)],
                 max(index[abs(-3):length(index)]):(length(index) -1))
    } else if(!missing(utc) & utc < 0){
      AA <- length(index) + utc
      a <- index[(AA + 1):length(index)]
      b <- index[1:(length(index) + utc)]
      index <- c(a, b)
    }

    if(verbose)
      cat("emissions starting at",start,"with",hours,"hours\n")
  }
  else{
    stop("invalid start argument!\n") # nocov
  }
  if(class(EMISSION)[1] %in% c("GriddedEmissionsArray", "array")){
    # this lines rearange the GriddedEmissionsArray output to be used in wrf_put
    NEW   <- array(NA, dim = c(dim(EMISSION)[1],dim(EMISSION)[2], hours))
    NEW   <- EMISSION[, , c(index)]
    class(NEW) <- "GriddedEmissionsArray"
    return(NEW)
  } else if(class(EMISSION)[1] == c("Spatial")){
    sr <- as.numeric(substr(sp::CRS(EMISSION), 12, nchar(sr)))
    id <- 1:nrow(EMISSION)
    EMISSION$id <- NULL
    df <-  sf::st_as_sf(EMISSION)
    geo <- sf::st_geometry(df)
    df <- sf::st_set_geometry(df, NULL)
    dft   <- df[, index]
    dft <- cbind(data.frame(id = id), dft)
    dft <- sf::st_sf(dft, geometry = geo, crs = sr)
    return(dft)
  } else if(class(EMISSION)[1] == c("sf")){
    id <- 1:nrow(EMISSION)
    EMISSION$id <- NULL
    geo <- sf::st_geometry(EMISSION)
    df <- sf::st_set_geometry(EMISSION, NULL)
    dft   <- df[, index]
    dft <- cbind(data.frame(id = id), dft)
    dft <- sf::st_sf(dft, geometry = geo, crs = sf::st_crs(EMISSION))
    return(dft)
  }
  if(class(EMISSION)[1] %in% c("data.frame", "matrix", "Emissions")){
    id <- 1:nrow(EMISSION)
    EMISSION$id <- NULL
    df <- as.data.frame(EMISSION)
    dft   <- df[, index]
    dft <- cbind(data.frame(id = id), dft)
    return(dft)
  }
}
