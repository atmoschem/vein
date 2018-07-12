#' Re-order the emission to match specific hours and days
#'
#' @description returns the emission array matching with corresponding weekdays and with
#' the desired number of hours, recycling or droping hours from the emission array.
#'
#' @param EMISSION GriddedEmissionsArray or array with characteristics of
#' GriddedEmissionsArray
#' @param start Date or the start weekday or first 3 letters
#' @param hours Numeric; number of hours needed to the simualation
#' @param verbose Logical; display additional information
#' @aliases weekly
#'
#' @format Emissions
#'
#' @author Daniel Schuch
#'
#' @export
#' @note This function assumes that the emissions have hours with length of
#' factor of 24, e.g: 24 hours, 24*2 hours etc. Then, it re-order the emissions
#' by the hours of estimations to match another length of emissions. For
#' instance, if the input covers 168 hours and it is desired an object of 241
#' hours that start saturday, this function can do that. It is useful when you
#' are going to start a air quality simulation for specific periods of time.
#'
#' @examples \dontrun{
#' wCO <- weekly(CO, start = "sat", hours = 24, verbose = TRUE)
#' wCO <- weekly(CO, start = as.Date("2016-04-06"), hours = 241, verbose = TRUE)
#' }
#'

emis_order <- function(EMISSION, start = "mon", hours = 168, verbose = FALSE){
  seg <- 1:24
  ter <- 25:48
  qua <- 49:72
  qui <- 73:96
  sex <- 97:120
  sab <- 121:144
  dom <- 145:168

  if(class(start) == "Date"){
    if(verbose)
      cat("using date:",paste(start),"\n")
    s <- as.POSIXlt(as.Date(start))$wday
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
    if(verbose)
      cat("emissions starting at",start,"with",hours,"hours\n")
  }
  else{
    stop("invalid start argument!\n") # nocov
  }

  # this lines rearange the GriddedEmissionsArray output to be used in wrf_put
  if(class(EMISSION) %in% c("GriddedEmissionsArray", "array")){
    NEW   <- array(NA, dim = c(dim(EMISSION)[1],dim(EMISSION)[2], hours))
    NEW   <- EMISSION[, , c(index)]
    class(NEW) <- "GriddedEmissionsArray"
    return(NEW)
  } else {
    stop("Currently, only supports GriddedEmissionsArray or array\n")
  }
}
