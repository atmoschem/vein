#' Returns amount of vehicles at each age
#'
#' @description \code{my_age} returns amount of vehicles at each age using a
#' numeric vector.
#'
#' @param x Numeric; vehicles by street (or spatial feature).
#' @param y Numeric; vehicles by age of use
#' @param name of vehicle assigned to columns of dataframe.
#' @param k multiplication factor.
#' @param net SpatialLinesDataFrame or Spatial Feature of "LINESTRING"
#' @param message message with average age and total numer of vehicles.
#' @return dataframe of age distrubution of vehicles.
#' @importFrom sf st_sf st_as_sf
#' @export
#' @examples {
#' data(net)
#' dpc <- c(seq(1,20,3), 20:10)
#' PC_E25_1400 <- my_age(x = net$ldv, y = dpc, name = "PC_E25_1400")
#' class(PC_E25_1400)
#' plot(PC_E25_1400)
#' PC_E25_1400sf <- my_age(x = net$ldv, y = dpc, name = "PC_E25_1400", net = net)
#' class(PC_E25_1400sf)
#' plot(PC_E25_1400sf)
#' PC_E25_1400nsf <- sf::st_set_geometry(PC_E25_1400sf, NULL)
#' class(PC_E25_1400nsf)
#' }
my_age <- function (x,
                    y,
                    name = "veh",
                    k = 1,
                    net,
                    message = TRUE){
  if (missing(x) | is.null(x)) {
    stop (print("Missing vehicles"))
    } else if (missing(y) | is.null(y)) {
      stop (print("Missing distributed vehicles"))
    } else {
      y <- y[!is.na(y)]
    d <- matrix(data = y/sum(y), nrow = 1, ncol=length(y))
    df <- as.data.frame(as.matrix(x) %*%d)
    names(df) <- paste(name,seq(1,length(y)),sep="_")
    if(message){
      message(paste("Average age of",name, "is",
                  round(sum(seq(1,length(y))*base::colSums(df)/sum(df),na.rm = T), 2),
                  sep=" "))
    message(paste("Number of",name, "is",
                  round(sum(df*k, na.rm = T)/1000, 3),
                  "* 10^3 veh",
                  sep=" ")
            )
    cat("\n")
    }}
  df <- Vehicles(df*k)
  if(!missing(net)){
    netsf <- sf::st_as_sf(net)
    dfsf <- sf::st_sf(df, geometry = netsf$geometry)
    return(dfsf)
  } else {
    return(df)
  }
}
