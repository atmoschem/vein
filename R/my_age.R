#' Returns amount of vehicles at each age
#'
#' @description \code{my_age} returns amount of vehicles at each age using a
#' numeric vector.
#'
#' @param x Numeric; vehicles by street (or spatial feature).
#' @param y Numeric or data.frame; when pro_street is not available, y must be
#' 'numeric', else, a 'data.frame'. The names of the columns of this data.frame
#' must be the same of the elements of pro_street and each column must have a
#' profile of age of use of vehicle. When 'y' is 'numeric' the vehicles
#' has the same age distribution to all street. When 'y' is a data.frame,
#' the distribution by age of use varies the streets.
#' @param name Character; of vehicle assigned to columns of dataframe.
#' @param k Integer; multiplication factor.
#' @param pro_street Character; each category of profile for each street.
#' The length of this character vector must be equal to the length of 'x'. The
#' characters of this vector must be the same of the 'data.frame' 'y'. When
#' pro_street is not used, 'y' must be a numeric vector.
#' @param net SpatialLinesDataFrame or Spatial Feature of "LINESTRING"
#' @param message Logical; message with average age and total numer of vehicles.
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
#' yy <- data.frame(a = 1:5, b = 5:1)    # perfiles por categoria de calle
#' pro_street <- c("a", "b", "a")         # categorias de cada calle
#' x <- c(100,5000, 3)                               # vehiculos
#' my_age(x = x, y =  yy, pro_street = pro_street)
#' }
my_age <- function (x,
                    y,
                    name = "age",
                    k = 1,
                    pro_street,
                    net,
                    message = TRUE){
  if (missing(x) | is.null(x)) {
    stop (print("Missing vehicles"))
  } else if (missing(y) | is.null(y)) {
    stop (print("Missing distributed vehicles"))
  } else {
    if(!missing(pro_street)){
      if(class(y) != "data.frame"){
        stop("'y' must be 'data.frame'")
      }
      d <- as.data.frame(t(y / sum(y)))
      d$cat <- names(y)
      dfnet <- data.frame(cat = pro_street,
                          veh = x)
      veh <- merge(x = dfnet, y = d, by = "cat", all = T)
      veh <- veh[, names(d)]
      veh$cat <- NULL
      df <- veh * dfnet$veh
      names(df) <- paste(name, seq(1, length(df)), sep="_")
    } else {
      if(mode(y) != "numeric") stop("'y' must be 'numeric'")
      d <- matrix(data = y/sum(y), nrow = 1, ncol=length(y))
      df <- as.data.frame(as.matrix(x) %*%d)
      names(df) <- paste(name, seq(1, length(df)), sep="_")
    }
    if(message){
      if(!missing(pro_street)){
        message(paste("Average age of", name, "is",
                      round(sum(seq(1,ncol(df))*base::colSums(df)/sum(df),na.rm = T), 2),
                      sep=" "))
        message(paste("Number of",name, "is",
                      round(sum(df, na.rm = T)/1000, 3),
                      "* 10^3 veh",
                      sep=" ")
        )
        cat("\n")

      } else {
      message(paste("Average age of", name, "is",
                    round(sum(seq(1,length(y))*base::colSums(df)/sum(df),na.rm = T), 2),
                    sep=" "))
      message(paste("Number of",name, "is",
                    round(sum(df*k, na.rm = T)/1000, 3),
                    "* 10^3 veh",
                    sep=" ")
      )
      cat("\n")
      }
      }
    }
  df <- vein::Vehicles(df*k)
  if(!missing(net)){
    netsf <- sf::st_as_sf(net)
    dfsf <- sf::st_sf(df, geometry = netsf$geometry)
    return(dfsf)
  } else {
    return(df)
  }
}
