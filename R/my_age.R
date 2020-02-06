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
#' @param agemax Integer; age of oldest vehicles for that category
#' @param name Character; of vehicle assigned to columns of dataframe.
#' @param k Integer; multiplication factor. If its length is > 1, it must match the length of x
#' @param pro_street Character; each category of profile for each street.
#' The length of this character vector must be equal to the length of 'x'. The
#' names of the data.frame 'y' must be have the same content of 'pro_street'
#' @param net SpatialLinesDataFrame or Spatial Feature of "LINESTRING"
#' @param verbose Logical; message with average age and total numer of vehicles.
#' @param namerows Any vector to be change row.names. For instance, name of
#' regions or streets.
#' @return dataframe of age distrubution of vehicles.
#' @importFrom sf st_sf st_as_sf
#' @note
#'
#'
#' The functions age* produce distribution of the circulating fleet by age of use.
#' The order of using these functions is:
#'
#' 1. If you know the distribution of the vehicles by age of use , use:  \code{\link{my_age}}
#' 2. If you know the sales of vehicles, or (the regis)*better) the registry of new vehicles,
#' use \code{\link{age}} to apply a survival function.
#' 3. If you know the theoretical shape of the circulating fleet and you can use
#' \code{\link{age_ldv}}, \code{\link{age_hdv}} or \code{\link{age_moto}}. For instance,
#' you dont know the sales or registry of vehicles, but somehow you know
#' the shape of this curve.
#' 4. You can use/merge/transform/adapt any of these functions.
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
                    agemax,
                    name = "vehicle",
                    k = 1,
                    pro_street,
                    net,
                    verbose = FALSE,
                    namerows){

  # length k
  if(length(k)  > 1 & length(k) < length(x)){
    stop("length of 'k' must be 1 ore equal to length of 'x'")
  }

# check y
  if(!is.data.frame(y)){
    y <- as.numeric(y)
    y[is.na(y)] <- 0
  } else {
    for(i in 1:ncol(y)) y[, i] <- as.numeric( y[, i])
    y[is.na(y)] <- 0

  }
  #
  # start
  if (missing(x) | is.null(x)) {
    stop("Missing vehicles")
  } else {
    if(!missing(pro_street)){
      if(class(y) != "data.frame"){
        stop("'y' must be 'data.frame'")
      }
      for(i in 1:ncol(y)) y[, i] <-  y[, i]/sum( y[, i])

      d <- as.data.frame(t(y / sum(y)))
      d$cat <- names(y)
      dfnet <- data.frame(cat = pro_street,
                          veh = x)
      dl <- list()
      for(i in 1:length(x)){
        if(dfnet$cat[i] %in% d$cat){
          dl[[i]] <- as.matrix(x[i]) %*%matrix(y[[dfnet$cat[i]]],
                                               ncol = nrow(y),
                                               nrow = 1)
        }
      }
      df <- as.data.frame(do.call("rbind", dl))
      names(df) <- paste(name, seq(1, length(df)), sep="_")
    } else {
      if(mode(y) != "numeric") stop("When there is no 'pro_street', 'y' must be 'numeric'")
      d <- matrix(data = y/sum(y), nrow = 1, ncol=length(y))
      df <- as.data.frame(as.matrix(x) %*%d)
      names(df) <- paste(name, seq(1, length(df)), sep="_")
    }
    # check k
      df <- df*k

    # verbose
    if(verbose){
      secu <- seq(1, ncol(df))
      colus <- colSums(df, na.rm = T)
      sumdf <- sum(df, na.rm = T)
      message(paste("Average age of", name, "is",
                    round(sum(secu*colus/sumdf, na.rm = T), 2),
                    sep=" "))
      message(paste("Number of",name, "is",
                    round(sum(df, na.rm = T), 3),
                    " [veh]",
                    sep=" "))
      cat("\n")
    }
  }
  # ending
  df <- Vehicles(df)
  if(!missing(namerows)) {
    if(length(namerows) != nrow(df)) stop("length of namerows must be the length of number of rows of veh")
    row.names(df) <- namerows
  }
  if(!missing(net)){
    netsf <- sf::st_as_sf(net)
    df <- sf::st_sf(df, geometry = netsf$geometry)
  }
  if(!missing(agemax)) df <- df[, 1:agemax]
  # replace NA and NaN
  df[is.na(df)] <- 0
  return(df)
}
