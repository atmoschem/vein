#' Returns amount of vehicles at each age
#'
#' @family age
#'
#' @description \code{\link{age_veh}} returns amount of vehicles at each age
#' @param x Numeric; numerical vector of vehicles with length equal to lines features of road network
#' @param type "ldv", "hdv", or "mc" representing light vehicles, heavy vehicles or motorcycles
#' @param name Character; of vehicle assigned to columns of dataframe
#' @param a Numeric; parameter of survival equation
#' @param b Numeric; parameter of survival equation
#' @param agemin Integer; age of newest vehicles for that category
#' @param agemax Integer; age of oldest vehicles for that category
#' @param k Numeric; multiplication factor. If its length is > 1, it must match the length of x
#' @param bystreet Logical; when TRUE it is expecting that 'a' and 'b' are numeric vectors with length equal to x
#' @param net SpatialLinesDataFrame or Spatial Feature of "LINESTRING"
#' @param verbose Logical;  message with average age and total numer of vehicles
#' @param namerows Any vector to be change row.names. For instance, name of
#' regions or streets.
#' @param time Character to be the time units as denominator, eg "1/h"
#' @return dataframe of age distrubution of vehicles
#' @importFrom sf st_sf st_as_sf
#' @note
#' The functions age* produce distribution of the circulating fleet by age of use.
#' The order of using these functions is:
#'
#' 1. If you know the distribution of the vehicles by age of use , use:  \code{\link{my_age}}
#' 2. If you know the sales of vehicles, or the registry of new vehicles,
#' use \code{\link{age}} to apply a survival function.
#' 3. If you know the theoretical shape of the circulating fleet and you can use
#' \code{\link{age_ldv}}, \code{\link{age_hdv}} or \code{\link{age_moto}}. For instance,
#' you dont know the sales or registry of vehicles, but somehow you know
#' the shape of this curve.
#' 4. You can use/merge/transform/adapt any of these functions.
#' @export
#' @note It consists in a Gompertz equation with default parameters from
#' 1 national emissions inventory for green housegases in Brazil, MCT 2006
#' @examples \dontrun{
#' data(net)
#' PC_E25_1400 <- age_ldv(x = net$ldv, name = "PC_E25_1400")
#' plot(PC_E25_1400)
#' PC_E25_1400 <- age_ldv(x = net$ldv, name = "PC_E25_1400", net = net)
#' plot(PC_E25_1400)
#' }
age_veh <- function (x,
                     type = "ldv",
                     name = "age",
                     a = if(type == "ldv") 1.698 else 0.2,
                     b = if(type == "ldv") -0.2 else 17,
                     agemin = 1,
                     agemax = 50,
                     k = 1,
                     bystreet = F,
                     net,
                     verbose = FALSE,
                     namerows,
                     time){
  if(!type %in% c("ldv", "hdv", "mc"))stop("Type can be ldv, hdv or mc")

  # check na
  x[is.na(x)] <- 0

  x <- as.numeric(x)

  # length k
  if(length(k)  > 1 & length(k) < length(x)){
    stop("length of 'k' must be 1 ore equal to length of 'x'")
  }

  #check agemax
  if(agemax < 1) stop("Agemax should be bigger than 1")


  #bystreet = TRUE
  if (bystreet == T){
    if(length(x) != length(a)){
      stop("Lengths of veh and age must be the same")
    }

    # functions
    if(type == "ldv") {
      fx <- function (t) {1 - exp(-exp(a[i] + b[i]*t))}
    } else {
      fx <- function (t) {1/(1 + exp(a[i]*(t+b[i])))+1/(1 + exp(a[i]*(t-b[i])))}
    }


    d <- suca <- list()
    for (i in seq_along(x)) {
      suca[[i]] <- fx
      anos <- seq(agemin,agemax)
      d[[i]] <- (-1)*diff(suca[[i]](anos))
      d[[i]][length(d[[i]])+1] <- d[[i]][length(d[[i]])]
      d[[i]] <- d[[i]] + (1 - sum(d[[i]]))/length(d[[i]])
      d[[i]] <- d[[i]]*x[i]
    }
    df <- as.data.frame(matrix(0,ncol=length(anos), nrow=1))
    for (i in seq_along(x)) {
      df[i,] <- d[[i]]
    }

    df <- as.data.frame(cbind(as.data.frame(matrix(0,ncol=agemin-1,
                                                   nrow=length(x))),
                              df))

    names(df) <- paste(name,seq(1,agemax),sep="_")

    df <- df*k



    #bystreet = FALSE
  } else {
    # functions
    if(type == "ldv") {
      fx <- function (t) {1 - exp(-exp(a + b*t))}
    } else {
      fx <- function (t) {1/(1 + exp(a*(t+b)))+1/(1 + exp(a*(t-b)))}
    }

    suca <- fx
    anos <- seq(agemin,agemax)
    d <- (-1)*diff(suca(anos))
    d[length(d)+1] <- d[length(d)]
    d <- d + (1 - sum(d))/length(d)
    df <- as.data.frame(as.matrix(x) %*%matrix(d,ncol=length(anos), nrow=1))

    df <- as.data.frame(cbind(as.data.frame(matrix(0,ncol=agemin-1,
                                                   nrow=length(x))),
                              df))

    names(df) <- paste(name,seq(1,agemax),sep="_")

    df <- df*k

  }

  if(verbose){
    message(paste("Average age of",name, "is",
                  round(sum(seq(1,agemax)*base::colSums(df, na.rm = T)/sum(df, na.rm = T)), 2),
                  sep=" "))
    message(paste("Number of",name, "is",
                  round(sum(df, na.rm = T)/1000, 2),
                  "* 10^3 veh",
                  sep=" ")
    )
    cat("\n")
  }
  if(!missing(namerows)) {
    if(length(namerows) != nrow(df)) stop("length of namerows must be the length of number of rows of veh")
    row.names(df) <- namerows
  }

  if(!missing(net)){
    netsf <- sf::st_as_sf(net)
    if(!missing(time)){
      dfsf <- sf::st_sf(Vehicles(df*k, time = time), geometry = sf::st_geometry(netsf))
    } else {
      dfsf <- sf::st_sf(Vehicles(df*k), geometry = sf::st_geometry(netsf))
    }
    return(dfsf)
  } else {
    if(!missing(time)){
      return(Vehicles(df*k, time = time))
    } else {
      return(Vehicles(df*k))
    }
  }

}
