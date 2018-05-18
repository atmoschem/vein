#' Returns amount of vehicles at each age
#'
#' @description Returns amount of vehicles at each age
#'
#' @param x numerical vector of vehicles with length equal to lines features of raod network
#' @param name of vehicle assigned to columns of dataframe
#' @param a parameter of survival equation
#' @param b parameter of survival equation
#' @param agemin age of newest vehicles for that category
#' @param agemax age of oldest vehicles for that category
#' @param k multiplication factor
#' @param bystreet when TRUE it is expecting that 'a' and 'b' are numeric vectors with length equal to x
#' @param net SpatialLinesDataFrame or Spatial Feature of "LINESTRING"
#' @param message message with average age and total numer of vehicles
#' @return dataframe of age distrubution of vehicles
#' @importFrom sf st_sf st_as_sf
#' @export
#' @examples {
#' data(net)
#' LT_B5 <- age_hdv(x = net$hdv,name = "LT_B5")
#' plot(LT_B5)
#' LT_B5 <- age_hdv(x = net$hdv, name = "LT_B5", net = net)
#' plot(LT_B5)
#' }
age_hdv <- function (x,
                     name = "veh",
                     a = 0.2,
                     b = 17,
                     agemin = 1,
                     agemax = 50,
                     k = 1,
                     bystreet = F,
                     net,
                     message = TRUE){
  if (missing(x) | is.null(x)) {
    stop (print("Missing vehicles"))
  } else if (bystreet == T){
    if(length(x) != length(a)){
      stop((print("Lengths of veh and age must be the same")))
    }
    d <- suca <- list()
    for (i in seq_along(x)) {
      suca[[i]] <- function (t) {1/(1 + exp(a[i]*(t+b[i])))+1/(1 + exp(a[i]*(t-b[i])))}
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
      if (agemin > 1) {
      df <- cbind(as.data.frame(matrix(0,ncol=agemin-1, nrow=length(x))),
                  df)
    } else {df <- df}
    if(message){
    names(df) <- paste(name,seq(1,agemax),sep="_")
    message(paste("Average age of",name, "is",
                  round(sum(seq(1,agemax)*base::colSums(df)/sum(df)), 2),
                  sep=" "))
    message(paste("Number of",name, "is",
                  round(sum(df*k, na.rm = T)/1000, 2),
                  "* 10^3 veh",
                  sep=" ")
    )
    cat("\n")
    }
    if(!missing(net)){
      netsf <- sf::st_as_sf(net)
      dfsf <- sf::st_sf(Vehicles(df*k), geometry = netsf$geometry)
      return(dfsf)
    } else {
      return(Vehicles(df*k))
    }

  } else {
    suca <- function (t) {1/(1 + exp(a*(t+b)))+1/(1 + exp(a*(t-b)))}
    anos <- seq(agemin,agemax)
    d <- (-1)*diff(suca(anos))
    d[length(d)+1] <- d[length(d)]
    d <- d + (1 - sum(d))/length(d)
    df <- as.data.frame(as.matrix(x) %*%matrix(d,ncol=length(anos), nrow=1))
    if (agemin > 1) {
      df <- cbind(as.data.frame(matrix(0,ncol=agemin-1, nrow=length(x))),
                  df)
    } else {df <- df}
    if(message){
    names(df) <- paste(name,seq(1,agemax),sep="_")
    message(paste("Average age of",name, "is",
                  round(sum(seq(1,agemax)*base::colSums(df)/sum(df)), 2),
                  sep=" "))
    message(paste("Number of",name, "is",
                  round(sum(df*k, na.rm = T)/1000, 2),
                  "* 10^3 veh",
                  sep=" ")
    )
    cat("\n")
    }
    if(!missing(net)){
      netsf <- sf::st_as_sf(net)
      dfsf <- sf::st_sf(Vehicles(df*k), geometry = netsf$geometry)
      return(dfsf)
    } else {
      return(Vehicles(df*k))
    }
    }
}
