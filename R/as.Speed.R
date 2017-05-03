#' Construction function for class "Speed"
#'
#' @description Returns a tranformed object with class "Speed" and units
#'  km/h. This functions includes two arguments, distance and time. Therefore,
#'  it is posibel to change the units of the speed to "m" to "s" for example.
#'
#' @return Constructor for class "Speed". Allows specification of units.
#' @param distance
#' @param time
#' @export
#' @examples \dontrun{
#' data(net)
#' lt <- as.Vehicles(net$hdv)
#' class(lt)
#' plot(lt)
#' }
as.Speed <- function(spd, distance = "km", time = "h", ...) {
  if  (is.matrix(spd)) {
    spd <- as.data.frame(spd)
    for(i in 1:ncol(spd)){
      spd[,i] <- spd[,i] * parse_unit(paste0(distance," ", time,"-1"))
    }
    class(spd) <- c("Speed",class(spd))
  } else if (is.data.frame(spd)) {
    for(i in 1:ncol(spd)){
      spd[,i] <- spd[,i] * parse_unit(paste0(distance," ", time,"-1"))
    }
    class(spd) <- c("Speed",class(spd))
  } else if (is.list(spd) && is.list(spd[[1]])) {
    for (i in 1:length(spd) ) {
      for (j in 1:length(spd[[1]]) ) {
        spd[[i]][[j]] <- spd[[i]][[j]] * parse_unit(paste0(distance," ", time,"-1"))
      }
    }
    #SpeedList?
  } else {
    units(spd) <- spd * parse_unit(paste0(distance," ", time,"-1"))
  }
  return(spd)
}
