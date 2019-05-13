#' Add polygon id to lines road network
#'
#'@description Sometimes you need to add polygon id into your streets road network.
#'\code{\link{add_polid}} add add_polid id into your road network cropping your
#' network by
#'
#' @param polyg sf object POLYGON
#' it is transformed to "sf" with emissions.
#' @param street streets road network class 'sf'
#' @param by Character indicating the column woith the id in polyg
#' @importFrom sf st_crop st_cast
#' @export
#' @seealso \code{\link{emis_to_streets}}
#' @examples \dontrun{
#' #to do
#' }
add_polid <- function(polyg, street, by){
  do.call("rbind",
          lapply(1:nrow(polyg), function(i){
            b <- sf::st_crop(x = street, polyg[i, ])
            b <- sf::st_cast(b, "MULTILINESTRING")
            b[[by]] <- polyg[[by]][i]
            b
          }))
}


