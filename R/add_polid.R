#' Add polygon id to lines road network
#'
#'@description Sometimes you need to add polygon id into your streets road network.
#'\code{\link{add_polid}} add add_polid id into your road network cropping your
#' network by.
#'
#' For instance, you have open street maps road network the you have the
#' polygon of your regions. This function adds the id of your polygon
#' as a new column in the streets network.
#'
#' @param polyg sf object POLYGON or sp
#' @param street streets road network class sf or sp
#' @param by Character indicating the column with the id in polyg
#' @importFrom sf st_crop st_cast st_as_sf
#' @export
#' @seealso \code{\link{emis_to_streets}}
#' @examples \dontrun{
#' data(net)
#' nets <- sf::st_as_sf(net)
#' bb <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(nets)))
#' bb$id <- "a"
#' a <- add_polid(polyg = bb, street = nets, by = "id")
#' }
add_polid <- function(polyg, street, by){
  polyd <- sf::st_as_sf(polyg)
  street <- sf::st_as_sf(street)
  do.call("rbind",
          lapply(1:nrow(polyg), function(i){
            b <- suppressWarnings(sf::st_crop(x = street, polyg[i, ]))
            b <- sf::st_cast(b, "MULTILINESTRING")
            b[[by]] <- polyg[[by]][i]
            b
          }))
}


