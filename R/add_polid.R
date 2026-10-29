#' Add polygon id to lines road network
#'
#' @description Sometimes you need to add polygon id into your streets road network.
#' \code{\link{add_polid}} add add_polid id into your road network intersecting
#' (\code{\link[sf]{st_intersection}}) your network with each polygon.
#' Streets crossing polygon boundaries are split.
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
add_polid <- function(polyg, street, by) {
  polyg <- sf::st_as_sf(polyg)
  street <- sf::st_as_sf(street)
  geo <- sf::st_geometry(polyg)
  do.call("rbind",
    lapply(seq_len(nrow(polyg)), function(i) {
      b <- suppressWarnings(sf::st_intersection(x = street, y = geo[i]))
      if (nrow(b) > 0 &&
        any(sf::st_geometry_type(b) == "GEOMETRYCOLLECTION")) {
        b <- suppressWarnings(sf::st_collection_extract(b, "LINESTRING"))
      }
      # remove points from touching boundaries
      b <- b[sf::st_dimension(b) %in% 1, ]
      b <- sf::st_cast(b, "MULTILINESTRING")
      b[[by]] <- rep(polyg[[by]][i], nrow(b))
      b
    }))
}
