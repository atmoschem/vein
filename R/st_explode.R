#' Split line by vertex (experimental)
#'
#' @description \code{\link{st_explode}} split a lines data.frame into
#' each vertex. It to mimic the function explode from qgis, that the reason for the name
#' \code{\link{st_explode}}
#' @param net A spatial dataframe of class "sp" or "sf". When class is "sp"
#' it is transformed to "sf".
#' @importFrom sf st_sf st_as_sf st_set_geometry st_length  st_intersection
#' @importFrom eixport sfx_explode
#' @export
#' @note All variables are transformed into numeric.
#'
#' Thanks Michael Summer ([at]mdsummer)  for the function sfx_explode!
#' @examples \dontrun{
#' data(net)
#' net <- sf::st_as_sf(net)
#' # transformign factor in numeric
#' net2 <- st_explode(net)
#' dim(net)
#' dim(net2)
#' }
st_explode <- function(net){
  net <- sf::st_as_sf(net)
  net$id <- NULL
  netdf <- sf::st_set_geometry(net, NULL)
  namesnet <- names(netdf)

  for(i in 1:length(namesnet)) {
    netdf[[namesnet[i]]] <- as.numeric(as.character(netdf[[namesnet[i]]]))
  }

  snetdf <- sum(netdf, na.rm = TRUE)
  net$LKM <- sf::st_length(net)

  df <- eixport::sfx_explode(net)# Thanks @mdsummer
    for(i in 1:length(namesnet)) {
    df[[namesnet[i]]] <- as.numeric(as.character(df[[namesnet[i]]]))
  }

  df$LKM2 <- sf::st_length(df)
  df <- as.data.frame(df)
  df[, namesnet] <- df[, namesnet] * as.numeric(df$LKM2/df$LKM)
  df[, namesnet] <- df[, namesnet] * snetdf/sum(df[, namesnet], na.rm = TRUE)

  cat(paste0("Sum: ", round(sum(df[, namesnet], na.rm = T), 2), "\n"))

  df <- sf::st_sf(as.data.frame(df), geometry = df$geometry)

  sf::st_sf(as.data.frame(df), geometry = df$geometry)

}
