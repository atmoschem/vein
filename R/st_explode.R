#' Split line by vertex (experimental)
#'
#' @description \code{\link{st_explode}} split a lines data.frame into
#' each vertex. It to mimic the function explode from qgis, that the reason for the name
#' \code{\link{st_explode}}
#' @param net A spatial dataframe of class "sp" or "sf". When class is "sp"
#' it is transformed to "sf".
#' @importFrom sf st_sf st_as_sf st_set_geometry st_length  st_intersection
#' @export
#' @note All variables are transformed into numeric.
#' @examples {
#' data(net)
#' net <- sf::st_as_sf(net)[1:10, ]
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
    netdf[[i]] <- as.numeric(netdf[[i]])
  }
  snetdf <- sum(netdf, na.rm = TRUE)
  net$LKM <- sf::st_length(net)
  lg <- lapply(net$geometry, matrix, ncol = 2)


  dfl <- lapply(1:nrow(net), function(j){

    dfa <- sf::st_set_geometry(net[j, ], NULL)

    m1 <- lg[[j]]

    la <- lapply(1:(nrow(m1) - 1), function(i){
      c(i, i+1)
    })

    sf::st_sf(as.data.frame(data.table::rbindlist(lapply(1:length(la), function(i){as.data.frame(dfa[1, ])}))),
              geometry = sf::st_sfc(lapply(1:length(la), function(i){
                sf::st_linestring(m1[la[[i]], ])})),
              crs = sf::st_crs(net))
  })
  # df <- data.table::rbindlist( dfl) # rbindlist changes geometries!
  df <- do.call("rbind", dfl)
  for(i in 1:length(namesnet)) {
    df[[i]] <- as.numeric(df[[i]])
  }

  df <- sf::st_sf(as.data.frame(df), geometry = df$geometry)


  df$LKM2 <- sf::st_length(df)
  df <- as.data.frame(df)
  df[, namesnet] <- df[, namesnet] * as.numeric(df$LKM2/df$LKM)
  df[, namesnet] <- df[, namesnet] * snetdf/sum(df[, namesnet], na.rm = TRUE)

  cat(paste0("Sum: ", round(sum(df[, namesnet], na.rm = T), 2), "\n"))

  df <- sf::st_sf(as.data.frame(df), geometry = df$geometry)

  sf::st_sf(as.data.frame(df), geometry = df$geometry)

}
