#' Emis to streets distribute top-down emissions into streets
#'
#' @description \code{\link{emis_to_streets}} allocates emissions proportionally to
#' each feature. "Spatial" objects are converter to "sf" objects. Currently,
#' 'LINESTRING' or 'MULTILINESTRING' supported. The emissions are distributed
#' in each street.
#'
#' @param streets sf object with geometry 'LINESTRING' or 'MULTILINESTRING'. Or
#' SpatialLinesDataFrame
#' @param dfemis data.frame with emissions
#' @param by Character indicating the columns that must be present in both
#' 'street' and 'dfemis'
#' @param verbose Logical; to show more info.
#' @importFrom sf st_geometry st_as_sf st_length
#' @export
#' @seealso \code{\link{add_polid}}
#' @note When spobj is a 'Spatial' object (class of sp), they are converted
#'  into 'sf'.
#' @examples \dontrun{
#' # TO DO
#' }
emis_to_streets <- function(streets,
                            dfemis,
                            by = "ID",
                            verbose = TRUE){
  streets <- sf::st_as_sf(streets)
  streets$length <- sf::st_length(streets)
  dfa <- do.call("rbind",lapply(1:nrow(dfemis), function(i){
if(verbose)   message(paste0("filtering ", dfemis[[by]][i]))
    dfstreets <- streets[streets[[by]] == dfemis[[by]][i], ]

    dfstreets$p_length <- as.numeric(dfstreets$length)/sum(as.numeric(dfstreets$length))


    dfs <- unlist(dfemis[dfemis[[by]] == dfemis[[by]][i], 1:(ncol(dfemis) - 1)])

    dft <- as.matrix(dfstreets$p_length)  %*%  matrix(as.numeric(dfs), nrow = 1)
    cbind(dfstreets, dft)
  }))
  dfa$length <- NULL
  dfa$p_length <- NULL
  names(dfa) <- c(names(streets)[1:2],
                  names(dfemis)[!names(dfemis) %in% by],
                  "geometry")
  sf::st_geometry(dfa) <- "geometry"
  return(dfa)
}
