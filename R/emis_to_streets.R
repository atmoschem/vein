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
#' @param stpro data.frame with two columns, category of streets and value.
#' The name of the first column must be "stpro" and the sf streets must also
#' have a column with the nam "stpro" indicating the category of streets.
#' The second column must have the name "VAL" indicating the associated values
#' to each category of street
#' @param verbose Logical; to show more info.
#' @importFrom sf st_geometry st_as_sf st_length
#' @export
#' @seealso \code{\link{add_polid}}
#' @note When spobj is a 'Spatial' object (class of sp), they are converted
#'  into 'sf'.
#' @examples {
#' data(net)
#' stpro = data.frame(stpro = as.character(unique(net$tstreet)),
#'                    VAL = 1:9)
#' dnet <- net["ldv"]
#' dnet$stpro <- as.character(net$tstreet)
#' dnet$ID <- "A"
#' df2 <- data.frame(BC = 10, CO = 20, ID = "A")
#' ste <- emis_to_streets(streets = dnet, dfemis = df2)
#' sum(ste$ldv)
#' sum(net$ldv)
#' sum(ste$BC)
#' sum(df2$BC)
#' ste2 <- emis_to_streets(streets = dnet, dfemis = df2, stpro = stpro)
#' sum(ste2$ldv)
#' sum(net$ldv)
#' sum(ste2$BC)
#' sum(df2$BC)
#' }
emis_to_streets <- function(streets,
                             dfemis,
                             by = "ID",
                             stpro,
                             verbose = TRUE){
  outersect <- function(x, y) {
    sort(c(setdiff(x, y),
           setdiff(y, x)))
  }
  rn <- row.names(streets)
  streets <- sf::st_as_sf(streets)
  nstreets <- names(sf::st_set_geometry(streets, NULL))
  geo <- sf::st_geometry(streets)
  streets$length <- sf::st_length(streets)
  streets <- sf::st_set_geometry(streets, NULL)

  # check stpro
  if(!missing(stpro)) {
    if(names(stpro)[1] != "stpro") stop("First name of data.frame stpro must be 'stpro'")
    if(names(stpro)[2] != "VAL") stop("Second name of data.frame stpro must be 'VAL'")
    streets <- merge(streets, stpro, by = "stpro", all.x = T)
    streets$VAL <- ifelse(is.na(streets$VAL), 1, streets$VAL)
    dfa <- do.call("rbind",lapply(1:nrow(dfemis), function(i){
      if(verbose)   message(paste0("filtering ", dfemis[[by]][i]))
      dfstreets <- streets[streets[[by]] == dfemis[[by]][i], ]
      dfstreets$length <- dfstreets$length*dfstreets$VAL
      dfstreets$p_length <- as.numeric(dfstreets$length)/sum(as.numeric(dfstreets$length))
      dfs <- unlist(dfemis[dfemis[[by]] == dfemis[[by]][i], 1:(ncol(dfemis) - 1)])
      dft <- as.matrix(dfstreets$p_length)  %*%  matrix(as.numeric(dfs), nrow = 1)
      a <- as.data.frame(cbind(dfstreets, dft))
      a
    }))
    dfa$VAL <- NULL
  } else {

    dfa <- do.call("rbind",lapply(1:nrow(dfemis), function(i){
      if(verbose)   message(paste0("filtering ", dfemis[[by]][i]))
      dfstreets <- streets[streets[[by]] == dfemis[[by]][i], ]
      dfstreets$p_length <- as.numeric(dfstreets$length)/sum(as.numeric(dfstreets$length))
      dfs <- unlist(dfemis[dfemis[[by]] == dfemis[[by]][i], 1:(ncol(dfemis) - 1)])
      dft <- as.matrix(dfstreets$p_length)  %*%  matrix(as.numeric(dfs), nrow = 1)
      a <- as.data.frame(cbind(dfstreets, dft))
      a
    }))
  }
  dfa$length <- NULL
  dfa$p_length <- NULL
  ndfemis <- outersect(names(dfemis), by)
  ldfa <- length(names(dfa))
  lndfemis <- length(ndfemis)
   ll <- ldfa - lndfemis
  names(dfa) <- c(names(dfa)[1:ll], ndfemis)
  dfa <- sf::st_sf(dfa, geometry = geo)
  return(dfa)
}

