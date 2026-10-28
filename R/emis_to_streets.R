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
#' @return sf with the same rows and order of `streets`, adding one column
#' for each column of `dfemis` (except `by`). Streets whose `by` is not
#' present in `dfemis` receive 0 emissions.
#' @importFrom sf st_geometry st_as_sf st_length st_set_geometry
#' @export
#' @seealso \code{\link{add_polid}}
#' @note When spobj is a 'Spatial' object (class of sp), they are converted
#'  into 'sf'.
#' @examples \dontrun{
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
  if(inherits(dfemis, "sf")) {
    dfemis <- sf::st_set_geometry(dfemis, NULL)
  }
  dfemis <- as.data.frame(dfemis)
  if (!by %in% names(dfemis)) stop("'dfemis' must have the column '", by, "'")
  streets <- sf::st_as_sf(streets)
  if (!by %in% names(streets)) stop("'streets' must have the column '", by, "'")
  geo <- sf::st_geometry(streets)
  dfa <- sf::st_set_geometry(streets, NULL)
  w <- as.numeric(sf::st_length(streets))

  # check stpro
  if(!missing(stpro)) {
    if(names(stpro)[1] != "stpro") stop("First name of data.frame stpro must be 'stpro'")
    if(names(stpro)[2] != "VAL") stop("Second name of data.frame stpro must be 'VAL'")
    if(!"stpro" %in% names(dfa)) stop("'streets' must have the column 'stpro'")
    val <- stpro$VAL[match(dfa$stpro, stpro$stpro)]
    val[is.na(val)] <- 1
    w <- w * val
  }

  # emissions columns, in the order of dfemis
  vars <- setdiff(names(dfemis), by)
  out <- matrix(0, nrow = nrow(dfa), ncol = length(vars))
  for (i in seq_len(nrow(dfemis))) {
    if(verbose)   message(paste0("filtering ", dfemis[[by]][i]))
    idx <- which(dfa[[by]] == dfemis[[by]][i])
    if (length(idx) == 0) {
      warning("There are no streets with ", by, " = ", dfemis[[by]][i])
      next
    }
    p <- w[idx] / sum(w[idx])
    vals <- vapply(vars, function(v) as.numeric(dfemis[[v]][i]), numeric(1))
    out[idx, ] <- out[idx, , drop = FALSE] + p %o% vals
  }
  nomatch <- !dfa[[by]] %in% dfemis[[by]]
  if (any(nomatch)) {
    warning(sum(nomatch), " streets have a '", by,
            "' not present in 'dfemis', their emissions are 0")
  }
  for (j in seq_along(vars)) {
    x <- out[, j]
    if (inherits(dfemis[[vars[j]]], "units")) units(x) <- units(dfemis[[vars[j]]])
    dfa[[vars[j]]] <- x
  }
  dfa <- sf::st_sf(dfa, geometry = geo)
  return(dfa)
}
