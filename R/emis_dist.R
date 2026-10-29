#' Allocate emissions into spatial objects (street emis to grid)
#'
#' @description \code{\link{emis_dist}} allocates emissions proportionally to
#' each feature. "Spatial" objects are converter to "sf" objects. Currently,
#' 'LINESTRING' or 'MULTILINESTRING' supported. The emissions are distributed
#' in each street.
#'
#' @param gy Numeric; a unique total (top-down)
#' @param spobj A spatial dataframe of class "sp" or "sf". When class is "sp"
#' it is transformed to "sf".
#' @param pro Matrix or data-frame profiles, for instance, pc_profile.
#' @param osm Numeric; vector of length 5, for instance, c(5, 3, 2, 1, 1).
#' The first element covers 'motorway' and 'motorway_link.
#' The second element covers 'trunk' and 'trunk_link'.
#' The third element covers 'primary' and 'primary_link'.
#' The fourth element covers 'secondary' and 'secondary_link'.
#' The fifth element covers 'tertiary' and 'tertiary_link'.
#' Streets of other types are not used. When there are no streets for one
#' category, `osm` is normalized with the categories present, so that the sum
#' of the emissions is always `gy`.
#' @param verbose Logical; to show more info.
#' @importFrom sf st_sf st_as_sf  st_length st_geometry_type st_set_geometry
#' @importFrom units as_units
#' @export
#' @note When spobj is a 'Spatial' object (class of sp), they are converted
#'  into 'sf'.
#' @examples \dontrun{
#' data(net)
#' data(pc_profile)
#' po <- 1000
#' t1 <- emis_dist(gy = po, spobj = net)
#' head(t1)
#' sum(t1$gy)
#' #t1 <- emis_dist(gy = po, spobj = net, osm = c(5, 3, 2, 1, 1) )
#' t1 <- emis_dist(gy = po, spobj = net, pro = pc_profile)
#' }
emis_dist <- function(gy,
                      spobj,
                      pro,
                      osm,
                      verbose = FALSE) {
  net <- sf::st_as_sf(spobj)
  if (any(
    !unique(as.character(
      sf::st_geometry_type(net))) %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("Currently, geometries supported are 'LINESTRING' or 'MULTILINESTRING'")
  }

  net$lkm1 <- as.numeric(sf::st_length(net))
  geo <- suppressMessages(suppressWarnings(sf::st_geometry(net)))
  lkm <- net$lkm1 / sum(net$lkm1)
  e_street <- lkm * as.numeric(gy)

  # PROFILE SECTION
  if (missing(pro) & missing(osm)) {
    if (verbose) message("Selecting column geometry")
    net <- sf::st_sf(emission = e_street, geometry = geo)
    if (verbose) cat("Columns:", names(net), "\n")
    return(net)
  }

  if (!missing(pro) & missing(osm)) {
    pro <- pro / sum(pro)
    df <- as.data.frame(as.matrix(e_street) %*% matrix(unlist(pro), nrow = 1))
    net <- sf::st_sf(df, geometry = geo)
    if (verbose) {
      cat(paste0("Columns: ", names(net), "\n"))
    }

    return(net)
  }

  # osm
  if (!"highway" %in% names(net)) stop("Need OpenStreetMap network with colum highway")
  if (verbose) message("Selecting column highway")
  st <-  c("motorway", "motorway_link",
    "trunk", "trunk_link",
    "primary", "primary_link",
    "secondary", "secondary_link",
    "tertiary", "tertiary_link")
  if (verbose) cat("Selecting:", st, "\n")
  if (length(osm) != 5) stop("length of osm must be 5")
  net <- net[net$highway %in% st, ]
  # category of each street: 1 motorway, 2 trunk, 3 primary, 4 secondary, 5 tertiary
  cat_st <- ceiling(match(net$highway, st) / 2)
  present <- sort(unique(cat_st))
  if (length(present) == 0) stop("There are no streets of the types: ", paste(st, collapse = ", "))
  if (any(osm[-present] > 0)) {
    warning("There are no streets for the 'osm' categories ",
      paste(c("motorway", "trunk", "primary", "secondary", "tertiary")[setdiff(which(osm > 0), present)],
        collapse = ", "),
      "; 'osm' is normalized with the categories present")
  }
  osm <- osm[present] / sum(osm[present])
  net$gy <- 0
  for (i in seq_along(present)) {
    idx <- cat_st == present[i]
    net$gy[idx] <- net$lkm1[idx] / sum(net$lkm1[idx]) * as.numeric(gy) * osm[i]
  }
  # same order as before: motorway, trunk, primary, secondary, tertiary
  net <- net[order(cat_st), ]

  if (missing(pro)) {
    net_all <- sf::st_sf(emission = net$gy,
      highway = net$highway,
      geometry = sf::st_geometry(net))
    if (verbose) cat("Columns:", names(net_all), "\n")
    return(net_all)
  } else {
    pro <- pro / sum(pro)
    df <- as.data.frame(as.matrix(net$gy) %*% matrix(unlist(pro), nrow = 1))
    df$highway <- net$highway
    net_all <- sf::st_sf(df, geometry = sf::st_geometry(net))
    if (verbose) {
      cat("Columns: ", names(net_all), "\n")
    }
    return(net_all)
  }
}
