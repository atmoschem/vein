# NetCDF ####
# Reads the gridded emissions in `dir_grids` (sf polygons, columns V1..V24
# with units) and writes one CF NetCDF in `dir_nc`.
# Adds the variable NMHC = sum of all NMHC_* grids.
#
# Can be sourced from main.R after scripts/post.R:
#   language <- "spanish"
#   year <- 2024
#   source("scripts/nc.R", encoding = "UTF-8")

# defaults (override before source() if needed) ####
if (!exists("language")) language <- "spanish" # portuguese english spanish
if (!exists("year") || !is.numeric(get("year"))) year <- 2024
if (!exists("dir_grids")) dir_grids <- "post/grids"
if (!exists("dir_nc")) dir_nc <- "post/nc"
if (!exists("unit_out")) unit_out <- "g/(h*km^2)"
if (!exists("file_nc")) file_nc <- paste0(dir_nc, "/emissions_", year, ".nc")

suppressMessages({
  library(ncdf4)
  library(sf)
  library(units)
})

switch(
  language,
  "portuguese" = message("\nGrelhas em NetCDF...\n"),
  "english" = message("\nGrids to NetCDF...\n"),
  "spanish" = message("\nGrillas a NetCDF...\n")
)

dir.create(dir_nc, showWarnings = FALSE, recursive = TRUE)

lf <- list.files(path = dir_grids, pattern = "\\.rds$", full.names = TRUE)
na <- gsub("\\.rds$", "", basename(lf))

if (length(lf) == 0) {
  stop("No .rds grids found in ", dir_grids)
}

# grid geometry (lat/lon of the cell centers) from the first grid ####
x0 <- suppressWarnings(readRDS(lf[1]))
if (!inherits(x0, "sf")) {
  stop(basename(lf[1]), " is not an sf object")
}

ll <- st_coordinates(st_transform(st_centroid(st_geometry(x0)), 4326))
lon_v <- sort(unique(round(ll[, 1], 6)))
lat_v <- sort(unique(round(ll[, 2], 6)))
nlon <- length(lon_v)
nlat <- length(lat_v)
icol <- match(round(ll[, 1], 6), lon_v)
irow <- match(round(ll[, 2], 6), lat_v)

if (nrow(x0) != nlat * nlon) {
  stop(
    "The grid is not regular: ", nrow(x0), " cells != ", nlat, " x ", nlon
  )
}

# reshape data.frame [cell, hour] into array [hour, lat, lon] ####
# lat ascending (south to north), lon ascending (west to east)
to_array <- function(df) {
  ah <- array(0, dim = c(ncol(df), nlat, nlon))
  for (h in seq_len(ncol(df))) {
    m <- matrix(0, nlat, nlon)
    m[cbind(irow, icol)] <- df[[h]]
    ah[h, , ] <- m
  }
  ah
}

# read grids, check and convert units to `unit_out` ####
grids <- vector("list", length(lf))
names(grids) <- na
u_nc <- setNames(rep(unit_out, length(lf)), na)
nmhc <- NULL
nmhc_files <- character()
ntime <- NULL

for (i in seq_along(lf)) {
  x <- suppressWarnings(readRDS(lf[i]))

  if (!identical(st_geometry(x), st_geometry(x0))) {
    stop(basename(lf[i]), " has a different grid than ", basename(lf[1]))
  }

  df <- st_set_geometry(x, NULL)
  df$id <- NULL

  if (is.null(ntime)) {
    ntime <- ncol(df)
  } else if (ncol(df) != ntime) {
    stop(
      basename(lf[i]), " has ", ncol(df), " columns, expected ", ntime
    )
  }

  if (inherits(df[[1]], "units")) {
    # mode = "standard" is required to convert with a unit string in a variable
    # (`units` >= 1.0 defaults set_units() to "symbols" mode)
    df <- as.data.frame(lapply(df, function(z) {
      as.numeric(set_units(z, unit_out, mode = "standard"))
    }))
  } else {
    warning(na[i], ": columns have no units")
    u_nc[na[i]] <- ""
    df <- as.data.frame(lapply(df, as.numeric))
  }

  grids[[i]] <- to_array(df)

  if (grepl("^NMHC_", na[i])) {
    nmhc <- if (is.null(nmhc)) as.matrix(df) else nmhc + as.matrix(df)
    nmhc_files <- c(nmhc_files, na[i])
  }
}

# total NMHC = sum of all NMHC_* grids ####
if (!is.null(nmhc)) {
  grids[["NMHC"]] <- to_array(as.data.frame(nmhc))
  u_nc[["NMHC"]] <- unit_out
  switch(
    language,
    "portuguese" = message(
      "\nNMHC = ", paste(nmhc_files, collapse = " + "), "\n"
    ),
    "english" = message(
      "\nNMHC = ", paste(nmhc_files, collapse = " + "), "\n"
    ),
    "spanish" = message(
      "\nNMHC = ", paste(nmhc_files, collapse = " + "), "\n"
    )
  )
} else {
  warning("No NMHC_* grids found in ", dir_grids)
}

# long names ####
long_names <- c(
  "CO" = "carbon monoxide",
  "CO2" = "carbon dioxide",
  "CH4" = "methane",
  "FC" = "fuel consumption",
  "HC" = "hydrocarbons",
  "NO" = "nitric oxide",
  "NO2" = "nitrogen dioxide",
  "NOx" = "nitrogen oxides",
  "NMHC" = "total non-methane hydrocarbons",
  "NMHC_EXHAUST_G" = "non-methane hydrocarbons, exhaust, gasoline",
  "NMHC_EXHAUST_D" = "non-methane hydrocarbons, exhaust, diesel",
  "NMHC_EVAP_G" = "non-methane hydrocarbons, evaporative, gasoline",
  "PM2.5" = "particulate matter < 2.5 um",
  "PM10" = "particulate matter < 10 um",
  "SO2" = "sulfur dioxide"
)

lname <- function(n) {
  if (n %in% names(long_names)) long_names[[n]] else n
}

# dimensions ####
d_time <- ncdim_def(
  "time", "hour", seq_len(ntime),
  longname = "hour of day (local time)"
)
d_lat <- ncdim_def("lat", "degrees_north", lat_v, longname = "latitude")
d_lon <- ncdim_def("lon", "degrees_east", lon_v, longname = "longitude")

# variables ####
vars <- lapply(names(grids), function(n) {
  ncvar_def(
    name = n,
    units = u_nc[[n]],
    dim = list(d_time, d_lat, d_lon),
    missval = NA,
    longname = lname(n),
    prec = "float"
  )
})

nc <- nc_create(file_nc, vars)

for (n in names(grids)) {
  ncvar_put(nc, n, grids[[n]])
}

if (!is.null(nmhc)) {
  ncatt_put(
    nc, "NMHC", "comment",
    paste0("sum of ", paste(nmhc_files, collapse = " + ")),
    prec = "text"
  )
}

# global attributes ####
ncatt_put(nc, 0, "Conventions", "CF-1.8")
ncatt_put(
  nc, 0, "title",
  paste0("Gridded emissions inventory ", year, " (", basename(getwd()), ")")
)
ncatt_put(nc, 0, "source", "vein (https://github.com/atmoschem/vein)")
ncatt_put(
  nc, 0, "history",
  paste0(
    "Created ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " by scripts/nc.R"
  )
)
ncatt_put(
  nc, 0, "comment",
  paste(
    "Hourly-average emission fluxes by local hour of day;",
    "lat ascending (south to north), lon ascending (west to east)"
  )
)
ncatt_put(
  nc, 0, "crs",
  paste0(
    "input grids: ", st_crs(x0)$proj4string,
    "; coordinates: WGS84 lat/lon"
  )
)

nc_close(nc)

switch(
  language,
  "portuguese" = message("\nNetCDF: ", file_nc, "\n"),
  "english" = message("\nNetCDF: ", file_nc, "\n"),
  "spanish" = message("\nNetCDF: ", file_nc, "\n")
)

message(
  "dimensions: time = ", ntime,
  ", lat = ", nlat, ", lon = ", nlon, "\n"
)
message(
  "variables: ",
  paste0(names(grids), " [", u_nc[names(grids)], "]", collapse = ", "),
  "\n"
)

suppressWarnings(
  rm(
    "d_lat", "d_lon", "d_time", "df", "h", "i", "icol", "irow", "lat_v",
    "lf", "ll", "lname", "lon_v", "long_names", "m", "na", "nmhc",
    "nmhc_files", "nlat", "nlon", "ntime", "to_array", "u_nc", "vars",
    "x", "x0", "nc"
  )
)
