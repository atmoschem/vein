# Replacement for vein::emis_merge(), which was deleted in vein 1.2.0
# (NEWS.md: "Deleted emis_merge, now all projects store emissions in
# databases"). It reproduces the old behaviour used by this project:
# - files: every file under `path` (recursive) whose name matches `what` and
#   contains paste0(pol, "_") (emis_merge default under = "after"); or the
#   files given explicitly in `files`;
# - streets = TRUE: the street tables written by emis_post(by = "streets") are
#   summed row by row (row = street) and returned as an sf with the geometry of
#   `net`: an `id` column followed by the hourly columns in g/h, times k;
# - streets = FALSE: the data.frames written by emis_post(by = "veh") are
#   bound and column g is multiplied by k.
# Differences from the old function: k is a plain number (the old code needed
# k = units::set_units(1, "1/h") to keep units) and `id` is not multiplied by k.
merge_emis <- function(pol,
                       what = "STREETS.rds",
                       streets = TRUE,
                       net,
                       path = "emi",
                       k = 1,
                       files,
                       verbose = TRUE) {
  if (missing(files)) {
    files <- list.files(
      path = path,
      pattern = what,
      all.files = TRUE,
      full.names = TRUE,
      recursive = TRUE
    )
    files <- files[grep(pattern = paste0(pol, "_"), x = files)]
  }
  if (length(files) == 0) {
    stop("No emission files for '", pol, "' matching '", what, "' in ", path)
  }
  if (verbose) {
    cat("\nReading emissions from:\n")
    print(files)
  }
  x <- lapply(files, readRDS)

  if (streets) {
    x <- lapply(x, function(d) {
      if (inherits(d, "sf")) d <- sf::st_drop_geometry(d)
      d <- as.data.frame(d)
      d$id <- NULL
      d[] <- lapply(d, as.numeric)
      d$id <- seq_len(nrow(d))
      d
    })
    nombres <- setdiff(names(x[[1]]), "id")
    x <- data.table::rbindlist(x, use.names = TRUE)
    x <- x[, lapply(.SD, sum, na.rm = TRUE), by = "id", .SDcols = nombres]
    if (nrow(x) != nrow(net)) {
      stop("Number of rows of net must be equal to number of rows of estimates")
    }
    x <- as.data.frame(x)
    for (i in nombres) x[[i]] <- vein::Emissions(x[[i]] * k, time = "h")
    sf::st_sf(x, geometry = sf::st_geometry(sf::st_as_sf(net)))
  } else {
    x <- as.data.frame(data.table::rbindlist(x, use.names = TRUE))
    x$g <- x$g * k
    x
  }
}
