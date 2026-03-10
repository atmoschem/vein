year_selected <- as.numeric(substr(
  x = getwd(),
  start = nchar(getwd()) - 3,
  stop = nchar(getwd())
))

gs <- paste0(
  "post/grids/",
  "emis_grid_",
  sprintf("%02d", 1:12),

  ".rds"
)

voc <- paste0("post/spec_grid/", "emis_voc_", sprintf("%02d", 1:12), ".rds")

# target units molecules cm-2 s-1 ####
for (kk in seq_along(gs)) {
  # gas
  x <- readRDS(gs[kk])

  for (i in seq_along(pol)) {
    # mol/km^2/h
    x[[pol[i]]] <- x[[pol[i]]] /
      mw[i] /
      units::set_units(dmonth(year_selected, kk) / 24, "h")
  }

  # voc
  y <- readRDS(voc[kk])

  ny <- names(y)[2:ncol(y)]

  for (i in seq_along(ny)) {
    # mol/km^2/s
    y[[ny[i]]] <- y[[ny[i]]] /
      units::set_units(dmonth(year_selected, kk) / 24 / 3600, "s")
    # molec/km^2/s
    y[[ny[i]]] <- y[[ny[i]]] * units::set_units(6.022e23, "molec/mol")

    # molec / cm^2 / s
    y[[ny[i]]] <- units::set_units(y[[ny[i]]], "molec/cm^2/s")
  }

  # bring y to grid by id

  g <- x[, c("id", df_gas$gas)]

  gx <- merge(g, y, by = "id", all.x = T)

  pols <- c(df_gas$gas, ny)

  for (i in seq_along(pols)) {
    gx[[pols[i]]][is.na(gx[[pols[i]]])] <- 0
  }

  gx <- st_transform(gx, 4326)

  print(paste0(
    "post/spec_grid/",
    "emis_cams_",
    sprintf("%02d", kk),
    "_molec_cm2_s.rds"
  ))
  saveRDS(
    gx,
    paste0(
      "post/spec_grid/",
      "emis_cams_",
      sprintf("%02d", kk),
      "_molec_cm2_s.rds"
    )
  )
}
