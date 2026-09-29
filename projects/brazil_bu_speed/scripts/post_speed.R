# Post-processing of the speed inventory --------------------------------------
# Grids the hourly street emissions and saves totals.  Requires: language, net,
# pol, crs, g.

g <- sf::st_transform(g, crs)

switch(
  language,
  "portuguese" = message("\nAgregando emissões por rua e grade...\n"),
  "english" = message("\nAggregating emissions by street and grid...\n"),
  "spanish" = message("\nAgregando emisiones por calle y grilla...\n")
)

resumen <- data.table::data.table()

for (pj in pol) {
  x <- readRDS(paste0("emi/EXHAUST_SPEED_", pj, ".rds"))
  df <- as.data.frame(x)
  names(df) <- paste0("h", seq_len(ncol(df)))
  df[is.na(df)] <- 0

  xn <- sf::st_sf(
    Emissions(df, mass = "g", time = "h"),
    geometry = sf::st_geometry(net)
  )
  saveRDS(xn, paste0("post/streets/", pj, ".rds"))

  gx <- emis_grid(spobj = xn, g = g)
  saveRDS(gx, paste0("post/grids/", pj, ".rds"))

  resumen <- rbind(
    resumen,
    data.table::data.table(
      pollutant = pj,
      g_week = sum(x, na.rm = TRUE),
      g_day = sum(x, na.rm = TRUE) / 7
    )
  )
  rm(x, df, xn, gx)
  invisible(gc())
}

data.table::fwrite(resumen, "csv/emissions_speed_by_pol.csv")
print(resumen)

switch(
  language,
  "portuguese" = message("\nArquivos em: post/streets, post/grids e csv/\n"),
  "english" = message("\nFiles in: post/streets, post/grids and csv/\n"),
  "spanish" = message("\nArchivos en: post/streets, post/grids y csv/\n")
)
