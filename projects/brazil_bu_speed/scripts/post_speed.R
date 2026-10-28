# Post-processing of the speed inventory --------------------------------------
# Mirrors scripts/post.R (brazil_bu_chem) for the speed inventory:
# - exhaust from scripts/exhaust_speed.R (hourly, H hours, a week),
# - evaporatives and wear from scripts/evaporatives.R and scripts/wear.R
#   (hours of `tfs`, a day, repeated to cover the H hours).
# Writes post/streets/<pol>.rds and post/grids/<pol>.rds with hourly columns
# V1..VH ([g/h]), NMHC split by fuel as <fuel>_NMHC (exhaust) and <fuel>_EVAP
# (evaporatives), PM2.5 = exhaust PM + wear PM2.5, PM10 = exhaust PM + wear
# PM10, and post/datatable/emissions.rds in g per day (scripts/plots.R).
# Requires: language, net, crs, g, tfs, factor_emi.

g <- sf::st_transform(g, crs)

switch(
  language,
  "portuguese" = message("\nAgregando emissões por rua e grade...\n"),
  "english" = message("\nAggregating emissions by street and grid...\n"),
  "spanish" = message("\nAgregando emisiones por calle y grilla...\n")
)

# outputs of this step are rewritten from scratch
for (d in c("post/streets", "post/grids", "post/datatable")) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  suppressWarnings(file.remove(list.files(d, pattern = "\\.rds$", full.names = TRUE)))
}
dir.create("csv", showWarnings = FALSE)

# streets ####
ex <- readRDS("emi/EXHAUST_SPEED_BY_FUEL.rds") # "<fuel>_<pol>" -> S x H
H <- ncol(ex[[1]])
S <- nrow(net)
ndays <- H / 24
ex_pol <- sub("^[^_]*_", "", names(ex))
ex_fuel <- sub("_.*$", "", names(ex))

# evaporatives and wear: sum by street over the hours of `tfs` and repeat that
# day to cover the H hours of the exhaust
na <- paste0("V", 1:nrow(tfs))
to_week <- function(d) {
  d <- d[, lapply(.SD, sum, na.rm = TRUE), .SDcols = na, keyby = id]
  m <- matrix(0, nrow = S, ncol = length(na))
  m[d$id, ] <- as.matrix(d[, na, with = FALSE])
  m[, rep_len(seq_len(ncol(m)), H), drop = FALSE]
}
ev <- fread("emi/EVAP_STREETS.csv")
we <- fread("emi/WEAR_STREETS.csv")

out <- list()

# exhaust, all fuels together (NMHC is split by fuel below)
for (pj in setdiff(unique(ex_pol), "NMHC")) {
  out[[pj]] <- Reduce(`+`, ex[ex_pol == pj])
}
# NMHC by fuel: exhaust and evaporatives
for (f in unique(ex_fuel[ex_pol == "NMHC"])) {
  out[[paste0(f, "_NMHC")]] <- ex[[paste0(f, "_NMHC")]]
}
for (f in unique(ev$fuel)) {
  out[[paste0(f, "_EVAP")]] <- to_week(ev[fuel == f])
}
# PM: exhaust PM is fine (PM2.5) and also part of PM10
ex_pm <- if (is.null(out[["PM"]])) matrix(0, nrow = S, ncol = H) else out[["PM"]]
out[["PM"]] <- NULL
out[["PM2.5"]] <- ex_pm + to_week(we[pol == "PM2.5"])
out[["PM10"]] <- ex_pm + to_week(we[pol == "PM10"])

resumen <- data.table::data.table()

for (pj in names(out)) {
  print(pj)
  df <- as.data.frame(out[[pj]])
  names(df) <- paste0("V", seq_len(ncol(df)))
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
      g_week = sum(out[[pj]], na.rm = TRUE) * 7 / ndays,
      g_day = sum(out[[pj]], na.rm = TRUE) / ndays
    )
  )
  rm(df, xn, gx)
  invisible(gc())
}

data.table::fwrite(resumen, "csv/emissions_speed_by_pol.csv")
print(resumen)

# datatable (g per day) ####
switch(
  language,
  "portuguese" = message("\nAgregando emissões em data.table\n"),
  "english" = message("\nAgregating emissions in data.table...\n"),
  "spanish" = message("\nAgregando emisiones en data.table...\n")
)

# exhaust: H hours -> mean day, hour of the day
dt_ex <- fread("emi/EXHAUST_SPEED_DF.csv")
dt_ex <- dt_ex[,
  .(g = sum(g, na.rm = TRUE) / ndays),
  by = .(veh, size, fuel, type_emi, pollutant, age, hour = (hour - 1) %% 24 + 1)
]
dt <- rbind(
  dt_ex,
  fread("emi/EVAP_DF.csv"),
  fread("emi/WEAR_DF.csv"),
  use.names = TRUE,
  fill = TRUE
)
# same PM naming as the streets
dt <- rbind(dt, dt_ex[pollutant == "PM"][, pollutant := "PM10"], use.names = TRUE, fill = TRUE)
rm(dt_ex)
dt[, pollutant := fifelse(pollutant == "PM", "PM2.5", pollutant)]

dt$pollutant <- as.character(dt$pollutant)
dt$g <- units::set_units(dt$g, g)
dt$t <- units::set_units(dt$g, t)
saveRDS(dt, "post/datatable/emissions.rds")
data.table::fwrite(dt, "csv/emissions.csv", row.names = FALSE)

dt0 <- dt[, round(sum(t) * factor_emi, 2), by = .(pollutant)]
print(dt0)

# emissoes by veh
dt1 <- dt[, sum(t), by = .(pollutant, veh)]
df1 <- dcast.data.table(data = dt1, formula = pollutant ~ veh)
saveRDS(df1, "post/datatable/emissions_by_veh.rds")
data.table::fwrite(df1, "csv/emissions_by_veh.csv", row.names = FALSE)

# emissoes by fuel
dt2 <- dt[, sum(t), by = .(pollutant, fuel)]
df2 <- dcast.data.table(data = dt2, formula = pollutant ~ fuel)
saveRDS(df2, "post/datatable/emissions_by_fuel.rds")
data.table::fwrite(df2, "csv/emissions_by_fuel.csv", row.names = FALSE)

# emissoes by age
dt3 <- dt[, sum(t), by = .(pollutant, age)]
df3 <- dcast.data.table(data = dt3, formula = pollutant ~ age)
saveRDS(df3, "post/datatable/emissions_by_age.rds")
data.table::fwrite(df3, "csv/emissions_by_age.csv", row.names = FALSE)

switch(
  language,
  "portuguese" = message("\nArquivos em: post/streets, post/grids, post/datatable e csv/\n"),
  "english" = message("\nFiles in: post/streets, post/grids, post/datatable and csv/\n"),
  "spanish" = message("\nArchivos en: post/streets, post/grids, post/datatable y csv/\n")
)
