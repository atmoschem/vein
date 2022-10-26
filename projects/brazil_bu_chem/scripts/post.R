# grade
g <- st_transform(g, crs)

# streets  ####
switch(language,
       "portuguese" = message("\nAgregando emissões por rua...\n"),
       "english" = message("\nAgregating emissions by street...\n"),
       "spanish" = message("\nAgregando emisiones por calle...\n")
)

x <- rbind(fread("emi/EXHAUST_STREETS.csv"),
           fread("emi/EVAP_STREETS.csv"),
           fread("emi/WEAR_STREETS.csv")
)

x[, pol := fifelse(pol == "PM", "PM2.5", pol)]
unique(x$pol)
na <- paste0("V", 1:nrow(tfs))

names(x)

nmhc <- c("NMHC", "Diurnal", "Hot Soak", "Running Losses")

# non NMHC
x[!pol %in% nmhc, 
  lapply(.SD, sum, na.rm = T),
  .SDcols = na,
  by = .(id, pol)] -> xx

uxxpol <- unique(xx$pol)

for (i in seq_along(uxxpol)) {
  print(uxxpol[i])
  
  dt <- xx[pol == uxxpol[i]]
  dt$id <- NULL
  dt$pol <- NULL
  
  dt <- st_sf(Emissions(dt, mass = "g", time = "h"), 
              geometry = st_geometry(net))
  saveRDS(dt, paste0(
    "post/streets/",uxxpol[i], ".rds"
  ))
}
# NMHC

na <- paste0("V", 1:nrow(tfs))
xnmhc <- x[pol %in% nmhc]
xnmhc[, pol := fifelse(
  pol %in% c("Diurnal", "Hot Soak", "Running Losses"),
  "EVAP",
  pol
)]

xnmhc[,
      lapply(.SD, sum, na.rm = T),
      .SDcols = na,
      by = .(id,paste0(fuel,"_", pol ))] -> xx

names(xx)[2] <- "pol"

uxxpol <- unique(xx$pol)

for (i in seq_along(uxxpol)) {
  print(uxxpol[i])
  
  dt <- xx[pol == uxxpol[i]]
  dt$id <- NULL
  dt$pol <- NULL
  dt <- st_sf(Emissions(dt, mass = "g", time = "h"), 
              geometry = st_geometry(net))
  saveRDS(dt, paste0(
    "post/streets/",uxxpol[i], ".rds"
  ))
}

# grids ####
switch(language,
       "portuguese" = message("\nAgregando emissões por grade\n"),
       "english" = message("\nAgregating emissions by grid...\n"),
       "spanish" = message("\nAgregando emisiones por grilla...\n")
)

lf <- list.files(path = "post/streets", pattern = ".rds", full.names = TRUE)
na <- list.files(path = "post/streets", pattern = ".rds", full.names = F)
na <- gsub(pattern = ".rds", replacement = "", x = na)

for (i in seq_along(lf)) {
  print(na[i])
  x <- readRDS(lf[i])
  gx <- emis_grid(spobj = x, g = g)
  saveRDS(gx, paste0("post/grids/", na[i], ".rds"))
}

# datatable ####
switch(language,
       "portuguese" = message("\nAgregando emissões em data.table\n"),
       "english" = message("\nAgregating emissions in data.table...\n"),
       "spanish" = message("\nAgregando emisiones en data.table...\n")
)


dt <- rbind(fread("emi/EXHAUST_DF.csv"),
           fread("emi/EVAP_DF.csv"),
           fread("emi/WEAR_DF.csv")
)
# Agregando emissões por categoria ####
switch(language,
       "portuguese" = message("\nAgregando emissões por categoria\n"),
       "english" = message("\nAggregating emissions by category...\n"),
       "spanish" = message("\nAgregando emisiones por categoria...\n")
)
dt$pollutant <- as.character(dt$pollutant)
dt$g <- units::set_units(dt$g, g)
dt$t <- units::set_units(dt$g, t)
saveRDS(dt, "post/datatable/emissions.rds")
data.table::fwrite(dt, "csv/emissions.csv", row.names = FALSE)


dt0 <- dt[, round(sum(t) * factor_emi, 2), by = .(pollutant)]
print(dt0)


# emissoes by veh
dt1 <- dt[, sum(t), by = .(pollutant, veh)]
df1 <- long_to_wide(df = dt1, column_with_new_names = "pollutant", column_with_data = "V1", column_fixed = "veh")
saveRDS(df1, "post/datatable/emissions_by_veh.rds")
data.table::fwrite(df1, "csv/emissions_by_veh.csv", row.names = FALSE)

# emissoes by fuel
dt2 <- dt[, sum(t), by = .(pollutant, fuel)]
df2 <- long_to_wide(df = dt2, column_with_new_names = "pollutant", column_with_data = "V1", column_fixed = "fuel")
saveRDS(df2, "post/datatable/emissions_by_fuel.rds")
data.table::fwrite(df2, "csv/emissions_by_fuel.csv", row.names = FALSE)

# emissoes by age
dt3 <- dt[, sum(t), by = .(pollutant, age)]
df3 <- long_to_wide(df = dt3, column_with_new_names = "pollutant", column_with_data = "V1", column_fixed = "age")
saveRDS(df3, "post/datatable/emissions_by_age.rds")
data.table::fwrite(df3, "csv/emissions_by_age.csv", row.names = FALSE)

switch(language,
       "portuguese" = message("\n\nArquivos em: /post/*:"),
       "english" = message("\nFiles in: /post/*"),
       "spanish" = message("\nArchivos en: /post/*")
)


switch(language,
       "portuguese" = message("Limpando..."),
       "english" = message("Cleaning..."),
       "spanish" = message("Limpiando...")
)

suppressWarnings(
  rm(
    "df1", "df2", "df3", "dt", "dt0", "dt1", "dt2", "dt3", "factor_emi",
    "g", "gx", "i", "lf", "na", "net", "pol", "pols", "x", "crs"
  )
)