met <- readRDS("config/met.rds")
names(met)

year <- met$Year[1]
month <- met$Month[1]
date <- as.Date(ISOdate(year, month, 1, 0, 0))

numberOfDays <- function(date) {
  m <- format(date, format = "%m")
  
  while (format(date, format = "%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format = "%d")))
}

factor_emi <- numberOfDays(date = date) # daily to month

# name_hours ####
ntfs <- paste0("H", 1:nrow(tfs))

# electric ####
f <- list.files(path = "emi", pattern = "ELEC", full.names = T, recursive = T)
f <- grep(pattern = "STREETS", x = f, value = T)
df_elec <- rbindlist(lapply(f, fread))
names(df_elec)

df_elec[, 
        lapply(.SD, sum, na.rm = T),
        .SDcols = ntfs,
        by = .(id, pollutantID)] -> emi_elec

lemi_elec <- split(emi_elec, emi_elec$pollutantID)
names(lemi_elec)

# pollutants  ####
switch(language,
       "portuguese" = message("\nDetectando poluentes:\n"),
       "english" = message("\nDetecting pollutants:\n"),
       "spanish" = message("\nDetectando contaminantes\n")
)

f <- list.files(path = "emi", pattern = "STREETS", full.names = T, recursive = T)

ff <- gsub(pattern = "emi/emissions_rate_per_distance/STREETS_pollutantID_", 
           replacement = "",
           f)
ff <- gsub(pattern = "emi/emissions_rate_per_vehicle/STREETS_pollutantID_", 
           replacement = "",
           ff)
ff <- gsub(pattern = ".csv.gz", 
           replacement = "",
           ff)
fff <- unique(suppressWarnings(as.numeric(ff)))
fff <- fff[!is.na(fff)]
fff <- fff[order(fff)]

dfpol <- decoder$pollutants
setDT(dfpol)

print(dfpol[ID %in% fff])


# streets  ####
switch(language,
       "portuguese" = message("\nAgregando emissões por rua...\n"),
       "english" = message("\nAgregating emissions by street...\n"),
       "spanish" = message("\nAgregando emisiones por calle...\n")
)
f <- list.files(path = "emi", pattern = "STREETS", full.names = T, recursive = T)

# emissions differnet than break and tire wear
for(i in seq_along(fff)) {
  if(fff[i] %in% c(106, 107, 116, 117)) next
  cat("Reading pollutantID", fff[i], "\n")
  df <- rbindlist(lapply(grep(pattern = paste0("pollutantID_", fff[i]), x = f, value = T), fread))
  df[, 
     lapply(.SD, sum, na.rm = T),
     .SDcols = ntfs,
     by = .(id)] -> dfx
  
  for(j in seq_along(ntfs))  dfx[[ntfs[j]]] <- Emissions(dfx[[ntfs[j]]], mass = "g", time = "h")

  dfx <- st_sf(dfx, geometry = st_geometry(net))
  saveRDS(object = dfx, paste0("post/streets/pollutantID_", fff[i], ".rds"))
}

# break and tire wear
nl <-names(lemi_elec)
for(i in seq_along(nl)) {
  dx <- lemi_elec[[nl[i]]]
  dx[, pollutantID := NULL]
  df <- rbindlist(lapply(grep(pattern = paste0("pollutantID_", nl[i]), x = f, value = T), fread))
  
  df <- rbind(dx, df)
  df[, 
     lapply(.SD, sum, na.rm = T),
     .SDcols = ntfs,
     by = .(id)] -> dfx
  
  for(j in seq_along(ntfs))  dfx[[ntfs[j]]] <- Emissions(dfx[[ntfs[j]]], mass = "g", time = "h")
  
  dfx <- st_sf(dfx, geometry = st_geometry(net))
  
  saveRDS(object = dfx, paste0("post/streets/pollutantID_", nl[i], ".rds"))
}

# nmhc
nmhcs <- grep(pattern = "NMHC", x = f, value = T)
nmhc1 <- fread(nmhcs[1])
nmhc2 <- fread(nmhcs[2], fill = T) # problem with emissions_per_distance_NMHC

df <- rbind(nmhc1, nmhc2)

df[, 
   lapply(.SD, sum, na.rm = T),
   .SDcols = ntfs,
   by = .(id, family, fuel, process2)] -> dfx

for(j in seq_along(ntfs))  dfx[[ntfs[j]]] <- Emissions(dfx[[ntfs[j]]], mass = "g", time = "h")

dfx <- st_sf(dfx, geometry = st_geometry(net))

saveRDS(object = dfx, "post/streets/NMHC.rds")

# grids ####
switch(language,
       "portuguese" = message("\nAgregando emissões por grade\n"),
       "english" = message("\nAgregating emissions by grid...\n"),
       "spanish" = message("\nAgregando emisiones por grilla...\n")
)

g <- make_grid(spobj = dfx, width = grid_width, crs = crs)

lf <- list.files(path = "post/streets", pattern = ".rds", full.names = TRUE)
na <- list.files(path = "post/streets", pattern = ".rds", full.names = F)
na <- gsub(pattern = ".rds", replacement = "", x = na)

for (i in seq_along(lf)) {
  switch(language,
         "portuguese" = message("\nGerando grade de  ", lf[i], "\n"),
         "english" = message("\nGridding emissions of ", lf[i], "\n"),
         "spanish" = message("\nGenerando grilla de ", lf[i], "\n")
  )
  x <- readRDS(lf[i])
  gx <- emis_grid(spobj = x, g = g, sr = crs)
  saveRDS(gx, paste0("post/grids/", na[i], ".rds"))
}

# datatable ####
switch(language,
       "portuguese" = message("\nAgregando emissões em data.table\n"),
       "english" = message("\nAgregating emissions in data.table...\n"),
       "spanish" = message("\nAgregando emisiones en data.table...\n")
)
f <- list.files(path = "emi", pattern = "DF", full.names = T, recursive = T)
f1 <- fread(f[1])
f2 <- fread(f[2])
f3 <- fread(f[3])
f2$fuelTypeID <- 9
f2$fuel <- "ELEC"
names(f1)
names(f2)
names(f3)

dt <- rbind(f1[,names(f3), with = F],
            f2[,names(f3), with = F], 
            f3)
names(dt)
dt$g <- units::set_units(dt$value, g)
dt$t <- units::set_units(dt$g, t)
data("decoder")

# pollutant
dfpol <- decoder$pollutants
names(dfpol)[1] <- "pollutantID"
dt <- merge(dt, dfpol, by = "pollutantID", all.x = T)

# process
dfpro <- decoder$emission_process
names(dfpro) <- c("processID", "process")
dt <- merge(dt, dfpro, by = "processID", all.x = T)

# vehicle
dfv <- metadata[, c("family", "vehicles")]
names(dfv)[2] <- "veh"
dt <- merge(dt, dfv, by = "veh", all.x = T)

saveRDS(dt, "post/datatable/emissions.rds")

dt0 <- dt[, round(sum(t), 2), by = .(pollutant)]
print(dt0)

# emissoes by veh
dt1 <- dt[, sum(t), by = .(pollutant, veh)]
df1 <- long_to_wide(df = dt1, 
                    column_with_new_names = "pollutant", 
                    column_with_data = "V1", 
                    column_fixed = "veh")
saveRDS(df1, "post/datatable/emissions_by_veh.rds")
data.table::fwrite(df1, "csv/emissions_by_veh.csv", row.names = FALSE)

# emissoes by fuel
dt2 <- dt[, sum(t), by = .(pollutant, fuel)]
df2 <- long_to_wide(df = dt2, 
                    column_with_new_names = "pollutant", 
                    column_with_data = "V1", 
                    column_fixed = "fuel")
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