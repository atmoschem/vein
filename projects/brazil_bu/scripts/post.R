# grade
g                  <- st_transform(g, crs)

# streets  ####
cat("Aggregando emissões por rua...\n")
for(i in seq_along(pol)) {
  pols <- ifelse(pol[i] == "HC", "_HC", pol[i])
  x <- emis_merge(pol = pols, path = "emi",  net = net, k = units::set_units(1, "1/h"))
  saveRDS(x, paste0("post/streets/", 
                    ifelse(pols == "_HC", "HC", pols), ".rds"))
}

# grids ####
cat("Aggregando emissões em grade ...\n")
lf <- list.files(path = "post/streets", pattern = ".rds", full.names = TRUE)
na <- list.files(path = "post/streets", pattern = ".rds", full.names = F)
na <- gsub(pattern = ".rds", replacement = "", x = na)

for(i in seq_along(lf)) {
  x <- readRDS(lf[i])
  gx <- emis_grid(spobj = x, g = g)
  saveRDS(gx, paste0("post/grids/", na[i], ".rds"))
}

# datatable ####
cat("Agregando emissões em data.table...\n")
for(i in seq_along(pol)) {
  pols <- ifelse(pol[i] == "HC", "_HC", pol[i])
  x <- emis_merge(pols, what = 'DF.rds', FALSE)
  saveRDS(x, paste0("post/datatable/", 
                    ifelse(pols == "_HC", "HC", pols), ".rds"))
}


# Agregando emissões por categoria ####
cat("Agregando emissões por categoria ...\n")

dt <- data.table::rbindlist(
  lapply(seq_along(pol), function(i){
    pols <- ifelse(pol[i] == "HC", "_HC", pol[i])
    x <- emis_merge(pols, what = 'DF.rds', FALSE)
  })
)
dt$pollutant <- as.character(dt$pollutant)
dt$t <- units::set_units(dt$g, t)
saveRDS(dt, "post/datatable/emissions.rds")
data.table::fwrite(dt, "csv/emissions.csv", row.names = FALSE)


dt0 <- dt[, round(sum(t)*factor_emi, 2), by = .(pollutant)]
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

suppressWarnings(
rm("df1", "df2", "df3", "dt", "dt0", "dt1", "dt2", "dt3", "factor_emi", 
   "g", "gx", "i", "lf", "na", "net", "pol", "pols", "x", "crs")
)