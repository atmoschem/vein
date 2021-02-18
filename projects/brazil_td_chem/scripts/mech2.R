dir.create("post/spec_grid", showWarnings = FALSE)

fs <- list.files(
  path = "post/spec_grid",
  pattern = ".rds",
  full.names = TRUE,
  recursive = TRUE
)
file.remove(fs)

# Gasoline Exhaust
gx <- st_set_geometry(readRDS("post/emi_grid.rds"), NULL)
id <- gx$id

nx <- paste0("NMHC_G_EXH_", months_subset)
x <- gx[nx]
x[is.na(x)] <- 0
x$id <- NULL
dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "E25",
  veh = "LDV",
  eu = "Exhaust"
)

dx$id <- rep(id, length(unique(dx$pol)))


vocE25EX <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx
)


# Gasoline Evap
nx <- paste0("NMHC_G_EVA_", months_subset)
x <- gx[nx]
x$id <- NULL
x[is.na(x)] <- 0

dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "E25",
  veh = "LDV",
  eu = "Evaporative"
)

dx$id <- rep(id, length(unique(dx$pol)))


vocE25EV <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)

# Etanol Exhaust
nx <- paste0("NMHC_E_EXH_", months_subset)
x <- gx[nx]
x[is.na(x)] <- 0
x$id <- NULL
dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "E100",
  veh = "LDV",
  eu = "Exhaust"
)

dx$id <- rep(id, length(unique(dx$pol)))


vocE100EX <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)

# Etanol Evap
nx <- paste0("NMHC_E_EVA_", months_subset)
x <- gx[nx]
x[is.na(x)] <- 0
x$id <- NULL
dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "E100",
  veh = "LDV",
  eu = "Evaporative"
)

dx$id <- rep(id, length(unique(dx$pol)))

vocE100EV <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx
)

# Diesel Exhaust
nx <- paste0("NMHC_D_EXH_", months_subset)
x <- gx[nx]
x[is.na(x)] <- 0
x$id <- NULL
dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "D",
  veh = "HDV",
  eu = "all"
)

dx$id <- rep(id, length(unique(dx$pol)))


vocB5EX <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)

voc <- rbind(vocB5EX,
  vocE100EV,
  vocE100EX,
  vocE25EV,
  vocE25EX,
  use.names = FALSE
)

dfvoc <- voc[,
  lapply(.SD, sum, na.rm = T),
  .SDcols = nx,
  by = .(id, group)
]

voc <- split(dfvoc, dfvoc$group)

names(voc) <- paste0("E_", toupper(names(voc)))

# saving VOC ####
voc[is.na(voc)] <- 0
names(voc)


for (i in seq_along(names(voc))) {
  g_x <- g
  g_x <- merge(g, voc[[names(voc)[i]]], by = "id", all = T)
  g_x <- st_sf(g_x, geometry = g$geometry)
  g_x$group <- NULL
  saveRDS(g_x, file = paste0("post/spec_grid/", names(voc)[i], ".rds"))
}

# other gases ####
nx <- paste0(pol, "_all_EXH_", months_subset)
for (j in seq_along(nx)) {
  x <- gx[nx[j]]
  x[is.na(x)] <- 0
  mm_x <- units::set_units(mol[j], "g/mol") # mm: massa molar
  x[[1]] <- x[[1]] * (mm_x)^-1
  x <- st_sf(x, geometry = g$geometry)
  saveRDS(x, paste0("post/spec_grid/E_", pol[j], ".rds"))
}


# PM
# we need to add time units
# Assuming hourly emissions, the untis will ne ug/m2/s
# when inputting emissions into wrfchemi, we must add % hourly profile
gPM <- gx[grep("PM", names(gx), value = T)]
gPM$PM <- gPM$PM_all_EXH_08 + gPM$PM25RES_all_EXH_08
gPM$PMcoarse <- gPM$PM10RES_all_EXH_08 - gPM$PM25RES_all_EXH_08

# gPM[[1]] <- gPM[[1]]*units::set_units(1, 1/h)
# gPM[[2]] <- gPM[[2]]*units::set_units(1, 1/h)
# gPM[[3]] <- gPM[[3]]*units::set_units(1, 1/h)
gPM[[4]] <- gPM[[4]] * units::set_units(1, 1 / h)
gPM[[5]] <- gPM[[5]] * units::set_units(1, 1 / h)


names(gPM)
gPM$id <- NULL
gPM1 <- speciate(x = gPM["PM"], spec = aer, list = T)

for (i in 1:length(names(gPM1))) {
  gPMx <- st_sf(gPM1[[i]], geometry = g$geometry)
  gPMx$group <- NULL
  saveRDS(gPMx, paste0("post/spec_grid/", toupper(names(gPM1))[i], ".rds"))
}
for (i in 1:ncol(gPM)) gPM[, i] <- units::set_units(gPM[, i], "ug/m^2/s")

# NO PM10 yet!
# gPM10 <- st_set_geometry(readRDS("post/grids/PM10.rds"), NULL)
# names(gPM10)
# gPM10$id <- NULL
# for(i in 1:ncol(gPM10)) gPM10[, i] <- units::set_units(gPM10[, i], "ug/m^2/s")
#
#
# # gPM2510 are in g/km/h
# # need to change units to ug/m2/s
# gPM2510 <- gPM10 -  gPM
# gPM2510 <- st_sf(gPM2510, geometry = g$g)
gPM2510 <- st_sf(gPM["PMcoarse"], geometry = g$geometry)
saveRDS(gPM2510, "post/spec_grid/E_PM_10.rds")

switch(language,
  "portuguese" = message("\npost/spec_grid"),
  "english" = message("\npost/spec_grid"),
  "spanish" = message("\npost/spec_grid")
)
ls()
suppressWarnings(
  rm("g", "gPM", "gPM1", "gPM10", "gPM2510", "gPMx", "i", "pol")
)