# CMBZ ####
dir.create("post/spec_grid", showWarnings = FALSE)

fs <- list.files(
  path = "post/spec_grid",
  pattern = ".rds",
  full.names = TRUE,
  recursive = TRUE
)
file.remove(fs)

# Gasoline Exhaust
x <- st_set_geometry(readRDS("post/grids/G_NMHC.rds"), NULL)
x[is.na(x)] <- 0
id <- x$id
x$id <- NULL

# nx!!!!
nx <- names(x)

dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "E25",
  veh = "LDV",
  eu = "Exhaust"
)

dx$id <- rep(id, length(unique(dx$pol)))
names(dx)
vocE25EX <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)

# Gasoline Evap
x <- st_set_geometry(readRDS(paste0("post/grids/", evap[1], ".rds")), NULL)
x[is.na(x)] <- 0
id <- x$id
x$id <- NULL



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
x <- st_set_geometry(readRDS("post/grids/E_NMHC.rds"), NULL)
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
x <- st_set_geometry(readRDS(paste0("post/grids/", evap[1], ".rds")), NULL)
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
x <- st_set_geometry(readRDS("post/grids/D_NMHC.rds"), NULL)
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
for (j in seq_along(pol)) {
  x <- st_set_geometry(readRDS(paste0("post/grids/", pol[j], ".rds")), NULL)
  x[is.na(x)] <- 0
  mm_x <- units::set_units(mol[j], "g/mol") # mm: massa molar
  for (i in 2:ncol(x)) x[, i] <- x[, i] * (mm_x)^-1
  x <- st_sf(x, geometry = g$geometry)
  saveRDS(x, paste0("post/spec_grid/E_", pol[j], ".rds"))
}

# PM
gPM <- st_set_geometry(readRDS("post/grids/PM.rds"), NULL)
names(gPM)
gPM$id <- NULL
gPM1 <- speciate(x = gPM, spec = aer, list = T)

for (i in 1:length(names(gPM1))) {
  gPMx <- st_sf(gPM1[[i]], geometry = g$geometry)
  saveRDS(gPMx, paste0("post/spec_grid/", toupper(names(gPM1))[i], ".rds"))
}
for (i in 1:ncol(gPM)) gPM[, i] <- units::set_units(gPM[, i], "ug/m^2/s")

gPM10 <- st_set_geometry(readRDS("post/grids/PM10.rds"), NULL)
names(gPM10)
gPM10$id <- NULL
for (i in 1:ncol(gPM10)) gPM10[, i] <- units::set_units(gPM10[, i], "ug/m^2/s")


# gPM2510 are in g/km/h
# need to change units to ug/m2/s
gPM2510 <- gPM10 - gPM
gPM2510 <- st_sf(gPM2510, geometry = g$g)
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