dir.create("post/mech/", showWarnings = FALSE)
dir.create(paste0("post/mech/", mech[k]), showWarnings = FALSE)

if(!type %in% c("streets", "grids")) {
  stop("type must be `streets` or `grids`")
}


if(type == "streets") {
  g <- net
} 

g$id <- 1:nrow(g)

if(remove_mech_files ){
  
mechdir <- paste0("post/mech/", mech[k])

fs <- list.files(
  path = mechdir,
  pattern = ".rds",
  full.names = TRUE,
  recursive = TRUE
)
file.remove(fs)
}

# Gasoline Exhaust
x <- st_set_geometry(readRDS(paste0("post/",
                                    type,
                                    "/G_NMHC.rds")), 
                     NULL)
x[is.na(x)] <- 0
id <- 1:nrow(x)
x$id <- NULL

# nx!!!!
nx <- names(x)

dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "G",
  veh = "LDV",
  eu = "I"
)

dx$id <- rep(id, length(unique(dx$pol)))

if(save_species) saveRDS(dx, paste0("post/", type, "/G_NMHC_SPEC.rds"))

vocg <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)

# Gasoline Evap
x <- st_set_geometry(readRDS(paste0("post/",
                                    type,
                                    "/EVAP_G_NMHC.rds")),
                     NULL)
x[is.na(x)] <- 0
id <- 1:nrow(x)
x$id <- NULL


dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "G",
  veh = "LDV",
  eu = "Evaporative"
)

dx$id <- rep(id, length(unique(dx$pol)))

if(save_species) saveRDS(dx, paste0("post/", type, "/EVAP_G_NMHC_SPEC.rds"))

vocev <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)


# Diesel Exhaust
x <- st_set_geometry(readRDS(paste0("post/",
                                    type,
                                    "/D_NMHC.rds")),
                     NULL)
x[is.na(x)] <- 0
id <- 1:nrow(x)
x$id <- NULL

dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "D",
  veh = "HDV",
  eu = "all"
)

dx$id <- rep(id, length(unique(dx$pol)))

if(save_species) saveRDS(dx, paste0("post/", type, "/D_NMHC_SPEC.rds"))

vocd <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)

# CNG Exhaust
x <- st_set_geometry(readRDS(paste0("post/",
                                    type,
                                    "/CNG_NMHC.rds")),
                     NULL)
x[is.na(x)] <- 0
id <- 1:nrow(x)
x$id <- NULL


dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "E85",
  veh = "LDV",
  eu = "Exhaust"
)

dx$id <- rep(id, length(unique(dx$pol)))

if(save_species) saveRDS(dx, paste0("post/", type, "/CNG_NMHC_SPEC.rds"))

voccng <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)


voc <- rbind(
  vocg,
  vocev,
  vocd,
  voccng,
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
  x <- st_sf(x, geometry = st_geometry(g))
  g_x$group <- NULL
  saveRDS(g_x, file = paste0("post/spec_grid/", mech,"/", 
                             names(voc)[i], ".rds"))
}

# other gases ####
for (j in seq_along(pol)) {
  x <- st_set_geometry(readRDS(paste0("post/", type, "/", pol[j], ".rds")), NULL)
  x[is.na(x)] <- 0
  mm_x <- units::set_units(mol[j], "g/mol") # mm: massa molar
  for (i in 2:ncol(x)) x[, i] <- x[, i] * (mm_x)^-1
  x <- st_sf(x, geometry = st_geometry(g))
  saveRDS(x, paste0("post/spec_grid/", mech,"/", pol[j], ".rds"))
}

# PM
if(type == "streets") {
  warning("Assuming area of street = length * 1m")  
  gPM <- readRDS(paste0("post/",
                        type,
                        "/PM2.5.rds"))
  gPM$id <- NULL
  area <- st_length(gPM)*units::set_units(1, m)
  
  gPM <- st_set_geometry(gPM, NULL)
  
  for (i in 1:ncol(gPM)) gPM[, i] <- gPM[, i]/area
  
  gPM1 <- speciate(x = gPM, 
                   spec = aer, 
                   list = T)
  for (i in 1:length(names(gPM1))) {
    gPMx <- st_sf(gPM1[[i]],
                  geometry = st_geometry(g))
    saveRDS(gPMx, paste0("post/mech/", 
                         mech[k], "/",
                         toupper(names(gPM1))[i], 
                         ".rds"))
    
  }
  
  for (i in 1:ncol(gPM)) gPM[, i] <- units::set_units(gPM[, i], "ug/m^2/s")
  
  gPM10 <- st_set_geometry(readRDS(paste0("post/",
                                          type,
                                          "/PM10.rds")), 
                           NULL)
  names(gPM10)
  gPM10$id <- NULL
  for (i in 1:ncol(gPM10)) gPM10[, i] <- gPM10[, i]/area
  
  for (i in 1:ncol(gPM10)) gPM10[, i] <- units::set_units(gPM10[, i], "ug/m^2/s")
  
  
  
} else {
  
  gPM <- st_set_geometry(readRDS(paste0("post/",
                                        type,
                                        "/PM2.5.rds")), 
                         NULL)
  names(gPM)
  gPM$id <- NULL
  gPM1 <- speciate(x = gPM, 
                   spec = aer, 
                   list = T)
  
  for (i in 1:length(names(gPM1))) {
    gPMx <- st_sf(gPM1[[i]],
                  geometry = g$geometry)
    saveRDS(gPMx, paste0("post/mech/", 
                         mech[k], "/",
                         toupper(names(gPM1))[i], 
                         ".rds"))
  }
  for (i in 1:ncol(gPM)) gPM[, i] <- units::set_units(gPM[, i], "ug/m^2/s")
  
  gPM10 <- st_set_geometry(readRDS(paste0("post/",
                                          type,
                                          "/PM10.rds")), 
                           NULL)
  names(gPM10)
  gPM10$id <- NULL
  for (i in 1:ncol(gPM10)) gPM10[, i] <- units::set_units(gPM10[, i], "ug/m^2/s")
  
}


# gPM2510 are in g/km/h
# need to change units to ug/m2/s
gPM2510 <- gPM10# - gPM
gPM2510 <- st_sf(gPM2510, geometry = g$g)
saveRDS(gPM2510, paste0("post/mech/",
                        mech[k],
                        "/E_PM_10.rds"))


switch(language,
       "portuguese" = message(paste0("\npost/mech/", mech[k])),
       "english" = message(paste0("\npost/mech/", mech[k])),
       "spanish" = message(paste0("\npost/mech/", mech[k]))
)
ls()
suppressWarnings(
  rm("g", "gPM", "gPM1", "gPM10", "gPM2510", "gPMx", "i", "pol")
)

