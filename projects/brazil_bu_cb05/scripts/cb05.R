# CMBZ ####
dir.create("post/spec_grid", showWarnings = FALSE)

x <- st_set_geometry(readRDS("post/grids/G_NMHC.rds"), NULL)
x[is.na(x)] <- 0
x$id<- NULL
vocE25EX <- speciate(x = x, 
                     spec = "iag", 
                     fuel = "G", 
                     veh = "veh", 
                     eu = "Exhaust",
                     list = T)


x <- st_set_geometry(readRDS(paste0("post/grids/", evap[1], ".rds")), NULL)
x[is.na(x)] <- 0
x$id <- NULL
vocE25EV <- speciate(x = x, 
                     spec = "iag", 
                     fuel = "G", 
                     veh = "veh", 
                     eu = "Evaporative",  
                     list = T)

x <- st_set_geometry(readRDS("post/grids/E_NMHC.rds"), NULL)
x[is.na(x)] <- 0
x$id <- NULL
vocE100EX <- speciate(x = x, 
                      spec = "iag", 
                      fuel = "E", 
                      veh = "veh", 
                      eu = "Exhaust",
                      list = T)

x <- st_set_geometry(readRDS(paste0("post/grids/", evap[2], ".rds")), NULL)
x[is.na(x)] <- 0
x$id <- NULL
vocE100EV <- speciate(x = x, 
                      spec = "iag", 
                      fuel = "E", 
                      veh = "veh", 
                      eu = "Evaporative",
                      list = T)

x <- st_set_geometry(readRDS("post/grids/D_NMHC.rds"), NULL)
x[is.na(x)] <- 0
x$id <- NULL
vocB5EX <- speciate(x = x, 
                    spec = "iag", 
                    fuel = "D", 
                    veh = "veh", 
                    eu = "Exhaust",
                    list = T)



voc <- lapply(1:length(vocE25EX), function(i) {
  vocE25EX[[i]] + vocE25EV[[i]] +
    vocE100EX[[i]] + vocE100EV[[i]] + 
    vocB5EX[[i]] 
})

names(voc) <- names(vocE25EX)

for (i in 1:length(voc) ) {
  voc[[i]]$id <- 1:nrow(voc[[i]]) 
}

# saving VOC ####
voc[is.na(voc)] <- 0
names(voc)

# eth ####
g_x <- g
g_x <- merge(g, voc[["e_eth"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_ETH.rds")

# hc3 ####
g_x <- g
g_x <- merge(g, voc[["e_hc3"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_HC3.rds")

# hc5 ####
g_x <- g
g_x <- merge(g, voc[["e_hc5"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_HC5.rds")

# hc8 ####
g_x <- g
g_x <- merge(g, voc[["e_hc8"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_HC8.rds")

# ol2 ####
g_x <- g
g_x <- merge(g, voc[["e_ol2"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_OL2.rds")

# olt ####
g_x <- g
g_x <- merge(g, voc[["e_olt"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_OLT.rds")

# oli ####
g_x <- g
g_x <- merge(g, voc[["e_oli"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_OLI.rds")

# iso ####
g_x <- g
g_x <- merge(g, voc[["e_iso"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_ISO.rds")

# tol ####
g_x <- g
g_x <- merge(g, voc[["e_tol"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_TOL.rds")

# xyl ####
g_x <- g
g_x <- merge(g, voc[["e_xyl"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_XYL.rds")

# ket ####
g_x <- g
g_x <- merge(g, voc[["e_ket"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_KET.rds")

# ch3oh ####
g_x <- g
g_x <- merge(g, voc[["e_ch3oh"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_CH3OH.rds")

# c2h5oh ####
g_x <- g
g_x <- merge(g, voc[["e_c2h5oh"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_C2H5OH.rds")


# hcho ####
g_x <- g
g_x <- merge(g, voc[["e_hcho"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_HCHO.rds")

# ald ####
g_x <- g
g_x <- merge(g, voc[["e_ald"]], by="id", all=T)
g_x <- st_sf(g_x, geometry = g$geometry)
saveRDS(g_x, file="post/spec_grid/E_ALD.rds")

# CO  ####
x <- st_set_geometry(readRDS("post/grids/CO.rds"), NULL)
x[is.na(x)] <- 0
mm_x <- units::set_units(12 + 16, "g/mol") # mm: massa molar
for (i in 2:ncol(x))   x[, i] <- x[, i]* (mm_x)^-1 
x <- st_sf(x, geometry = g$geometry)
saveRDS(x, "post/spec_grid/E_CO.rds")

# NO  ####
x <- st_set_geometry(readRDS("post/grids/NO.rds"), NULL)
x[is.na(x)] <- 0
mm_x <- units::set_units(14 + 16, "g/mol") # mm: massa molar
for (i in 2:ncol(x))   x[, i] <- x[, i]* (mm_x)^-1 
x <- st_sf(x, geometry = g$geometry)
saveRDS(x, "post/spec_grid/E_NO.rds")

# NO2  ####
x <- st_set_geometry(readRDS("post/grids/NO2.rds"), NULL)
x[is.na(x)] <- 0
mm_x <- units::set_units(14 + 2*16, "g/mol") # mm: massa molar
for (i in 2:ncol(x))   x[, i] <- x[, i]* (mm_x)^-1 
x <- st_sf(x, geometry = g$geometry)
saveRDS(x, "post/spec_grid/E_NO2.rds")

# SO2 ####
x <- st_set_geometry(readRDS("post/grids/SO2.rds"), NULL)
x[is.na(x)] <- 0
mm_x <- units::set_units(64 + 2*16, "g/mol") # mm: massa molar
for (i in 2:ncol(x))   x[, i] <- x[, i]* (mm_x)^-1 
x <- st_sf(x, geometry = g$geometry)
saveRDS(x, "post/spec_grid/E_SO2.rds")

# PM
gPM <- st_set_geometry(readRDS("post/grids/PM.rds"), NULL)
names(gPM)
gPM$id <- NULL
gPM1 <- speciate(x = gPM, spec = "pmiag", list = T)

for(i in 1:length(names(gPM1))){
  gPMx <- st_sf(gPM1[[i]], geometry = g$geometry)
  saveRDS(gPMx, paste0("post/spec_grid/", toupper(names(gPM1))[i], ".rds"))
  print(paste0("post/spec_grid/", toupper(names(gPM1))[i], ".rds"))
}
for(i in 1:ncol(gPM)) gPM[, i] <- units::set_units(gPM[, i], "ug/m^2/s")
file.remove("post/spec_grid/H2O.rds")

gPM10 <- st_set_geometry(readRDS("post/grids/PM10.rds"), NULL)
names(gPM10)
gPM10$id <- NULL
for(i in 1:ncol(gPM10)) gPM10[, i] <- units::set_units(gPM10[, i], "ug/m^2/s")


# gPM2510 are in g/km/h
# need to change units to ug/m2/s
gPM2510 <- gPM10 -  gPM
gPM2510 <- st_sf(gPM2510, geometry = g$g)
saveRDS(gPM2510, "post/spec_grid/E_PM_10.rds")


cat(paste0("Arquivos em ", getwd(), "/post/spec_grid/\n"))
ls()
suppressWarnings(
  rm("g", "gPM", "gPM1", "gPM10", "gPM2510", "gPMx", "i", "pol")
)