
dir.create("post/spec_grid", showWarnings = FALSE)

fs <- list.files(path = "post/spec_grid", 
                 pattern = ".rds", 
                 full.names = TRUE, 
                 recursive = TRUE)
file.remove(fs)

months_subset <- ifelse(nchar(months_subset) <2, 
                        paste0("0", months_subset), 
                        months_subset)


# Gasoline Exhaust
gx <- st_set_geometry(readRDS("post/emi_grid.rds"), NULL)

nx <- paste0("NMHC_G_EXH_", months_subset)
x <- gx[nx]
x[is.na(x)] <- 0
x$id<- NULL
vocE25EX <- speciate(x = x, 
                     spec = mech, 
                     fuel = "G", 
                     veh = "veh", 
                     eu = "Exhaust",
                     list = T)

# Gasoline Evap
nx <- paste0("NMHC_G_EVA_", months_subset)
x <- gx[nx]
x[is.na(x)] <- 0
x$id <- NULL
vocE25EV <- speciate(x = x, 
                     spec = mech, 
                     fuel = "G", 
                     veh = "veh", 
                     eu = "Evaporative",  
                     list = T)
# Etanol Exhaust
nx <- paste0("NMHC_E_EXH_", months_subset)
x <- gx[nx]
x[is.na(x)] <- 0
x$id <- NULL
vocE100EX <- speciate(x = x, 
                      spec = mech, 
                      fuel = "E", 
                      veh = "veh", 
                      eu = "Exhaust",
                      list = T)

# Etanol Evap
nx <- paste0("NMHC_E_EVA_", months_subset)
x <- gx[nx]
x[is.na(x)] <- 0
x$id <- NULL
vocE100EV <- speciate(x = x, 
                      spec = mech, 
                      fuel = "E", 
                      veh = "veh", 
                      eu = "Evaporative",
                      list = T)

# Diesel Exhaust
nx <- paste0("NMHC_D_EXH_", months_subset)
x <- gx[nx]
x[is.na(x)] <- 0
x$id <- NULL
vocB5EX <- speciate(x = x, 
                    spec = mech, 
                    fuel = "D", 
                    veh = "veh", 
                    eu = "Exhaust",
                    list = T)



voc <- lapply(1:length(vocE25EX), function(i) {
  vocE25EX[[i]] + vocE25EV[[i]] +
    vocE100EX[[i]] + vocE100EV[[i]] + 
    vocB5EX[[i]] 
})

names(voc) <- toupper(names(vocE25EX))

for (i in 1:length(voc) ) {
  voc[[i]]$id <- 1:nrow(voc[[i]]) 
}

# saving VOC ####
voc[is.na(voc)] <- 0
names(voc)

for(i in seq_along(names(voc))) {
  g_x <- g
  g_x <- merge(g, voc[[names(voc)[i]]], by="id", all=T)
  g_x <- st_sf(g_x, geometry = g$geometry)
  saveRDS(g_x, file = paste0("post/spec_grid/", names(voc)[i], ".rds"))
}

nx <- paste0(pol, "_all_EXH_", months_subset)
for(j in seq_along(nx)) {
  x <- gx[nx[j]]
  x[is.na(x)] <- 0
  mm_x <- units::set_units(mol[j], "g/mol") # mm: massa molar
  x[[1]] <- x[[1]]* (mm_x)^-1 
  x <- st_sf(x, geometry = g$geometry)
  saveRDS(x, paste0("post/spec_grid/E_", pol[j], ".rds"))
}


# PM
nx <- paste0( "PM_all_EXH_", months_subset)
gPM <- gx[nx]
# we need to add time units
# Assuming hourly emissions, the untis will ne ug/m2/s
# when inputting emissions into wrfchemi, we must add % hourly profile
gPM[[1]] <- gPM[[1]]*units::set_units(1, "1/h")

names(gPM)
gPM$id <- NULL
gPM1 <- speciate(x = gPM, spec = aer, list = T)

for(i in 1:length(names(gPM1))){
  gPMx <- st_sf(gPM1[[i]], geometry = g$geometry)
  saveRDS(gPMx, paste0("post/spec_grid/", toupper(names(gPM1))[i], ".rds"))
}
# NO PM10 yet!
# for(i in 1:ncol(gPM)) gPM[, i] <- units::set_units(gPM[, i], "ug/m^2/s")
# gPM10 <- st_set_geometry(readRDS("post/grids/PM10.rds"), NULL)
# names(gPM10)
# gPM10$id <- NULL
# for(i in 1:ncol(gPM10)) gPM10[, i] <- units::set_units(gPM10[, i], "ug/m^2/s")

# 
# # gPM2510 are in g/km/h
# # need to change units to ug/m2/s
# gPM2510 <- gPM10 -  gPM
# gPM2510 <- st_sf(gPM2510, geometry = g$g)
# saveRDS(gPM2510, "post/spec_grid/E_PM_10.rds")


switch (language,
        "portuguese" = message("\npost/spec_grid"),
        "english" = message("\npost/spec_grid"),
        "spanish" = message("\npost/spec_grid"))
ls()
suppressWarnings(
  rm("g", "gPM", "gPM1", "gPM10", "gPM2510", "gPMx", "i", "pol")
)
