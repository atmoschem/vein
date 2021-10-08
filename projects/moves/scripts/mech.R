
unlink(paste0("post/", mech), recursive = T)

dir.create(paste0("post/", mech), showWarnings = FALSE)

fs <- list.files(
  path = paste0("post/", mech),
  pattern = ".rds",
  full.names = TRUE,
  recursive = TRUE
)
file.remove(fs)

# ntfs
ntfs <- paste0("H", 1:nrow(tfs))

# NMHC
dfnmhc <- st_set_geometry(readRDS("post/streets/NMHC.rds"), NULL)
setDT(dfnmhc)

# check
dfnmhc[, unique(cbind(family, fuel, process2))]

# vein does not have speciation for heavy vehicles using gasoline
# we assumed light vehicles
dfnmhc[family == "HDV" & fuel == "G"]$family <- "LDV"

# check
dfnmhc[, unique(cbind(family, fuel, process2))]

dfnmhc[family == "HDV" & fuel == "D", unique(process2)]

# vein does not have speciation for heavy vehicles evaporated
# we assumed exhaust
dfnmhc[fuel == "D" & process2 == "evap"]$process2 <- "exhaust"

# check
dfnmhc[, unique(cbind(family, fuel, process2))] -> check

if(nrow(check) == 7) print("Emissions adjusted")

# updating the emissions
dfnmhc[, 
       lapply(.SD, sum, na.rm = T),
       .SDcols = ntfs,
       by = .(id, family, fuel, process2)] -> dfnmhc

nrow(dfnmhc)/nrow(net)
dfnmhc[, unique(cbind(family, fuel, process2))]

# 1) LDV Diesel Exhaust ####
x <- dfnmhc[family == "LDV" & 
              fuel == "D", 
            c("id", ntfs), with = F]

x[is.na(x)] <- 0
id <- x$id
x$id <- NULL

# nx!!!!
nx <- ntfs

dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "D",
  veh = "LDV",
  eu = "all"
)

dx$id <- rep(id, length(unique(dx$pol)))

nmhc_ldv_d_exh <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)

# 2) HDV Diesel Exhaust ####
x <- dfnmhc[family == "HDV" & 
              fuel == "D", 
            c("id", ntfs), with = F]

x[is.na(x)] <- 0
id <- x$id
x$id <- NULL

# nx!!!!
nx <- ntfs

dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "D",
  veh = "HDV",
  eu = "all"
)

dx$id <- rep(id, length(unique(dx$pol)))

nmhc_hdv_d_exh <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)


# 3) LDV Gasoline Exhaust ####
x <- dfnmhc[family == "LDV" & 
              fuel == "G" & 
              process2 == "exhaust", 
            c("id", ntfs), with = F]

x[is.na(x)] <- 0
id <- x$id
x$id <- NULL

# nx!!!!
nx <- ntfs

dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "G",
  veh = "LDV",
  eu = "I"
)

dx$id <- rep(id, length(unique(dx$pol)))

nmhc_ldv_g_exh <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)

# 4) LDV Gasoline Evaporative ####
x <- dfnmhc[family == "LDV" & 
              fuel == "G" & 
              process2 == "evap", 
            c("id", ntfs), with = F]

x[is.na(x)] <- 0
id <- x$id
x$id <- NULL

dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "G",
  veh = "LDV",
  eu = "Evaporative"
)

dx$id <- rep(id, length(unique(dx$pol)))

nmhc_ldv_g_evap <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)

# 5) LDV E85 Exhaust ####
x <- dfnmhc[family == "LDV" & 
              fuel == "E85" & 
              process2 == "exhaust", 
            c("id", ntfs), with = F]

x[is.na(x)] <- 0
id <- x$id
x$id <- NULL

dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "E85",
  veh = "LDV",
  eu = "Exhaust"
)

dx$id <- rep(id, length(unique(dx$pol)))

nmhc_ldv_e85_exh <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)


# 6) LDV E85 Evaporative ####
x <- dfnmhc[family == "LDV" & 
              fuel == "E85" & 
              process2 == "evap", 
            c("id", ntfs), with = F]

x[is.na(x)] <- 0
id <- x$id
x$id <- NULL

dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "E85",
  veh = "LDV",
  eu = "Evaporative"
)

dx$id <- rep(id, length(unique(dx$pol)))

nmhc_ldv_e85_evap <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)


# 7) CNG ####
x <- dfnmhc[fuel == "CNG", 
            c("id", ntfs), with = F]

x[is.na(x)] <- 0
id <- x$id
x$id <- NULL

dx <- speciate(
  x = x,
  spec = "nmhc",
  fuel = "E85",
  veh = "LDV",
  eu = "Evaporative"
)

dx$id <- rep(id, length(unique(dx$pol)))

nmhc_cng <- emis_chem2(
  df = dx,
  mech = mech,
  nx = nx,
  na.rm = TRUE
)

# aggregate VOC ####
voc <- rbind(nmhc_ldv_d_exh,
             nmhc_hdv_d_exh,
             nmhc_ldv_g_exh,
             nmhc_ldv_g_evap,
             nmhc_ldv_e85_exh,
             nmhc_ldv_e85_evap,
             nmhc_cng,
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

# 
# 
# 
for (i in seq_along(names(voc))) {

  x <- st_sf(voc[[names(voc)[i]]], geometry= net$geom)
  x$group <- NULL
  saveRDS(x, file = paste0("post/", mech, "/STREETS_", names(voc)[i], ".rds"))
}

# 
# # other gases ####
# for (j in seq_along(pol)) {
#   x <- st_set_geometry(readRDS(paste0("post/grids/", pol[j], ".rds")), NULL)
#   x[is.na(x)] <- 0
#   mm_x <- units::set_units(mol[j], "g/mol") # mm: massa molar
#   for (i in 2:ncol(x)) x[, i] <- x[, i] * (mm_x)^-1
#   x <- st_sf(x, geometry = g$geometry)
#   saveRDS(x, paste0("post/", mech, "/E_", pol[j], ".rds"))
# }
# 
# # PM
# gPM <- st_set_geometry(readRDS("post/grids/PM.rds"), NULL)
# names(gPM)
# gPM$id <- NULL
# gPM1 <- speciate(x = gPM, spec = aer, list = T)
# 
# for (i in 1:length(names(gPM1))) {
#   gPMx <- st_sf(gPM1[[i]], geometry = g$geometry)
#   saveRDS(gPMx, paste0("post/", mech, "/", toupper(names(gPM1))[i], ".rds"))
# }
# for (i in 1:ncol(gPM)) gPM[, i] <- units::set_units(gPM[, i], "ug/m^2/s")
# 
# gPM10 <- st_set_geometry(readRDS("post/grids/PM10.rds"), NULL)
# names(gPM10)
# gPM10$id <- NULL
# for (i in 1:ncol(gPM10)) gPM10[, i] <- units::set_units(gPM10[, i], "ug/m^2/s")
# 
# 
# # gPM2510 are in g/km/h
# # need to change units to ug/m2/s
# gPM2510 <- gPM10 - gPM
# gPM2510 <- st_sf(gPM2510, geometry = g$g)
# saveRDS(gPM2510, paste0("post/", mech, "/E_PM_10.rds"))


switch(language,
       "portuguese" = message(paste0("\nEmissões (mol/h) em npost/", mech)),
       "english" = message(paste0("\nEmissions (mol/h) in post/", mech)),
       "spanish" = message(paste0("\nEmisiones (mol/h)  en post/", mech))
)
fi <- list.files(paste0("post/", mech), full.names = T)
print(fi)

switch(language,
       "portuguese" = message("\nLembra que tu tem emissões de poluentes criterio em post/streets"),
       "english" = message("\nRemember that you have criteria emissions in post/streets"),
       "spanish" = message("\nRecuerda que tu tienes mas emisiones de conaminantes criterio en post/streets")
)

fi <- list.files("post/streets", pattern = "ID",full.names = T)
print(fi)

ls()
suppressWarnings(
  rm("g", "gPM", "gPM1", "gPM10", "gPM2510", "gPMx", "i", "pol")
)