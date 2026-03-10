dir.create("post/spec_grid/", showWarnings = FALSE)

place <- paste0("post/spec_grid/", mech, "/")

unlink(paste0("post/spec_grid/", mech), recursive = T)

dir.create(place, showWarnings = FALSE)

g <- readRDS("post/emi_grid.rds")["id"]

fs <- list.files(
  path = "post/spec_grid",
  pattern = ".rds",
  full.names = TRUE,
  recursive = TRUE
)
file.remove(fs)

months_subset <- c(paste0("0", 1:9), 10:12)
lapply(paste0(place, months_subset), dir.create, )

for (j in 1:12) {
  cat(months_subset[j], " ")


  # Gasoline Exhaust ####
  gx <- st_set_geometry(readRDS("post/emi_grid.rds"), NULL)
  id <- gx$id

  nx <- paste0("NMHC_G_EXH_", months_subset[j])
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


  # Gasoline Evap ####
  nx <- paste0("NMHC_G_EVA_", months_subset[j])
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

  # Etanol Exhaust ####
  nx <- paste0("NMHC_E_EXH_", months_subset[j])
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

  # Etanol Evap ####
  nx <- paste0("NMHC_E_EVA_", months_subset[j])
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

  # Diesel Exhaust ####
  nx <- paste0("NMHC_D_EXH_", months_subset[j])
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
    g_x <- gx
    g_x <- merge(g, voc[[names(voc)[i]]], by = "id", all = T)
    g_x <- st_sf(g_x, geometry = g$geometry)
    g_x$group <- NULL
    saveRDS(g_x, file = paste0(place, months_subset[j], "/", names(voc)[i], ".rds"))
  }

  # other gases ####
  nx <- paste0(pol, "_all_EXH_", months_subset[j])
  for (i in seq_along(nx)) {
    x <- gx[nx[i]]
    x[is.na(x)] <- 0
    mm_x <- units::set_units(mol[i], "g/mol") # mm: massa molar
    x[[1]] <- x[[1]] * (mm_x)^-1
    x <- st_sf(x, geometry = g$geometry)
    saveRDS(x, paste0(place, months_subset[j], "/E_", pol[i], ".rds"))
  }


  # PM
  # we need to add time units
  # Assuming hourly emissions, the untis will ne ug/m2/s
  # when inputting emissions into wrfchemi, we must add % hourly profile
  gPM <- gx[grep("PM", names(gx), value = T)]
  gPM$PM <- gPM[[paste0("PM_all_EXH_", months_subset[j])]] + gPM[[paste0("PM25RES_all_EXH_", months_subset[j])]]
  gPM$PMcoarse <- gPM[[paste0("PM10RES_all_EXH_", months_subset[j])]] - gPM[[paste0("PM25RES_all_EXH_", months_subset[j])]]

  gPM$PM <- gPM$PM * units::set_units(1, 1 / h)
  gPM$PMcoarse <- gPM$PMcoarse * units::set_units(1, 1 / h)


  names(gPM)
  gPM$id <- NULL
  gPM1 <- speciate(x = gPM["PM"], spec = aer, list = T)
  names(gPM1)

  for (i in 1:length(names(gPM1))) {
    gPMx <- st_sf(gPM1[[i]], geometry = g$geometry)
    saveRDS(gPMx, paste0(place, months_subset[j], "/", toupper(names(gPM1))[i], ".rds"))
  }
  gPM$PMcoarse <- units::set_units(gPM$PMcoarse, "ug/m^2/s")

  gPM2510 <- st_sf(gPM["PMcoarse"], geometry = g$geometry)
  saveRDS(gPM2510, paste0(place, months_subset[j], "/E_PM_10.rds"))
}

switch(language,
  "portuguese" = message("\npost/spec_grid"),
  "english" = message("\npost/spec_grid"),
  "spanish" = message("\npost/spec_grid")
)
ls()
suppressWarnings(
  rm("g", "gPM", "gPM1", "gPM10", "gPM2510", "gPMx", "i", "j")
)