year_selected <- as.numeric(substr(
  x = getwd(),
  start = nchar(getwd()) - 3,
  stop = nchar(getwd())
))

dir.create("post/spec_grid", showWarnings = F)

gs <- paste0("post/grids/", "emis_grid_", sprintf("%02d", 1:12), ".rds")


for (kk in seq_along(gs)) {
  x <- readRDS(gs[kk])

  nxx <- names(x)[2:19]

  for (i in seq_along(nxx)) {
    x[[nxx[i]]] <- x[[nxx[i]]] * set_units(1, g)
  }

  id <- x$id

  # Gasoline Exhaust ####
  dx1 <- speciate(
    x = x$NMHC_G_EXHAUST,
    spec = "nmhc",
    fuel = "E25",
    veh = "LDV",
    eu = "Exhaust"
  )

  dx1$id <- rep(id, length(unique(dx1$pol)))

  dx2 <- data.table(x = x$ETOH, pol = "ethanol", id = 1:nrow(x))

  dx <- rbind(dx1, dx2)

  # dx$id <- rep(id, length(unique(dx$pol)))

  dx <- dx[as.numeric(x) > 0, ]

  vocE25EX <- emis_chem2(
    df = dx,
    mech = mech,
    nx = "x",
    na.rm = TRUE
  )

  rm(dx1)
  gc()

  # Gasoline Evap ####
  dx2 <- speciate(
    x = x$NMHC_G_EVAPORATIVES_HISTORIC,
    spec = "nmhc",
    fuel = "E25",
    veh = "LDV",
    eu = "Evaporative"
  )

  dx2$id <- rep(id, length(unique(dx2$pol)))

  dx2 <- dx2[as.numeric(x) > 0, ]

  vocE25EV <- emis_chem2(
    df = dx2,
    mech = mech,
    nx = "x",
    na.rm = TRUE
  )

  rm(dx2)
  gc()

  # Etanol Exhaust ####
  dx3 <- speciate(
    x = x$NMHC_E_EXHAUST,
    spec = "nmhc",
    fuel = "E100",
    veh = "LDV",
    eu = "Exhaust"
  )

  dx3$id <- rep(id, length(unique(dx3$pol)))

  dx3 <- dx3[as.numeric(x) > 0, ]

  vocE100EX <- emis_chem2(
    df = dx3,
    mech = mech,
    nx = "x",
    na.rm = TRUE
  )

  rm(dx3)
  gc()

  # Etanol Evap ####
  dx4 <- speciate(
    x = x$NMHC_E_EVAPORATIVES_HISTORIC,
    spec = "nmhc",
    fuel = "E100",
    veh = "LDV",
    eu = "Evaporative"
  )

  dx4$id <- rep(id, length(unique(dx4$pol)))

  dx4 <- dx4[as.numeric(x) > 0, ]

  vocE100EV <- emis_chem2(
    df = dx4,
    mech = mech,
    nx = "x",
    na.rm = TRUE
  )

  rm(dx4)
  gc()

  # Diesel Exhaust ####
  dx5 <- speciate(
    x = x$NMHC_D_EXHAUST,
    spec = "nmhc",
    fuel = "B5",
    veh = "HDV",
    eu = "Exhaust"
  )

  dx5$id <- rep(id, length(unique(dx5$pol)))

  dx5 <- dx5[as.numeric(x) > 0, ]

  vocB5EX <- emis_chem2(
    df = dx5,
    mech = mech,
    nx = "x",
    na.rm = TRUE
  )

  rm(dx5)
  gc()

  voc <- rbind(vocE25EX, vocE25EV, vocE100EX, vocE100EV, vocB5EX)

  rm(vocE25EX, vocE25EV, vocE100EX, vocE100EV, vocB5EX)
  gc()

  dfvoc <- voc[,
    sum(x),
    by = .(id, group)
  ]

  dfvoc[, group := paste0("E_", group)]
  rm(voc)
  gc()

  # mol/km2/h
  dfvoc$V1 <- dfvoc$V1 / dmonth(year_selected, kk) / 24

  dcast.data.table(
    data = dfvoc,
    formula = id ~ group,
    value.var = "V1"
  ) -> dfvoc

  # PM ####
  gPM <- st_set_geometry(x[, "PM"], NULL) / dmonth(year_selected, kk) / 24

  gPM1 <- speciate(x = gPM, spec = aer, list = F)

  dfx <- cbind(dfvoc, gPM1)

  # gas ####

  for (i in seq_along(pol)) {
    # mol/km^2/h
    x[[paste0("E_", pol[i])]] <- x[[pol[i]]] /
      mw[i] /
      units::set_units(dmonth(year_selected, kk) / 24, "h")
  }

  dfx <- cbind(dfx, x[, pol, with = F])

  print(paste0("post/spec_grid/", "emis_voc_", sprintf("%02d", kk), ".rds"))

  saveRDS(
    dfx,
    paste0("post/spec_grid/", "emis_voc_", sprintf("%02d", kk), ".rds")
  )
  gc()
}


switch(
  language,
  "portuguese" = message("\n\nArquivos em:"),
  "english" = message("\n\nFiles in:"),
  "spanish" = message("\n\nArchivos en:")
)

message("post/spec_grid/*\n")
