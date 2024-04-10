dir.create("post/spec_grid", showWarnings = F)

gs <- paste0("post/grids/",
             dom,
             "emis_grid_0",
             month,
             ".rds")

# Gasoline Exhaust ####
x <- readRDS(gs)
id <- x$id

dx1 <- speciate(
  x = x$NMHC_G_EXHAUST,
  spec = "nmhc",
  fuel = "E25",
  veh = "LDV",
  eu = "Exhaust"
)

dx1$id <- rep(id, length(unique(dx1$pol)))

dx2 <- data.table(x = x$ETOH,
                  pol = "ethanol",
                  id = 1:nrow(x))

dx <- rbind(dx1, dx2)

# dx$id <- rep(id, length(unique(dx$pol)))

vocE25EX <- emis_chem2(
  df = dx,
  mech = mech,
  nx = "x",
  na.rm = TRUE
)



# Gasoline Evap ####
dx2 <- speciate(
  x = x$NMHC_G_EVAPORATIVES_HISTORIC,
  spec = "nmhc",
  fuel = "E25",
  veh = "LDV",
  eu = "Evaporative"
)

dx2$id <- rep(id, length(unique(dx2$pol)))

vocE25EV <- emis_chem2(
  df = dx2,
  mech = mech,
  nx = "x",
  na.rm = TRUE
)



# Etanol Exhaust ####
dx3 <- speciate(
  x = x$NMHC_E_EXHAUST,
  spec = "nmhc",
  fuel = "E100",
  veh = "LDV",
  eu = "Exhaust"
)

dx3$id <- rep(id, length(unique(dx3$pol)))

vocE100EX <- emis_chem2(
  df = dx3,
  mech = mech,
  nx = "x",
  na.rm = TRUE
)

# Etanol Evap ####
dx4 <- speciate(
  x = x$NMHC_E_EVAPORATIVES_HISTORIC,
  spec = "nmhc",
  fuel = "E100",
  veh = "LDV",
  eu = "Evaporative"
)

dx4$id <- rep(id, length(unique(dx4$pol)))

vocE100EV <- emis_chem2(
  df = dx4,
  mech = mech,
  nx = "x",
  na.rm = TRUE
)


# Diesel Exhaust ####
dx5 <- speciate(
  x = x$NMHC_D_EXHAUST,
  spec = "nmhc",
  fuel = "B5",
  veh = "HDV",
  eu = "Exhaust"
)

dx5$id <- rep(id, length(unique(dx5$pol)))

vocB5EX <- emis_chem2(
  df = dx5,
  mech = mech,
  nx = "x",
  na.rm = TRUE
)



voc <- rbind(vocE25EX,
             vocE25EV,
             vocE100EX,
             vocE100EV,
             vocB5EX)

rm(dx1,
   dx2,
   dx3,
   dx4,
   dx5
)
gc()

dfvoc <- voc[,
             sum(x),
             by = .(id, group)
]

rm(voc)
gc()


dcast.data.table(data = dfvoc, 
                 formula = id ~ group, 
                 value.var = "V1") -> dfvoc

gc()

print(paste0("post/spec_grid/",
             dom,
             "emis_voc_",
             sprintf("%02d", month),
             ".rds"))

saveRDS(dfvoc, 
        paste0("post/spec_grid/",
                      dom,
                      "emis_voc_",
                      sprintf("%02d", month),
                      ".rds"))




switch (language,
        "portuguese" = message("\n\nArquivos em:"),
        "english" = message("\n\nFiles in:"),
        "spanish" = message("\n\nArchivos en:"))

message("post/spec_grid/*\n")
