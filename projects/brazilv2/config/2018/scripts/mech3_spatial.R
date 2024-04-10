dir.create("post/spec_grid", showWarnings = F)

fs <- list.files(
  path = "post/spec_grid",
  pattern = ".rds",
  full.names = TRUE,
  recursive = TRUE
)

file.remove(fs)

months_subset <- sprintf("%02d", 1:12)

gs <- list.files(
  path = "post/grids",
  pattern = ".rds",
  full.names = TRUE,
  recursive = TRUE
)

for(i in seq_along(gs)) {
  
  # Gasoline Exhaust ####
  x <- readRDS(gs[i])
  id <- x$id
  
  dx1 <- speciate(
    x = x$NMHC_G_EXHAUST,
    spec = "voc",
    fuel = "E25",
    veh = "LDV",
    eu = "Exhaust"
  )
  
  dx1$id <- rep(id, length(unique(dx1$pol)))
  
  # Gasoline Evap ####
  dx2 <- speciate(
    x = x$NMHC_G_EVAPORATIVES_HISTORIC,
    spec = "voc",
    fuel = "E25",
    veh = "LDV",
    eu = "Evaporative"
  )
  
  dx2$id <- rep(id, length(unique(dx2$pol)))
  
  # Etanol Exhaust ####
  dx3 <- speciate(
    x = x$NMHC_E_EXHAUST,
    spec = "voc",
    fuel = "E100",
    veh = "LDV",
    eu = "Exhaust"
  )
  
  dx3$id <- rep(id, length(unique(dx3$pol)))
  
  # Etanol Evap ####
  dx4 <- speciate(
    x = x$NMHC_E_EVAPORATIVES_HISTORIC,
    spec = "voc",
    fuel = "E100",
    veh = "LDV",
    eu = "Evaporative"
  )
  
  dx4$id <- rep(id, length(unique(dx4$pol)))
  
  # Diesel Exhaust ####
  dx5 <- speciate(
    x = x$NMHC_D_EXHAUST,
    spec = "voc",
    fuel = "B5",
    veh = "HDV",
    eu = "Exhaust"
  )
  
  dx5$id <- rep(id, length(unique(dx5$pol)))
  
  voc <- rbind(dx1,
               dx2,
               dx3,
               dx4,
               dx5
  )
  
  rm(dx1,
     dx2,
     dx3,
     dx4,
     dx5
  )
  gc()
  
  dfvoc <- voc[,
               sum(x),
               by = .(id, pol)
  ]
  
  rm(voc)
  gc()
  
  
  dcast.data.table(data = dfvoc, 
                   formula = id ~ pol, 
                   value.var = "V1") -> dfvoc
  
  gc()
  
  saveRDS(dfvoc, gsub("post/grids/emis_grid_",
                      "post/spec_grid/emis_voc_",
                      gs[i]))
  
  
}


switch (language,
        "portuguese" = message("\n\nArquivos em:"),
        "english" = message("\n\nFiles in:"),
        "spanish" = message("\n\nArchivos en:"))

message("post/spec_grid/*\n")
