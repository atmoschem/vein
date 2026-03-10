year_selected              <- as.numeric(substr(x = getwd(), 
                                        start = nchar(getwd()) - 3, 
                                        stop = nchar(getwd()) ))

dir.create("post/spec_grid", showWarnings = F)

gs <- paste0("post/grids/",
             "emis_grid_",
             sprintf("%02d", 1:12),
             ".rds")


for(kk in seq_along(gs)){
  
  x <- readRDS(gs[kk])
  
  nxx <- names(x)[2:19]
  
  for(i in seq_along(nxx)) {
    x[[nxx[i]]] <- x[[nxx[i]]]*set_units(1, g)
  }
  
  id <- x$id
  
  # Gasoline Exhaust ####
  dx1 <- speciate(
    x = x$NMHC_G_EXHAUST,
    spec = "voc",
    fuel = "E25",
    veh = "LDV",
    eu = "Exhaust"
  )
  
  
  dx1$id <- rep(id, length(unique(dx1$pol)))
  
  dx2 <- data.table(x = x$ETOH,
                    pol = "ethanol",
                    id = 1:nrow(x))
  
  dx <- rbind(dx1, dx2)
  
 dx <- dx[as.numeric(x) > 0, ]

  
  # Gasoline Evap ####
  dx2 <- speciate(
    x = x$NMHC_G_EVAPORATIVES_HISTORIC,
    spec = "voc",
    fuel = "E25",
    veh = "LDV",
    eu = "Evaporative"
  )
  
  dx2$id <- rep(id, length(unique(dx2$pol)))
 
  dx2 <- dx2[as.numeric(x) > 0, ]

  
  # Etanol Exhaust ####
  dx3 <- speciate(
    x = x$NMHC_E_EXHAUST,
    spec = "voc",
    fuel = "E100",
    veh = "LDV",
    eu = "Exhaust"
  )
  
  dx3$id <- rep(id, length(unique(dx3$pol)))
 
  dx3 <- dx3[as.numeric(x) > 0, ]

  # Etanol Evap ####
  dx4 <- speciate(
    x = x$NMHC_E_EVAPORATIVES_HISTORIC,
    spec = "voc",
    fuel = "E100",
    veh = "LDV",
    eu = "Evaporative"
  )
   
  dx4$id <- rep(id, length(unique(dx4$pol)))
 
  dx4 <- dx4[as.numeric(x) > 0, ]


  # Diesel Exhaust ####
  dx5 <- speciate(
    x = x$NMHC_D_EXHAUST,
    spec = "voc",
    fuel = "B5",
    veh = "HDV",
    eu = "Exhaust"
  )
  
  dx5$id <- rep(id, length(unique(dx5$pol)))
  
  dx5 <- dx5[as.numeric(x) > 0, ]

  voc <- rbind(dx,
               dx2,
               dx3,
               dx4,
               dx5)
  
  rm(dx,
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
  
  
  print(paste0("post/spec_grid/",
               "emis_voc_",
               sprintf("%02d", kk),
               ".rds"))
  
  saveRDS(dfvoc, 
          paste0("post/spec_grid/",
                 "emis_voc_",
                 sprintf("%02d", kk),
                 ".rds"))
gc()  
}


switch (language,
        "portuguese" = message("\n\nArquivos em:"),
        "english" = message("\n\nFiles in:"),
        "spanish" = message("\n\nArchivos en:"))

message("post/spec_grid/*\n")


