suppressWarnings(file.remove("emi/DF_EVAP.csv"))

# Exhaust ####
switch(language,
       "portuguese" = cat("Estimando emissões\n"),
       "english" = cat("Estimating emissions\n"),
       "spanish" = cat("Estimando emisiones\n")
)

metadata <- metadata[metadata$f == "G", ]

for (i in seq_along(metadata$vehicles)) {
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  pro <- tfs[[metadata$vehicles[i]]]
  
  std1 <- std[[metadata$vehicles[i]]]
  
  if(metadata$f[i] == "ELEC") next
  if(metadata$f[i] == "G HY" & metadata$t[i] == "Bus") next
  
  
  
  for (j in seq_along(pol)) {
    
    cat(pol[j], " ")
    
    if(pol[j] == "Evaporative_parking") {
      # Assuming that parks represent trips, then
      # g/trip * trip/km
      x <- x/metadata$km_cycle[i]
    }
    
    array_x <- emis_china(x = x, 
                          lkm = net$lkm, 
                          tfs = pro, 
                          v = metadata$v[i],
                          t = metadata$t[i],
                          f = metadata$f[i], 
                          standard = std1, 
                          s = metadata$sppm[i],
                          speed = speed,
                          te = met$Temperature,
                          hu = met$Humidity,
                          h = h$h,
                          p = pol[j],
                          array = T,
                          verbose =verbose)
    
    x_DF <- emis_post(
      arra = array_x,
      veh = metadata$vehicles[i],
      size = metadata$t[i],
      fuel = metadata$f[i],
      pollutant = "HC",
      type_emi = pol[j],
      by = "veh"
    )
    
    fwrite(x = x_DF, 
           file = "emi/DF_EVAP.csv", 
           append = TRUE)
    
    # saveRDS(x_DF,
    #         file = paste0(
    #           "emi/",
    #           metadata$vehicles[i], "/",
    #           metadata$vehicles[i], "_",
    #           pol[j],
    #           "_DF.rds"
    #         )
    # )
    
    x_STREETS <- emis_post(
      arra = array_x,
      pollutant = pol[j],
      by = "streets"
    )
    saveRDS(x_STREETS,
            file = paste0(
              "emi/",
              metadata$vehicles[i], "/",
              metadata$vehicles[i], "_HC_",
              pol[j],
              "_STREETS.rds"
            )
    )
  }
  suppressWarnings(rm(array_x, ef, x, x_DF, x_STREETS))
  gc()
}


switch(language,
       "portuguese" = message("\n\nArquivos em: /emi/*:"),
       "english" = message("\nFiles in: /emi/*"),
       "spanish" = message("\nArchivos en: /emi/*")
)


switch(language,
       "portuguese" = message("Limpando..."),
       "english" = message("Cleaning..."),
       "spanish" = message("Limpiando...")
)

suppressWarnings(
  rm(
    i, j, pol,
    n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC,
    ns, ln, p, df, dl, cores
  )
)

invisible(gc())



