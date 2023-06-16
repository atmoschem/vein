suppressWarnings(file.remove("emi/DF_EXHAUST.csv"))
suppressWarnings(file.remove("emi/DF_EXHAUST_STREETS.csv"))



# Exhaust ####
switch(language,
       "portuguese" = cat("Estimando emissões\n"),
       "english" = cat("Estimating emissions\n"),
       "spanish" = cat("Estimando emisiones\n")
)



for (i in seq_along(metadata$vehicles)) {
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
  std1 <- std[[metadata$vehicles[i]]]
  
  if(metadata$f[i] == "ELEC") next
  if(metadata$f[i] == "G HY" & metadata$t[i] == "Bus") next
  
  
  
  for (j in seq_along(pol)) {
    cat(pol[j], " ")
    

    ef_base <- ef_china_long(v = metadata$v[i],
                             t = metadata$t[i],
                             f = metadata$f[i], 
                             standard = std1,
                             p = pol[j])
    
    ef_base_s <- ef_china_s(metadata$sppm[i],
                            f = metadata$f[i],
                            standard = std1,
                            p = pol[j])
    
    ef_base_det <- ef_china_det(v = metadata$v[i],
                                t = metadata$t[i],
                                f = metadata$f[i], 
                                standard = std1,
                                p = pol[j],
                                yeardet = 2018)
    
    ef_base_speed <- ef_china_speed(speed = speed[[1]],
                     f = metadata$f[i],
                     standard = std1,
                     p = pol[j], 
                     long = TRUE)
    
    ef_base_temp <- ef_china_te(te = met$Temperature[1],
                                v = metadata$v[i],
                                t = metadata$t[i],
                                f = metadata$f[i], 
                                p = pol[j])
    
    ef_base_hu <- ef_china_hu(hu = met$Humidity[1],
                              v = metadata$v[i],
                              t = metadata$t[i],
                              f = metadata$f[i], 
                              p = pol[j])
    
    ef_base_th <- ef_china_th(hu = met$Humidity[1],
                              te = met$Temperature[1],
                              v = metadata$v[i],
                              t = metadata$t[i],
                              f = metadata$f[i], 
                              p = pol[j])
    
    efmet <- ef_base_temp * ef_base_hu * ef_base_th

    ef_base_h <- ef_china_h(h = h$h[1],
                            v = metadata$v[i],
                            t = metadata$t[i],
                            f = metadata$f[i], 
                            p = pol[j])
    
    ef_base_speedv2 <- ef_base_h*
      ef_base_speed *
      remove_units(ef_base) *
      ef_base_det*
      ef_base_s *
      efmet
    
    
    efmet <- ef_base_temp * ef_base_hu * ef_base_th

    ef_base_speedv2 <- ef_base_speedv2*efmet
    
    E <- Emissions(do.call("cbind", lapply(1:ncol(x), function(i) {
      ef_base_speedv2[, i] * x[, i] * net$lkm
    })))
    
    x_DF <- data.frame(
      g = colSums(E, na.rm  = T),
      veh = metadata$vehicles[i],
      size = metadata$t[i],
      fuel = metadata$f[i],
      pollutant = pol[j],
      type_emi = "Exhaust",
      by = "veh"
    )
    
    fwrite(x = x_DF, 
           file = "emi/DF_EXHAUST.csv", 
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
    
    x_STREETS <- data.frame(
      id = net$id,
      g = rowSums(E, na.rm  = T),
      pollutant = pol[j],
        veh = metadata$vehicles[i],
        size = metadata$t[i],
        fuel = metadata$f[i],
        pollutant = pol[j],
        type_emi = "Exhaust",
        by = "veh"
      )
    
    fwrite(x = x_STREETS, 
           file = "emi/DF_EXHAUST_STREETS.csv", 
           append = TRUE)
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


invisible(gc())