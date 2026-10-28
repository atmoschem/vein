suppressWarnings(file.remove("emi/DF_EVAP.csv"))
suppressWarnings(file.remove("emi/DF_EVAP_STREETS.csv"))

# Exhaust ####
switch(language,
       "portuguese" = cat("Estimando emissões\n"),
       "english" = cat("Estimating emissions\n"),
       "spanish" = cat("Estimando emisiones\n")
)

metadata <- metadata[metadata$f == "G" & metadata$fuel == "G", ]

# met row of the snapshot hour (met$Hour is 0-23)
ih <- which(met$Hour == hour_snapshot)
if (length(ih) != 1) stop("hour_snapshot ", hour_snapshot, " not found in met$Hour")

# altitude per street: config h must have one row per street of net
if (length(h$h) == nrow(net)) {
  hh <- h$h
} else {
  warning("config h has ", length(h$h), " rows but net has ", nrow(net),
          " streets: using the median altitude for all streets")
  hh <- median(h$h, na.rm = TRUE)
}


for (i in seq_along(metadata$vehicles)) {
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
  std1 <- std[[metadata$vehicles[i]]]
  
  
  for (j in seq_along(pol)) {
    cat(pol[j], " ")
    
    # do not overwrite x: the division must not accumulate across pollutants
    if(pol[j] == "Evaporative_parking") {
      # Assuming that parks represent trips, then
      # g/trip * trip/km
      xx <- x/metadata$km_cycle[i]
    } else {
      xx <- x
    }
    
    
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
                                yeardet = year)
    
    ef_base_speed <- ef_china_speed(speed = speed[[1]],
                                    f = metadata$f[i],
                                    standard = std1,
                                    p = pol[j], 
                                    long = TRUE)
    
    ef_base_temp <- ef_china_te(te = met$Temperature[ih],
                                v = metadata$v[i],
                                t = metadata$t[i],
                                f = metadata$f[i], 
                                p = pol[j])
    
    ef_base_hu <- ef_china_hu(hu = met$Humidity[ih],
                              v = metadata$v[i],
                              t = metadata$t[i],
                              f = metadata$f[i], 
                              p = pol[j])
    
    ef_base_th <- ef_china_th(hu = met$Humidity[ih],
                              te = met$Temperature[ih],
                              v = metadata$v[i],
                              t = metadata$t[i],
                              f = metadata$f[i], 
                              p = pol[j])
    
    efmet <- ef_base_temp * ef_base_hu * ef_base_th
    
    ef_base_h <- ef_china_h(h = hh,
                            v = metadata$v[i],
                            t = metadata$t[i],
                            f = metadata$f[i], 
                            p = pol[j])
    
    # base EF, deterioration and sulphur have one value per age: apply them
    # across the age columns with sweep() (a plain product recycles them down
    # the street rows). Speed varies by street and age, altitude by street,
    # and the weather factor efmet is one value for the snapshot hour,
    # applied once.
    ef_age <- as.numeric(remove_units(ef_base)) * ef_base_det * ef_base_s
    if (length(ef_age) != ncol(ef_base_speed)) {
      warning(metadata$vehicles[i], " ", pol[j], ": base/deterioration/sulphur ",
              "factors for ", length(ef_age), " of ", ncol(ef_base_speed),
              " ages (standards missing in vein's China tables)")
    }
    ef_base_speedv2 <- sweep(as.matrix(ef_base_speed), 2, ef_age, "*") *
      ef_base_h *
      efmet
    
    E <- Emissions(do.call("cbind", lapply(1:ncol(x), function(i) {
      ef_base_speedv2[, i] * xx[, i] * net$lkm
    })))
    
    x_DF <- data.frame(
      g = colSums(E, na.rm  = T),
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
    
    x_STREETS <- data.frame(
      id = net$id,
      g = rowSums(E, na.rm  = T),
      veh = metadata$vehicles[i],
      size = metadata$t[i],
      fuel = metadata$f[i],
      pollutant = "HC",
      type_emi = pol[j],
      by = "veh"
    )
    
    fwrite(x = x_STREETS, 
           file = "emi/DF_EVAP_STREETS.csv", 
           append = TRUE)
  }
  suppressWarnings(rm(array_x, ef, x, xx, x_DF, x_STREETS))
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