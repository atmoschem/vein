
# Escapamento ####
switch(language,
       "portuguese" = cat("Estimando emissões Cold Start\n"),
       "english" = cat("Estimating emissions Cold Start\n"),
       "spanish" = cat("Estimando emisiones Cold Start\n")
)


# Cold Start ####

metadata_cold <- metadata[metadata$fuel_eea_old %in% "G" &
                            metadata$v_eea_old %in% c("PC", "LCV"), ]

for (i in seq_along(metadata_cold$vehicles)) {
  
  cat("\n", metadata_cold$vehicles[i],
      rep("", max(nchar(metadata_cold$vehicles) + 1) - nchar(metadata_cold$vehicles[i]))
  )
  
  veh <- readRDS(paste0("veh/", metadata_cold$vehicles[i], ".rds"))
  
  # euro
  cate <- suppressWarnings(
    as.character(as.roman(gsub("Euro ", "", 
                               euro[[metadata_cold$vehicles[i]]]))))
  cate[is.na(cate)] <- "PRE"
  
  for (j in seq_along(pol)) {
    
    f_fcorr <- fcorr[veh == metadata_cold$v_eea_old_fuel[i] &
                       pol == pol[j]]
    
    # deterioration factor
    if(pol[j] %in% c("CO", "NO", "NO2", "HC", "NMHC")) {
      
      (efdet <- emis_det(po = ifelse(
        pol[j] %in% c("NO", "NOx", "NO2"),"NOx",
        ifelse(
          pol[j] %in% c("NMHC", "HC"),"HC",
          "CO"
        )),
        cc = "<=1400",
        eu = cate[1:ncol(veh)], 
        km = cumsum(mileage[[metadata$vehicles[i]]])))
    } else {
      efdet <- 1
    }
    
    cat(pol[j], " ")
    
    ltrip <- add_lkm(metadata_cold$km_cycle[i])
    ta <- met$value
    a <- cold_mileage(ltrip = ltrip, ta = celsius(ta))
    
    ef <- lapply(seq_along(cate), function(eu) {
      ef_ldv_speed(v = metadata_cold$v_eea_old[i],
                       t = metadata_cold$t_eea_old[i],
                       cc = metadata_cold$cc_eea_old[i],
                       f = if(metadata_cold$fuel_eea_old[i] == "LPG" & pol[j] == "PM") "G" else  metadata_cold$fuel_eea_old[i],  
                       p = pol[j],
                       eu = cate[eu],
                       # speed = Speed(metadata$speed[i]),
                       fcorr = if(nrow(f_fcorr) == 0) rep(1, 8) else f_fcorr$value)
      })
    veh <- veh*efdet
    k <- 1
    
    poly <- ifelse(
      pol[j] %in% c("NO", "NO2"), "NOx",
      ifelse(
        pol[j] %in% c("NMHC"), "HC",
        pol[j]
      ))
    
    k <- ifelse(
      pol[j] %in% c("NO", "NMHC"), 0.9,
      ifelse(
        pol[j] %in% c("NO2"), 0.1,
        1
      ))
    
    
    efcold <- lapply(seq_along(cate), function(eu) {
      ef_ldv_cold(ta = ta,
                          cc = ifelse(metadata_cold$cc_eea_old[i] == "<3.5",
                                      ">2000",
                                      metadata_cold$cc_eea_old[i]),
                          f = metadata_cold$fuel_eea_old[i],   
                          p = poly,
                          eu = cate[eu],
                          # speed = Speed(metadata_cold$speed[i]),
                          fcorr = if(nrow(f_fcorr) == 0) rep(1, 8) else f_fcorr$value)
    })
    nrow(veh) ==  nrow(ef)
    ef$speed <- NULL
    
    if(IM) {
      ok <- im_ok[[metadata$vehicles[i]]][1:ncol(veh)]
      nok <- 1 - ok
      if(pol == "CO")fim <- im_co[[metadata$vehicles[i]]][1:ncol(veh)]
      if(pol %in% c("HC", "NMHC")) fim <- im_hc[[metadata$vehicles[i]]][1:ncol(veh)]
      if(pol %in% c("NO", "NO2", "NOx")) fim <- im_nox[[metadata$vehicles[i]]][1:ncol(veh)]
      if(pol %in% c("PM")) fim <- im_pm[[metadata$vehicles[i]]][1:ncol(veh)]
      ef <- EmissionFactors(matrix(as.numeric(ef)*ok + as.numeric(ef)*fim*nok, nrow = 1))
    }
    
    
    array_x <- emis_cold(
      veh = veh, 
      lkm = net$lkm, 
      ef = ef, 
      efcold = efcold, 
      beta = matrix(a, ncol= ncol(veh)), 
      speed = speed, 
      agemax = 40, 
      simplify = T,
      profile = tfs[[metadata_cold$vehicles[i]]], 
    )
    
    x_DF <- emis_post(
      arra = array_x,
      veh = metadata_cold$vehicles[i],
      size = metadata_cold$size[i],
      fuel = metadata_cold$fuel[i],
      pollutant = pol[j],
      type_emi = "Cold",
      by = "veh"
    )
    
    fwrite(x_DF, "emi/cold.csv", append = TRUE)
    
    x_ST <- emis_post(
      arra = array_x,
      veh = metadata$vehicles[i],
      size = metadata$size[i],
      fuel = metadata$fuel[i],
      pollutant = pol[j],
      type_emi = "Exhaust",
      by = "streets"
    )
    
    x_ST$fuel <- metadata$fuel[i]
    
    fwrite(x_ST, paste0("emi/", metadata$vehicles[i], "/COLD_", pol[j], ".csv"), append = FALSE)
    
    
  }
}


switch(language,
       "portuguese" = message("\nEmissões em: /emi/exhaust.csv:"),
       "english" = message("\nEmissions in: /emi/exhaust.csv"),
       "spanish" = message("\nEmisiones en: /emi/exhaust.csv")
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

ls()


invisible(gc())