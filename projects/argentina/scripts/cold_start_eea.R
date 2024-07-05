
# Escapamento ####
switch(language,
       "portuguese" = cat("Estimando emissões Cold Start\n"),
       "english" = cat("Estimating emissions Cold Start\n"),
       "spanish" = cat("Estimando emisiones Cold Start\n")
)

setDT(pmonth)

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
    
    ef <- ef_ldv_speed(v = metadata_cold$v_eea_old[i],
                       t = metadata_cold$t_eea_old[i],
                       cc = metadata_cold$cc_eea_old[i],
                       f = if(metadata_cold$fuel_eea_old[i] == "LPG" & pol[j] == "PM") "G" else  metadata_cold$fuel_eea_old[i],  
                       p = pol[j],
                       eu = cate,
                       speed = Speed(metadata$speed[i]),
                       fcorr = if(nrow(f_fcorr) == 0) rep(1, 8) else f_fcorr$value)[1:length(veh)]
    
    ef <- ef*efdet
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
    
    
    efcold <- ef_ldv_cold(ta = matrix(ta, nrow = 1),
                          cc = ifelse(metadata_cold$cc_eea_old[i] == "<3.5",
                                      ">2000",
                                      metadata_cold$cc_eea_old[i]),
                          f = metadata_cold$fuel_eea_old[i],   
                          p = poly,
                          eu = cate,
                          speed = Speed(metadata_cold$speed[i]),
                          fcorr = if(nrow(f_fcorr) == 0) rep(1, 8) else f_fcorr$value)
    
    nrow(veh) ==  nrow(ef)
    ef$speed <- NULL
    
    array_x <- emis_cold_td(
      veh = veh,
      lkm = mileage[[metadata_cold$vehicles[i]]],
      ef = ef[, 1:ncol(veh)],
      efcold = efcold[, 1:ncol(veh)],
      fortran = TRUE,
      beta = matrix(a, nrow = 1),
      nt = nt,
      pro_month = pmonth[fuel == ifelse(metadata$vehicles[i] == "D", "D", "G")]$consumption_lt,
      verbose = verbose,
      params = list(
        veh = metadata_cold$vehicles[i],
        size = metadata_cold$size[i],
        fuel = metadata_cold$fuel[i],
        pollutant = pol[j],
        type_emi = "Cold",
        subtype_emi = "Exhaust",
        baseyear = year,
        month = rep(1:12, each = ncol(veh))
      )
    )
    
    
    fwrite(array_x, "emi/exhaust.csv", append = TRUE)
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