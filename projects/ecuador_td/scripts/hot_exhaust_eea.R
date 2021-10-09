suppressWarnings(file.remove("emi/exhaust.csv"))

setDT(pmonth)

# Escapamento ####
switch(language,
       "portuguese" = cat("Estimando emissões Hot Running\n"),
       "english" = cat("Estimating emissions Hot Running\n"),
       "spanish" = cat("Estimando emisiones Hot Running\n")
)
metadata_original <- metadata
metadata <- metadata[metadata$fuel != "ELEC", ]

# fuel correction ####
euros <- c("PRE", "I", "II", "III", "IV", "V", "VI", "VIc") 

fc <- as.list(fuel_spec$value)
names(fc) <- paste0(fuel_spec$fuel, "_", fuel_spec$parameter)

fuel_corr(
  euro = euros, 
  g = c(e100 = fc$G_e100, 
        aro = fc$G_aro, 
        o2 = fc$G_o2, 
        e150 = fc$G_e150, 
        olefin = fc$G_olefin, 
        s = fc$G_s),
  d = c(den = fc$D_den,
        pah = fc$D_pah,
        cn = fc$D_cn,
        t95 = fc$D_t95,
        s = fc$D_s)
  
) -> fcorr

names(fcorr)[3:4] <- c("veh", "pol")
cov1 <- fcorr[pol == "COV"]
cov1$pol <- "HC"
cov2 <- fcorr[pol == "COV"]
cov2$pol <- "NMHC"
no1 <- fcorr[pol == "NOx"]
no1$pol <- "NO"
no2 <- fcorr[pol == "NOx"]
no2$pol <- "NO2"

fcorr <- rbind(fcorr, cov1, cov2, no1, no2)

metadata$v_eea_old_fuel <- ifelse(metadata$v_eea_old %in% c("PC", "LCV", "Motorcycle"),
                                  paste0("LDV", metadata$fuel_eea_old),
                                  "HDV")

# Hot Exhaust ####

for (i in seq_along(metadata$vehicles)) {
  
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )
  
  veh <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
  # euro
  cate <- suppressWarnings(
    as.character(as.roman(gsub("Euro ", "", 
                               euro[[metadata$vehicles[i]]]))))
  cate[is.na(cate)] <- "PRE"
  
  for (j in seq_along(pol)) {
    cat(pol[j], " ")
    
    f_fcorr <- fcorr[veh == metadata$v_eea_old_fuel[i] &
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
    
    
    if(metadata$v_eea_old[i] %in% c("PC", "LCV", "Motorcycle")) {
      
      ef <- ef_ldv_speed(v = metadata$v_eea_old[i],
                         t = metadata$t_eea_old[i],
                         cc = metadata$cc_eea_old[i],
                         f = if(metadata$fuel_eea_old[i] == "LPG" & pol[j] == "PM") "G" else  metadata$fuel_eea_old[i],  
                         p = pol[j],
                         eu = cate,
                         speed = Speed(metadata$speed[i]),
                         fcorr = if(nrow(f_fcorr) == 0) rep(1, 8) else f_fcorr$value)[1:length(veh)]
      ef <- ef*efdet
      
      if(metadata$fuel_eea_old[i] == "LPG" & pol[j] == "PM") ef <- ef*0
      
    } else {
      ef <- ef_hdv_speed(v = metadata$v_eea_old[i],
                         t = metadata$t_eea_old[i],
                         g = metadata$cc_eea_old[i],
                         eu = cate,
                         gr = 0,
                         l = 0.5,
                         p = pol[j],
                         speed = Speed(metadata$speed[i]), 
                         fcorr = if(nrow(f_fcorr) == 0) rep(1, 8) else f_fcorr)
    }
    
    array_x <- emis_hot_td(
      veh = veh,
      lkm = mileage[[metadata$vehicles[i]]],
      ef = ef[, 1:ncol(veh)],
      fortran = TRUE,
      nt = nt,
      pro_month = pmonth[fuel == ifelse(metadata$vehicles[i] == "D", "D", "G")]$consumption_lt,
      verbose = verbose,
      params = list(
        veh = metadata$vehicles[i],
        size = metadata$size[i],
        fuel = metadata$fuel[i],
        pollutant = pol[j],
        type_emi = "Hot",
        subtype_emi = "Exhaust",
        baseyear = year
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
