suppressWarnings(file.remove("emi/wear.csv"))

# Escapamento ####
switch(language,
       "portuguese" = cat("Estimando emissões Wear\n"),
       "english" = cat("Estimating emissions Wear\n"),
       "spanish" = cat("Estimando emisiones Wear\n")
)
metadata_original <- metadata

metadata$v_eea_old <- ifelse(metadata$v_eea_old %in% c("PC", "LCV", "Motorcycle"),
                             metadata$v_eea_old,
                             "HDV")

metadata$v_eea_old <- ifelse(metadata$v_eea_old %in% c("Motorcycle"),
                             "2W",
                             metadata$v_eea_old)


# pol <- c("TSP", "PM10", "PM2.5", "PM1",  "PM0.1")
# 
# wear <- c("tyre", "break", "road")
# It seems I have a problem wuth road

pol <- c("PM10", "PM2.5")

wear <- c("tyre", "break")


pol  <- expand.grid(pol = pol, wear = wear, stringsAsFactors = F)

ppol <- pol
ppol$pol <- gsub("PM2.5", "PM", ppol$pol)

ppol
# Hot Exhaust ####

for (i in seq_along(metadata$vehicles)) {
  
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )
  
  veh <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
  for (j in 1:nrow(pol)) {
    
    cat( pol[j, ]$pol, " ")
    
    
    ef <- ef_wear(wear= pol[j, ]$wear, 
                  type = metadata$v_eea_old[i],
                  pol = pol[j, ]$pol, 
                  speed = speed)
    
    lef  <- rbindlist(lapply(1:ncol(ef), function(i) {
      data.table(replicate(ncol(veh), ef[[i]]))
    }))
    
    array_x <- emis_long(
      x = veh,
      lkm = net$lkm,
      ef = lef,
      tfs = tfs[[metadata$vehicles[i]]], 
      speed = speed,
      array = T,
      verbose = verbose
    )
    
    x_DF <- emis_post(
      arra = array_x,
      veh = metadata$vehicles[i],
      size = metadata$size[i],
      fuel = metadata$fuel[i],
      pollutant = ppol$pol[j],
      type_emi = paste0(ppol$wear[j], " wear"),
      by = "veh"
    )
    
    fwrite(x_DF, "emi/wear.csv", append = TRUE)
    
    x_ST <- emis_post(
      arra = array_x,
      veh = metadata$vehicles[i],
      size = metadata$size[i],
      fuel = metadata$fuel[i],
      pollutant = ppol$pol[j],
      type_emi = paste0(ppol$wear[j], " wear"),
      by = "streets"
    )
    
    x_ST$fuel <- metadata$fuel[i]
    
    fwrite(x_ST, paste0("emi/", metadata$vehicles[i], "/WEAR_", ppol$pol[j], ".csv"), append = TRUE)
    
  }
}


switch(language,
       "portuguese" = message("\nEmissões em: /emi/wear.csv:"),
       "english" = message("\nEmissions in: /emi/wear.csv"),
       "spanish" = message("\nEmisiones en: /emi/wear.csv")
)
