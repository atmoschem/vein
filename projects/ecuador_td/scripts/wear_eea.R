suppressWarnings(file.remove("emi/wear.csv"))

setDT(pmonth)

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


pol <- c("TSP", "PM10", "PM2.5", "PM1",  "PM0.1")

wear <- c("tyre", "break", "road")

pol  <- expand.grid(pol = pol, wear = wear, stringsAsFactors = F)

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
                  speed = metadata$speed[i])
    
    array_x <- emis_hot_td(
      veh = veh,
      lkm = mileage[[metadata$vehicles[i]]],
      ef = if(pol[j, ]$wear == "road") rep(ef, ncol(veh)) else rep(ef$V1, ncol(veh)),
      fortran = TRUE,
      nt = nt,
      pro_month = pmonth[fuel == ifelse(metadata$vehicles[i] == "D", "D", "G")]$consumption_lt,
      verbose = verbose,
      params = list(
        veh = metadata$vehicles[i],
        size = metadata$size[i],
        fuel = metadata$fuel[i],
        pollutant = ifelse(pol[j, ]$pol == "PM2.5", "PM", pol[j, ]$pol),
        type_emi = "Wear",
        subtype_emi = pol[j, ]$wear,
        baseyear = year
      )
    )
    
    fwrite(array_x, "emi/wear.csv", append = TRUE)
  }
}


switch(language,
       "portuguese" = message("\nEmissões em: /emi/wear.csv:"),
       "english" = message("\nEmissions in: /emi/wear.csv"),
       "spanish" = message("\nEmisiones en: /emi/wear.csv")
)
