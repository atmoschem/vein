suppressWarnings(file.remove("emi/DF_FC.csv"))

metadata_original <- metadata

metadata <- metadata[!metadata$fuel %in% remove_fuel, ]

# replacing Trucks_G to PC_G for FC estimation
metadata[metadata$vehicles %in% c("TRUCKS_MINI_G",
                                  "TRUCKS_LIGHT_G",
                                  "TRUCKS_MEDIUM_G",
                                  "TRUCKS_HEAVY_G"),
]$v_eea_2016  <- "PC"

metadata[metadata$vehicles %in% c("TRUCKS_MINI_G",
                                  "TRUCKS_LIGHT_G",
                                  "TRUCKS_MEDIUM_G",
                                  "TRUCKS_HEAVY_G"),
]$t_eea_2016  <- "4S"

metadata[metadata$vehicles %in% c("TRUCKS_MINI_G",
                                  "TRUCKS_LIGHT_G",
                                  "TRUCKS_MEDIUM_G",
                                  "TRUCKS_HEAVY_G"),
]$cc_eea_2016 <- ">2000"



# Hot Exhaust ####
cat("\n\nHot Running Fuel Consumption\n ")

for (i in seq_along(metadata$vehicles)) {
  
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
  # euro
  std1 <- std[[metadata$vehicles[i]]]
  
  
  if(metadata$v_eea_2016[i] %in% c("PC", "LCV", "Motorcycle")) {
    
    ef <- lapply(seq_along(std1), function(eu) {
      ef_ldv_speed(v = metadata$v_eea_2016[i],
                   t = metadata$t_eea_2016[i],
                   cc = metadata$cc_eea_2016[i],
                   f = metadata$fuel_eea_2016[i],   
                   p = "FC",
                   eu = std1[eu])
    })
    
  } else {
    
    ef <- lapply(seq_along(std1), function(eu) {
      ef_hdv_speed(v = metadata$v_eea_2016[i],
                   t = metadata$t_eea_2016[i],
                   g = metadata$cc_eea_2016[i],
                   eu = std1[eu],
                   gr = 0,
                   l = 0.5,
                   p = "FC")
    })
  }
  
  array_x <- emis(
    veh = x, 
    lkm = add_lkm(net$lkm), 
    ef = ef, 
    speed = as.data.frame(net[["speed"]]), 
    profile = 1,
    verbose = verbose
  )
  
  x_DF <- emis_post(
    arra = array_x,
    veh = metadata$vehicles[i],
    size = metadata$size[i],
    fuel = metadata$fuel[i],
    pollutant = "FC",
    type_emi = "Exhaust",
    by = "veh"
  )
  
  
  fwrite(x_DF, "emi/DF_FC.csv", append = TRUE)
}



# Cold Start ####
cat("\n\nCold Exhaust Fuel Consumption ")

metadata_cold <- metadata[metadata$fuel_eea_2016 %in% "G" &
                            metadata$v_eea_2016 %in% c("PC", "LCV"), ]

for (i in seq_along(metadata_cold$vehicles)) {
  cat(
    "\n", metadata_cold$vehicles[i],
    rep("", max(nchar(metadata_cold$vehicles) + 1) - nchar(metadata_cold$vehicles[i]))
  )
  
  x <- readRDS(paste0("veh/", metadata_cold$vehicles[i], ".rds"))
  
  # euro
  std1 <- std[[metadata$vehicles[i]]]
  
  ltrip <- add_lkm(metadata_cold$km_cycle[i])
  ta <- mean(met$Temperature)
  a <- cold_mileage(ltrip = ltrip, ta = celsius(ta))
  
  ef <- lapply(seq_along(std1), function(eu) {
    ef_ldv_speed(v = metadata_cold$v_eea_2016[i],
                 t = metadata_cold$t_eea_2016[i],
                 cc = metadata_cold$cc_eea_2016[i],
                 f = metadata_cold$fuel_eea_2016[i],   
                 p = "FC",
                 eu = std1[eu])
  })
  
  efcold <- lapply(seq_along(std1), function(eu) {
    ef_ldv_cold(ta = ta,
                cc = ifelse(metadata_cold$cc_eea_2016[i] == "<3.5",
                            ">2000",
                            metadata_cold$cc_eea_2016[i]),
                f = metadata_cold$fuel_eea_2016[i],   
                p = "FC",
                eu = std1[eu])
  })
  
  
  array_x <- emis_cold(
    veh = x, 
    lkm = add_lkm(net$lkm), 
    ef = ef, 
    efcold = efcold, 
    beta = matrix(a, ncol= ncol(x)), 
    speed = as.data.frame(net[["speed"]]), 
    agemax = 30, 
    simplify = T,
    profile = 1
  )
  
  x_DF <- emis_post(
    arra = array_x,
    veh = metadata_cold$vehicles[i],
    size = metadata_cold$size[i],
    fuel = metadata_cold$fuel[i],
    pollutant = "FC",
    type_emi = "Cold",
    by = "veh"
  )
  
  
  
  fwrite(x_DF, "emi/DF_FC.csv", append = TRUE)
}




switch(language,
       "portuguese" = message("\nArquivos em:"),
       "english" = message("\nFiles in:"),
       "spanish" = message("\nArchivos en:")
)

cat(paste0(getwd(), "/emi/*\n"))
