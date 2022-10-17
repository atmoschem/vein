suppressWarnings(file.remove("emi/FC.csv"))

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
  
  for (j in seq_along(pol)) {
    
    cat(pol[j], " ")
    
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
      lkm = net$lkm, 
      ef = ef, 
      speed = speed, 
      profile = tfs[[metadata$vehicles[i]]],
      verbose = verbose
    )
    
    x_DF <- emis_post(
      arra = array_x,
      veh = metadata$vehicles[i],
      size = metadata$size[i],
      fuel = metadata$fuel[i],
      pollutant = pol[j],
      type_emi = "Exhaust",
      by = "veh"
    )
    
    
    fwrite(x_DF, "emi/FC.csv", append = TRUE)
  }
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
  
  for (j in seq_along(pol)) {
    cat(pol[j], " ")
    
    
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
      lkm = net$lkm, 
      ef = ef, 
      efcold = efcold, 
      beta = matrix(a, ncol= ncol(x)), 
      speed = speed, 
      agemax = 30, 
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
    
    
    
    fwrite(x_DF, "emi/FC.csv", append = TRUE)
  }
}



switch(language,
       "portuguese" = message("\nArquivos em:"),
       "english" = message("\nFiles in:"),
       "spanish" = message("\nArchivos en:")
)

cat(paste0(getwd(), "/emi/*\n"))

# data.table ####
dt <- fread("emi/FC.csv")

dt$pollutant <- as.character(dt$pollutant)
dt$g <- units::set_units(dt$g, "g")
dt$t <- units::set_units(dt$g, t)

dt0 <- dt[pollutant == "FC",
          round(sum(t) * factor_emi, 2),
          by = .(fuel)
]
data.table::setkey(dt0, "fuel")

names(dt0)[2] <- "estimation_t"

dtf <- merge(dt0, fuel, by = "fuel")

setorderv(dtf, cols = c("fuel"))

dtf$density_tm3 <- units::set_units(dtf$density_tm3, "t/m^3")
dtf$consumption_lt <- units::set_units(dtf$consumption_lt, "l")
dtf$consumption_m3 <- units::set_units(dtf$consumption_lt, "m^3")
dtf$consumption_t <- dtf$consumption_m3 * dtf$density_tm3
dtf$estimation_consumption <- dtf$estimation_t / dtf$consumption_t
print(dtf[, c("fuel", "estimation_t", "consumption_t", "estimation_consumption")])

# calibrate k ####
k_D <- as.numeric(1/dtf[fuel == "D"]$estimation_consumption)
k_G <- as.numeric(1/dtf[fuel == "G"]$estimation_consumption)
k_CNG <- as.numeric(1/dtf[fuel == "G"]$estimation_consumption)

# traffic ####
language <- "english" # portuguese english spanish
net <- readRDS("network/net.rds")
metadata <- readRDS("config/metadata.rds")
veh <- readRDS("config/fleet_age.rds")
verbose <- FALSE
year <- 2016
theme <- "black" # dark clean ink
  source("scripts/traffic.R")


# re estimate fc ####
suppressWarnings(file.remove("emi/FC.csv"))

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
  
  for (j in seq_along(pol)) {
    
    cat(pol[j], " ")
    
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
      lkm = net$lkm, 
      ef = ef, 
      speed = speed, 
      profile = tfs[[metadata$vehicles[i]]],
      verbose = verbose
    )
    
    x_DF <- emis_post(
      arra = array_x,
      veh = metadata$vehicles[i],
      size = metadata$size[i],
      fuel = metadata$fuel[i],
      pollutant = pol[j],
      type_emi = "Exhaust",
      by = "veh"
    )
    
    
    fwrite(x_DF, "emi/FC.csv", append = TRUE)
  }
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
  
  for (j in seq_along(pol)) {
    cat(pol[j], " ")
    
    
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
      lkm = net$lkm, 
      ef = ef, 
      efcold = efcold, 
      beta = matrix(a, ncol= ncol(x)), 
      speed = speed, 
      agemax = 30, 
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
    
    
    
    fwrite(x_DF, "emi/FC.csv", append = TRUE)
  }
}

switch(language,
       "portuguese" = message("\nArquivos em:"),
       "english" = message("\nFiles in:"),
       "spanish" = message("\nArchivos en:")
)

cat(paste0(getwd(), "/emi/*\n"))

# data.table ####
dt <- fread("emi/FC.csv")

dt$pollutant <- as.character(dt$pollutant)
dt$g <- units::set_units(dt$g, "g")
dt$t <- units::set_units(dt$g, t)

dt0 <- dt[pollutant == "FC",
          sum(t) * factor_emi,
          by = .(fuel)
]
data.table::setkey(dt0, "fuel")

names(dt0)[2] <- "estimation_t"

dtf <- merge(dt0, fuel, by = "fuel")

setorderv(dtf, cols = c("fuel"))

dtf$density_tm3 <- units::set_units(dtf$density_tm3, "t/m^3")
dtf$consumption_lt <- units::set_units(dtf$consumption_lt, "l")
dtf$consumption_m3 <- units::set_units(dtf$consumption_lt, "m^3")
dtf$consumption_t <- dtf$consumption_m3 * dtf$density_tm3
dtf$estimation_consumption <- dtf$estimation_t / dtf$consumption_t
print(dtf[, c("fuel", "estimation_t", "consumption_t", "estimation_consumption")])

switch(language,
       "portuguese" = message("Limpando..."),
       "english" = message("Cleaning..."),
       "spanish" = message("Limpiando...")
)
# suppressWarnings(rm(i, j, pol, dt, dt0, dtf, factor_emi, fuel))
# suppressWarnings(rm(
#   kPC, kPC_G, kPC_E, kPC_FG, kPC_FE,
#   kLCV, kLCV_G, kLCV_E, kLCV_FG, kLCV_FE, kLCV_D,
#   kTRUCKS, kTRUCKS_SL_D, kTRUCKS_L_D, kTRUCKS_M_D, kTRUCKS_SH_D, kTRUCKS_H_D,
#   kBUS, kBUS_URBAN_D, kBUS_MICRO_D, kBUS_COACH_D,
#   kMC, kMC_150_G, kMC_150_500_G, kMC_500_G,
#   kMC_150_FG, kMC_150_500_FG, kMC_500_FG,
#   kMC_150_FE, kMC_150_500_FE, kMC_500_FE,
#   l_PC, l_LCV, l_TRUCKS, l_BUS, l_MC,
#   i, arquivos, cores, df, f, ff,
#   n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC,
#   na, nveh, p, tit, tit2, veiculos, x, kf,
#   k_G, k_D, k_E,
# ))
ls()
invisible(gc())