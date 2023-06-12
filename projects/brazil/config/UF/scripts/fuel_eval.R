year               <- as.numeric(substr(x = getwd(), start = nchar(getwd()) - 6, stop = nchar(getwd()) - 3))

suppressWarnings(file.remove("emi/FC.csv"))

# Exhaust ####
for(i in seq_along(metadata$vehicles)) {
  
  cat("\n", metadata$vehicles[i], 
      rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])))
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
  for(j in seq_along(pol)){

    cat(pol[j], " ")
    
    ef <- ef_cetesb(p = pol[j], 
                    veh = metadata$vehicles[i], 
                    year = year,
                    agemax = ncol(x), 
                    verbose = verbose)
    
    array_x <- emis_hot_td(veh = x, 
                           lkm = mileage[[metadata$vehicles[i]]], 
                           ef = ef, 
                           fortran = TRUE,
                           pro_month = as.numeric(pmonth[fuel == metadata$fuel[i]]$m3),
                           verbose = verbose,
                           params = list(veh = metadata$vehicles[i],
                                         size = metadata$size[i],
                                         fuel = metadata$fuel[i],
                                         pollutant = pol[j],
                                         type_emi = "Exhaust",
                                         subtype_emi = "Exhaust",
                                         baseyear = year))
    
    fwrite(array_x, "emi/FC.csv", append = TRUE)
  }
}

switch (language,
        "portuguese" = message("\nArquivos em:"),
        "english" = message("\nFiles in:"),
        "spanish" = message("\nArchivos en:"))

cat(paste0(getwd(), "/emi/*\n"))

# data.table ####
dt <- fread("emi/FC.csv")

dt$pollutant <- as.character(dt$pollutant)
dt$g <- units::set_units(dt$emissions, "g")
dt$t <- units::set_units(dt$g, t)

dt0 <- dt[pollutant == "FC", 
          round(sum(t), 2), 
          by = .(fuel)]
data.table::setkey(dt0, "fuel")

names(dt0)[2] <- "estimation_t" 
dtf <- merge(dt0, fuel, by = "fuel")
dtf$estimation_consumption <- dtf$estimation_t/dtf$consumption_t
print(dtf[, c("fuel", "estimation_t", "consumption_t", "estimation_consumption")])
fwrite(dtf, "config/kfuel.csv")

# 2) Traffic ####
net                <- readRDS("network/net.rds")
metadata           <- readRDS("config/metadata.rds")
categories         <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh                <- readRDS("config/fleet_age.rds")
k_D                <- 1/dtf[dtf$fuel == "D", ]$estimation_consumption
k_E                <- 1/dtf[dtf$fuel == "E", ]$estimation_consumption
k_G                <- 1/dtf[dtf$fuel == "G", ]$estimation_consumption
verbose            <- FALSE
theme              <- "black" # dark clean ink
source("scripts/trafficfuel.R", encoding = "UTF-8")

# Re estimating FC ####
suppressWarnings(file.remove("emi/FC.csv"))

switch (language,
        "portuguese" = message("\nEstimando consumo de combustivel:"),
        "english" = message("\nEstimating fuel consumption"),
        "spanish" = message("\nEstimando consumo de combustible"))
# Exhaust ####
for(i in seq_along(metadata$vehicles)) {
  
  cat("\n", metadata$vehicles[i], 
      rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])))
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
  for(j in seq_along(pol)){
    
    cat(pol[j], " ")
    
    ef <- ef_cetesb(p = pol[j], 
                    veh = metadata$vehicles[i], 
                    year = year,
                    agemax = ncol(x), 
                    verbose = verbose)
    
    array_x <- emis_hot_td(veh = x, 
                           lkm = mileage[[metadata$vehicles[i]]], 
                           ef = ef, 
                           fortran = TRUE,
                           pro_month = as.numeric(pmonth[fuel == metadata$fuel[i]]$m3),
                           verbose = verbose,
                           params = list(veh = metadata$vehicles[i],
                                         size = metadata$size[i],
                                         fuel = metadata$fuel[i],
                                         pollutant = pol[j],
                                         type_emi = "Exhaust",
                                         subtype_emi = "Exhaust",
                                         baseyear = year))
    
    fwrite(array_x, "emi/FC.csv", append = TRUE)
  }
}

# data.table ####
dt <- fread("emi/FC.csv")
fuel <- readRDS("config/fuel.rds")

dt$pollutant <- as.character(dt$pollutant)
dt$g <- units::set_units(dt$emissions, "g")
dt$t <- units::set_units(dt$g, t)

dt0 <- dt[pollutant == "FC", 
          round(sum(t), 2), 
          by = .(fuel)]
data.table::setkey(dt0, "fuel")

names(dt0)[2] <- "estimation_t" 
dtf <- merge(dt0, fuel, by = "fuel")
dtf$estimation_consumption <- dtf$estimation_t/dtf$consumption_t
print(dtf[, c("fuel", "estimation_t", "consumption_t", "estimation_consumption")])

switch (language,
        "portuguese" = message("Limpando..."),
        "english" = message("Cleaning..."),
        "spanish" = message("Limpiando..."))

