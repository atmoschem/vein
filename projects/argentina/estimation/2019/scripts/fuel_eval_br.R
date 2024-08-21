
year   <- as.numeric(substr(x = getwd(), 
                            start = nchar(getwd()) - 3, 
                            stop = nchar(getwd()) ))

year_selected <- year

suppressWarnings(file.remove("emi/FC_INITIAL.csv"))

reg <- unique(pmonth$region)

# Exhaust ####
for(k in seq_along(reg)) {
  
  cat(reg[k],  " ")
  
  
  for(i in seq_along(metadata$vehicles)) {
    
    # cat("\n", metadata$vehicles[i],
    #     rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])))
    
    
    x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
    
    x[is.na(x)] <- 0
    
    x <- x[region == reg[k], ]
    
    x$region <- NULL
    
    setDF(x)
    
    dm <- pmonth[region == reg[k] &
                   fuel == metadata$fuel[i]]$consumption_t
    
    for(j in seq_along(pol)){
      
      # cat(pol[j])
      
      
      ef <- ef_cetesb(p = pol[j],
                      veh = metadata$vehicles[i],
                      year = year_selected,
                      agemax = ncol(x),
                      verbose = verbose)
      
      array_x <- emis_hot_td(
        veh = x,
        lkm = mileage[[metadata$vehicles[i]]][1:metadata$maxage[i]],
        ef = ef,
        fortran = TRUE,
        pro_month = dm,
        verbose = verbose,
        params = list(veh = metadata$vehicles[i],
                      size = metadata$size[i],
                      fuel = metadata$fuel[i],
                      pollutant = pol[j],
                      type_emi = "Exhaust",
                      subtype_emi = "Exhaust",
                      baseyear = year_selected))
      
      array_x$region <- reg[k]
      
      fwrite(array_x, "emi/FC_INITIAL.csv", append = TRUE)
    }
  }
}

cat("\n")

switch (language,
        "portuguese" = message("\nArquivos em:"),
        "english" = message("\nFiles in:"),
        "spanish" = message("\nArchivos en:"))

cat(paste0(getwd(), "/emi/*\n"))



# data.table ####
dt <- fread("emi/FC_INITIAL.csv")

dt$pollutant <- as.character(dt$pollutant)
dt$g <- units::set_units(dt$emissions, "g")
dt$t <- units::set_units(dt$g, t)

dt0 <- dt[pollutant == "FC", 
          round(sum(t, na.rm = T), 2), 
          by = .(fuel, 
                 region)]

# data.table::setkey(dt0, c("fuel", "region"))

names(dt0)[3] <- "estimation_t" 
dtf <- merge(dt0, fuel, by = c("fuel", "region"))
dtf$estimation_consumption <- dtf$estimation_t/dtf$consumption_t
print(dtf[, c("region", 
              "fuel", 
              "estimation_t",
              "consumption_t", 
              "estimation_consumption")])

dtf$kfinal <- as.numeric(1/dtf$estimation_consumption)

dtf$kfinal[is.na(dtf$kfinal)] <- 1

dtf$kfinal[is.infinite(dtf$kfinal)] <- 1
fwrite(dtf, "config/kfuel.csv")

# 2) Traffic ####
language <- "english" # spanish portuguese
metadata <- readRDS("config/metadata.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
verbose <- FALSE
theme <- "black" # dark clean ink
survival   <- TRUE
fuel <- dtf
source("scripts/trafficfuel.R", 
       encoding = "UTF-8",
       echo = TRUE)

# Re estimating FC ####
suppressWarnings(file.remove("emi/FC_ADJUSTED.csv"))

switch (language,
        "portuguese" = message("\nEstimando consumo de combustivel:"),
        "english" = message("\nEstimating fuel consumption"),
        "spanish" = message("\nEstimando consumo de combustible"))


# Exhaust ####
for(k in seq_along(reg)) {
  
  cat(reg[k],  " ")
  
  
  for(i in seq_along(metadata$vehicles)) {
    
    # cat("\n", metadata$vehicles[i],
    #     rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])))
    
    
    x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
    
    x[is.na(x)] <- 0
    x <- x[region == reg[k], ]
    
    x$region <- NULL
    
    setDF(x)
    
    dm <- pmonth[region == reg[k] &
                   fuel == metadata$fuel[i]]$consumption_t
    
    for(j in seq_along(pol)){
      
      # cat(pol[j])
      
      
      ef <- ef_cetesb(p = pol[j],
                      veh = metadata$vehicles[i],
                      year = year_selected,
                      agemax = ncol(x),
                      verbose = verbose)
      
      array_x <- emis_hot_td(
        veh = x,
        lkm = mileage[[metadata$vehicles[i]]][1:metadata$maxage[i]],
        ef = ef,
        fortran = TRUE,
        pro_month = dm,
        verbose = verbose,
        params = list(veh = metadata$vehicles[i],
                      size = metadata$size[i],
                      fuel = metadata$fuel[i],
                      pollutant = pol[j],
                      type_emi = "Exhaust",
                      subtype_emi = "Exhaust",
                      baseyear = year_selected))
      
      array_x$region <- reg[k]
      
      fwrite(array_x, "emi/FC_ADJUSTED.csv", append = TRUE)
    }
  }
}

cat("\n")

# data.table ####
dt <- fread("emi/FC_ADJUSTED.csv")
fuel <- readRDS("config/fuel.rds")

dt$pollutant <- as.character(dt$pollutant)
dt$g <- units::set_units(dt$emissions, "g")
dt$t <- units::set_units(dt$g, t)

dt0 <- dt[pollutant == "FC", 
          round(sum(t), 2), 
          by = .(fuel, 
                 region)]

# data.table::setkey(dt0, c("fuel", "region"))

names(dt0)[3] <- "estimation_t" 
dtf <- merge(dt0, fuel, by = c("fuel", "region"))

dtf$estimation_consumption <- dtf$estimation_t/dtf$consumption_t
print(dtf[, c("region", 
              "fuel",
              "estimation_t",
              "consumption_t",
              "estimation_consumption")])

switch (language,
        "portuguese" = message("Limpando..."),
        "english" = message("Cleaning..."),
        "spanish" = message("Limpiando..."))


