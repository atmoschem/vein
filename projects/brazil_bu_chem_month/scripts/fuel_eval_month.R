# calibração combustivel http://dadosenergeticos.energia.sp.gov.br/portalcev2/intranet/PetroGas/index.html
suppressWarnings(file.remove("emi/fuel.csv"))

# Second estimation fuel ####
# tfs
tfs <- as.data.frame(tfs)

# Escapamento ####
for (i in seq_along(metadata$vehicles)) {
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
  for (j in seq_along(pol)) {
    cat(pol[j], " ")
    
    ef <- ef_cetesb(
      p = pol[j],
      veh = metadata$vehicles[i],
      year = year,
      agemax = nrow(x),
      verbose = verbose,
      scale = scale
    )
    
    array_x <- emis(
      veh = x,
      lkm = lkm,
      ef = ef,
      profile = tfs[[metadata$vehicles[i]]],
      fortran = TRUE,
      nt = check_nt() / 2,
      simplify = TRUE,
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
    
    
    fwrite(x_DF, "emi/fuel.csv", append = T)
  }
  rm(array_x, ef, x, x_DF)
}
# 
# switch(language,
#   "portuguese" = message("\nArquivos em: /emi/*:"),
#   "english" = message("\nFiles in: /emi/*"),
#   "spanish" = message("\nArchivos en: /emi/*")
# )

# data.table ####

dt <- fread("emi/fuel.csv")
# dt <- data.table::rbindlist(
#   lapply(seq_along(pol), function(i) {
#     pols <- ifelse(pol[i] == "HC", "_HC", pol[i])
#     emis_merge(pols, what = "DF.rds", FALSE, verbose = FALSE)
#   })
# )
dt$pollutant <- as.character(dt$pollutant)
dt$g <- units::set_units(dt$g, g)
dt$t <- units::set_units(dt$g, t)

dt0 <- dt[pollutant == "FC", 
          round(sum(t) * factor_emi, 2), 
          by = .(fuel)]

data.table::setkey(dt0, "fuel")

names(dt0)[2] <- "estimation_t"

dtf <- dt0[fuel]
dtf$density_tm3 <- units::set_units(dtf$density_tm3, "t/m^3")
dtf$consumption_lt <- units::set_units(dtf$consumption_lt, "l")
dtf$consumption_m3 <- units::set_units(dtf$consumption_lt, "m^3")
dtf$consumption_t <- dtf$consumption_m3 * dtf$density_tm3
dtf$estimation_consumption <- dtf$estimation_t / dtf$consumption_t
print(dtf)

# factors ####
k_D <- 1/as.numeric(dtf[fuel == "D"]$estimation_consumption)
k_E <- 1/as.numeric(dtf[fuel == "E"]$estimation_consumption)
k_G <- 1/as.numeric(dtf[fuel == "G"]$estimation_consumption)

# traffic again ####

# 2) Traffic ####
language <- "portuguese" # english spanish
net <- readRDS("network/net.rds")
metadata <- readRDS("config/metadata.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
verbose <- FALSE
year <- 2018
theme <- "black" # dark clean ink
source("scripts/traffic.R", encoding = "UTF-8")

# Second estimation fuel ####
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
met <- readRDS("config/met.rds")
net <- readRDS("network/net.rds")
lkm <- net$lkm
scale <- "tunnel"
verbose <- FALSE
year <- 2018
month <- 6

# Fuel eval
language <- "portuguese" # english spanish
fuel <- readRDS("config/fuel.rds")
pol <- "FC"
# factor_emi <- dmonth(year, month) / (nrow(tfs) / 24)

# tfs
suppressWarnings(file.remove("emi/fuel.csv"))
tfs <- as.data.frame(tfs)

# Escapamento ####
for (i in seq_along(metadata$vehicles)) {
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
  for (j in seq_along(pol)) {
    cat(pol[j], " ")
    
    ef <- ef_cetesb(
      p = pol[j],
      veh = metadata$vehicles[i],
      year = year,
      agemax = nrow(x),
      verbose = verbose,
      scale = scale
    )
    
    array_x <- emis(
      veh = x,
      lkm = lkm,
      ef = ef,
      profile = tfs[[metadata$vehicles[i]]],
      fortran = TRUE,
      nt = check_nt() / 2,
      simplify = TRUE,
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
    
    
    fwrite(x_DF, "emi/fuel.csv", append = T)
  }
  rm(array_x, ef, x, x_DF)
}
# 
switch(language,
       "portuguese" = message("\nArquivos em: /emi/*:"),
       "english" = message("\nFiles in: /emi/*"),
       "spanish" = message("\nArchivos en: /emi/*")
)

# data.table ####

dt <- fread("emi/fuel.csv")
dt$pollutant <- as.character(dt$pollutant)
dt$g <- units::set_units(dt$g, g)
dt$t <- units::set_units(dt$g, t)

dt0 <- dt[pollutant == "FC", 
          round(sum(t) * factor_emi, 2), 
          by = .(fuel)]

data.table::setkey(dt0, "fuel")

names(dt0)[2] <- "estimation_t"

dtf <- dt0[fuel]
dtf$density_tm3 <- units::set_units(dtf$density_tm3, "t/m^3")
dtf$consumption_lt <- units::set_units(dtf$consumption_lt, "l")
dtf$consumption_m3 <- units::set_units(dtf$consumption_lt, "m^3")
dtf$consumption_t <- dtf$consumption_m3 * dtf$density_tm3
dtf$estimation_consumption <- dtf$estimation_t / dtf$consumption_t
print(dtf)


switch(language,
       "portuguese" = message("Limpando..."),
       "english" = message("Cleaning..."),
       "spanish" = message("Limpiando...")
)

suppressWarnings(rm(i, j, pol, dt, dt0, dtf, factor_emi, fuel))

ls()
invisible(gc())