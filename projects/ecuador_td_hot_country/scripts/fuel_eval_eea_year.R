file.remove("emi/FC.csv")

metadata_original <- metadata

metadata <- metadata[metadata$fuel != "ELEC", ]
# para vehiculos hibridos solo hay euro 4, entonces se asumio para 
# esa categoria en inventory.xlsx
# es importante cerar el numero de vehiculos antes que estos entraran
# en circulacion

# Exhaust ####
for (i in seq_along(metadata$vehicles)) {
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )

  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))

  # euro
  cate <- as.character(as.roman(gsub("Euro ", "", euro[[metadata$vehicles[i]]])))
  cate[is.na(cate)] <- "PRE"
  
  for (j in seq_along(pol)) {
    cat(pol[j], " ")

    if(metadata$v_eea_old[i] %in% c("PC", "LCV", "Motorcycle")) {

      rbindlist(lapply(seq_along(net[[id_region]]), function(k) {
        
      ef_ldv_speed(v = metadata$v_eea_old[i],
                         t = metadata$t_eea_old[i],
                         cc = metadata$cc_eea_old[i],
                         f = metadata$fuel_eea_old[i],   
                         p = "FC",
                         eu = cate,
                         speed = Speed(rep(metadata$speed[i], 12)))
      }))-> ef
      
    } else {
      rbindlist(lapply(seq_along(net[[id_region]]), function(k) {
        
      ef <- ef_hdv_speed(v = metadata$v_eea_old[i],
                         t = metadata$t_eea_old[i],
                         g = metadata$cc_eea_old[i],
                         eu = cate,
                         gr = 0,
                         l = 0.5,
                         p = "FC",
                         speed = Speed(rep(metadata$speed[i], 12)))
    
    }))-> ef
    }  
    nrow(x) ==  nrow(ef)
    
    ef$speed <- NULL
    
    array_x <- emis_hot_td(
      veh = x,
      lkm = mileage[[metadata$vehicles[i]]],
      ef = ef[, 1:ncol(x)],
      fortran = TRUE,
      nt = check_nt() / 2,
      # pro_month = pmonth[[metadata$vehicles[i]]],
      verbose = verbose,
      params = list(
        veh = metadata$vehicles[i],
        size = metadata$size[i],
        fuel = metadata$fuel[i],
        pollutant = pol[j],
        type_emi = "Exhaust",
        subtype_emi = "Exhaust",
        baseyear = year,
        month = 1:12
      )
    )
    array_x$region <- rep(net[[id_region]], each = 12)

    fwrite(array_x, "emi/FC.csv", append = TRUE)
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
dt$g <- units::set_units(dt$emissions, "g")
dt$t <- units::set_units(dt$g, t)

dt$fuel_month_region <- paste0(dt$fuel, "_", dt$month, "_", dt$region)

dt0 <- dt[pollutant == "FC",
  round(sum(t), 2),
  by = .(fuel_month_region)
]
data.table::setkey(dt0, "fuel_month_region")

fuel$fuel_month_region <- paste0(fuel$fuel, "_", fuel$Month,"_", fuel$region)

names(dt0)[2] <- "estimation_t"

dtf <- merge(dt0, fuel, by = "fuel_month_region")

setorderv(dtf, cols = c("region", "fuel", "Month"))

dtf$density_tm3 <- units::set_units(dtf$density_tm3, "t/m^3")
dtf$consumption_lt <- units::set_units(dtf$consumption_lt, "l")
dtf$consumption_m3 <- units::set_units(dtf$consumption_lt, "m^3")
dtf$consumption_t <- dtf$consumption_m3 * dtf$density_tm3
dtf$estimation_consumption <- dtf$estimation_t / dtf$consumption_t
print(dtf[, c("region", "Month", "fuel", "estimation_t", "consumption_t", "estimation_consumption")])

# calibrate k ####
k_D <- as.numeric(1/dtf[fuel == "D"]$estimation_consumption)
k_G <- as.numeric(1/dtf[fuel == "G"]$estimation_consumption)
language <- "spanish" # english spanish portuguese
net <- readRDS("network/net.rds")
metadata <- readRDS("config/metadata.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
verbose <- FALSE
year <- 2019
theme <- "black" # dark clean ink

source("scripts/traffic_year.R")

# re estimate fc ####
file.remove("emi/FC.csv")

metadata_original <- metadata

metadata <- metadata[metadata$fuel != "ELEC", ]
# para vehiculos hibridos solo hay euro 4, entonces se asumio para 
# esa categoria en inventory.xlsx
# es importante cerar el numero de vehiculos antes que estos entraran
# en circulacion


# Exhaust ####
for (i in seq_along(metadata$vehicles)) {
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
  # euro
  cate <- as.character(as.roman(gsub("Euro ", "", euro[[metadata$vehicles[i]]])))
  cate[is.na(cate)] <- "PRE"
  
  for (j in seq_along(pol)) {
    cat(pol[j], " ")
    
    if(metadata$v_eea_old[i] %in% c("PC", "LCV", "Motorcycle")) {
      
      rbindlist(lapply(seq_along(net[[id_region]]), function(k) {
        
        ef_ldv_speed(v = metadata$v_eea_old[i],
                     t = metadata$t_eea_old[i],
                     cc = metadata$cc_eea_old[i],
                     f = metadata$fuel_eea_old[i],   
                     p = "FC",
                     eu = cate,
                     speed = Speed(rep(metadata$speed[i], 12)))
      }))-> ef
      
    } else {
      rbindlist(lapply(seq_along(net[[id_region]]), function(k) {
        
        ef <- ef_hdv_speed(v = metadata$v_eea_old[i],
                           t = metadata$t_eea_old[i],
                           g = metadata$cc_eea_old[i],
                           eu = cate,
                           gr = 0,
                           l = 0.5,
                           p = "FC",
                           speed = Speed(rep(metadata$speed[i], 12)))
        
      }))-> ef
    }  
    nrow(x) ==  nrow(ef)
    
    ef$speed <- NULL
    
    array_x <- emis_hot_td(
      veh = x,
      lkm = mileage[[metadata$vehicles[i]]],
      ef = ef[, 1:ncol(x)],
      fortran = TRUE,
      nt = check_nt() / 2,
      # pro_month = pmonth[[metadata$vehicles[i]]],
      verbose = verbose,
      params = list(
        veh = metadata$vehicles[i],
        size = metadata$size[i],
        fuel = metadata$fuel[i],
        pollutant = pol[j],
        type_emi = "Exhaust",
        subtype_emi = "Exhaust",
        baseyear = year,
        month = 1:12
      )
    )
    array_x$region <- rep(net[[id_region]], each = 12)
    
    fwrite(array_x, "emi/FC.csv", append = TRUE)
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
dt$g <- units::set_units(dt$emissions, "g")
dt$t <- units::set_units(dt$g, t)

dt$fuel_month_region <- paste0(dt$fuel, "_", dt$month, "_", dt$region)

dt0 <- dt[pollutant == "FC",
          round(sum(t), 2),
          by = .(fuel_month_region)
]
data.table::setkey(dt0, "fuel_month_region")

fuel$fuel_month_region <- paste0(fuel$fuel, "_", fuel$Month,"_", fuel$region)

names(dt0)[2] <- "estimation_t"

dtf <- merge(dt0, fuel, by = "fuel_month_region")

setorderv(dtf, cols = c("region", "fuel", "Month"))

dtf$density_tm3 <- units::set_units(dtf$density_tm3, "t/m^3")
dtf$consumption_lt <- units::set_units(dtf$consumption_lt, "l")
dtf$consumption_m3 <- units::set_units(dtf$consumption_lt, "m^3")
dtf$consumption_t <- dtf$consumption_m3 * dtf$density_tm3
dtf$estimation_consumption <- dtf$estimation_t / dtf$consumption_t
print(dtf[, c("region", "Month", "fuel", "estimation_t", "consumption_t", "estimation_consumption")])

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