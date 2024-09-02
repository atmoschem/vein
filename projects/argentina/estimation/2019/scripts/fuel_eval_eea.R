
year   <- as.numeric(substr(x = getwd(),
                            start = nchar(getwd()) - 3,
                            stop = nchar(getwd()) ))

year_selected <- year

suppressWarnings(file.remove("emi/FC_INITIAL.csv"))

metadata_original <- metadata

metadata <- metadata[metadata$fuel != "ELEC", ]
# para vehiculos hibridos solo hay euro 4, entonces se asumio para
# esa categoria en inventory.xlsx
# es importante cerar el numero de vehiculos antes que estos entraran
# en circulacion

setDT(pmonth)

reg <- unique(pmonth$region)

cat("\nHot Running Fuel Consumption\n")

# 1 Exhaust ####
for(k in seq_along(reg)) {

  cat("\n", reg[k],  "\n")


  for(i in seq_along(metadata$vehicles)) {

    # cat("\n", metadata$vehicles[i],
    #     rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])))


    x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))

    x[is.na(x)] <- 0

    x <- x[region == reg[k], ]

    x$region <- NULL

    setDF(x)

      # euro
  cate <- suppressWarnings(
    as.character(as.roman(gsub("Euro ", "",
                               euro[[metadata$vehicles[i]]]))))
  cate[is.na(cate)] <- "PRE"

    dm <- pmonth[region == reg[k] &
                   fuel == metadata$fuel[i]]$consumption_t

    for(j in seq_along(pol)){

      # cat(pol[j])

      if(metadata$v_eea_old[i] %in% c("PC", "LCV", "Motorcycle")) {


        ef <- ef_ldv_speed(v = metadata$v_eea_old[i],
                           t = metadata$t_eea_old[i],
                           cc = metadata$cc_eea_old[i],
                           f = metadata$fuel_eea_old[i],
                           p = pol,
                           eu = cate,
                           speed = Speed(metadata$speed[i]))

      } else {
        ef <- ef_hdv_speed(v = metadata$v_eea_old[i],
                           t = metadata$t_eea_old[i],
                           g = metadata$cc_eea_old[i],
                           eu = cate,
                           gr = 0,
                           l = 0.5,
                           p = pol,
                           speed = Speed(metadata$speed[i]))
      }

      nrow(x) ==  nrow(ef)

      ef$speed <- NULL



      array_x <- emis_hot_td(
        veh = x,
        lkm = mileage[[metadata$vehicles[i]]][1:metadata$maxage[i]],
        ef = ef[1:metadata$maxage[i]],
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


# 2 Cold Start ####
cat("\nCold Exhaust Fuel Consumption\n")

metadata_cold <- metadata[metadata$fuel_eea_old %in% "G" &
                       metadata$v_eea_old %in% c("PC", "LCV"), ]


for(k in seq_along(reg)) {

  cat(reg[k],  "\n")


  for(i in seq_along(metadata_cold$vehicles)) {

    # cat("\n", metadata$vehicles[i],
    #     rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])))


    x <- readRDS(paste0("veh/", metadata_cold$vehicles[i], ".rds"))

    x[is.na(x)] <- 0

    x <- x[region == reg[k], ]

    x$region <- NULL

    setDF(x)

    # euro
    cate <- suppressWarnings(
      as.character(as.roman(gsub("Euro ", "",
                                 euro[[metadata_cold$vehicles[i]]]))))
    cate[is.na(cate)] <- "PRE"

    dm <- pmonth[region == reg[k] &
                   fuel == metadata_cold$fuel[i]]$consumption_t


    ta <- met[capitals == unique(capitals[k])]$Temperature

        for(j in seq_along(pol)){

      # cat(pol[j])

      ltrip <- add_lkm(metadata_cold$km_cycle[i])

      a <- cold_mileage(ltrip = ltrip, ta = celsius(ta))

      (ef <- ef_ldv_speed(v = metadata_cold$v_eea_old[i],
                          t = metadata_cold$t_eea_old[i],
                          cc = metadata_cold$cc_eea_old[i],
                          f = metadata_cold$fuel_eea_old[i],
                          p = pol,
                          eu = cate,
                          speed = Speed(metadata_cold$speed[i])))

      (efcold <- ef_ldv_cold(ta = matrix(ta, nrow = 1),
                             cc = ifelse(metadata_cold$cc_eea_old[i] == "<3.5",
                                         ">2000",
                                         metadata_cold$cc_eea_old[i]),
                             f = metadata_cold$fuel_eea_old[i],
                             p = pol,
                             eu = cate,
                             speed = Speed(metadata_cold$speed[i])))

      nrow(x) ==  nrow(ef)

      ef$speed <- NULL

      array_x <- emis_cold_td(
        veh = x,
        lkm = mileage[[metadata_cold$vehicles[i]]],
        ef = ef[, 1:ncol(x)],
        efcold = efcold[, 1:ncol(x)],
        fortran = TRUE,
        beta = matrix(a, nrow = 1),
        nt = vein::check_nt()/2,
        pro_month = dm,
        verbose = verbose,
        params = list(
          veh = metadata_cold$vehicles[i],
          size = metadata_cold$size[i],
          fuel = metadata_cold$fuel[i],
          pollutant = pol[j],
          type_emi = "Cold",
          subtype_emi = "Exhaust",
          baseyear = year_selected,
          month = rep(1:12, each = ncol(x))
        )
      )

      array_x$region <- reg[k]

      fwrite(array_x, "emi/FC_INITIAL.csv", append = TRUE)

    }
  }
}


switch(language,
       "portuguese" = message("\nArquivos em:"),
       "english" = message("\nFiles in:"),
       "spanish" = message("\nArchivos en:")
)

cat(paste0(getwd(), "/emi/*\n"))

# data.table ####
dt <- fread("emi/FC_INITIAL.csv")

dt$pollutant <- as.character(dt$pollutant)
dt$g <- units::set_units(dt$emissions, "g")
dt$t <- units::set_units(dt$g, t)

dt0 <- dt[pollutant == "FC",
          round(sum(t), 2),
          by = .(fuel,
                 region)
]

names(dt0)[ncol(dt0)] <- "estimation_t"

dtf <- merge(dt0, fuel, by = c("fuel", "region"))

setorderv(dtf, cols = c("fuel", "region"))

dtf$estimation_consumption <- dtf$estimation_t / dtf$consumption_t

print(dtf[, c("region", "fuel", "estimation_t", "consumption_t", "estimation_consumption")])

# calibrate k ####
k_D <- as.numeric(1/dtf[fuel == "D"]$estimation_consumption)
k_G <- as.numeric(1/dtf[fuel == "G"]$estimation_consumption)
# print(paste(k_D, k_G))

dtf[, kfinal := as.numeric(1/estimation_consumption)]


metadata <- readRDS("config/metadata.rds")

categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
pmonth <- readRDS("config/pmonth.rds")
verbose <- TRUE
theme <- "black" # dark clean ink
survival   <- TRUE

fuel <- dtf

# 3 #####
language <- "english" # spanish portuguese
metadata <- readRDS("config/metadata.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
verbose <- TRUE
theme <- "black" # dark clean ink
survival   <- TRUE
source("scripts/trafficfuel.R")

#number# re estimate fc ####
verbose <- FALSE

suppressWarnings(file.remove("emi/FC_FINAL.csv"))

metadata_original <- metadata

metadata <- metadata[metadata$fuel != "ELEC", ]

# Hot Exhaust ####
cat("\nHot Running Fuel Consumption\n")

reg <- unique(pmonth$region)

# 4 Exhaust ####
for(k in seq_along(reg)) {

  cat(reg[k],  "\n")


  for(i in seq_along(metadata$vehicles)) {

    # cat("\n", metadata$vehicles[i],
    #     rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])))


    x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))

    x[is.na(x)] <- 0

    x <- x[region == reg[k], ]

    x$region <- NULL

    setDF(x)

    # euro
    cate <- suppressWarnings(
      as.character(as.roman(gsub("Euro ", "",
                                 euro[[metadata$vehicles[i]]]))))
    cate[is.na(cate)] <- "PRE"

    dm <- pmonth[region == reg[k] &
                   fuel == metadata$fuel[i]]$consumption_t

    for(j in seq_along(pol)){

      # cat(pol[j])

      if(metadata$v_eea_old[i] %in% c("PC", "LCV", "Motorcycle")) {


        ef <- ef_ldv_speed(v = metadata$v_eea_old[i],
                           t = metadata$t_eea_old[i],
                           cc = metadata$cc_eea_old[i],
                           f = metadata$fuel_eea_old[i],
                           p = "FC",
                           eu = cate,
                           speed = Speed(metadata$speed[i]))

      } else {
        ef <- ef_hdv_speed(v = metadata$v_eea_old[i],
                           t = metadata$t_eea_old[i],
                           g = metadata$cc_eea_old[i],
                           eu = cate,
                           gr = 0,
                           l = 0.5,
                           p = "FC",
                           speed = Speed(metadata$speed[i]))
      }

      nrow(x) ==  nrow(ef)

      ef$speed <- NULL



      array_x <- emis_hot_td(
        veh = x,
        lkm = mileage[[metadata$vehicles[i]]][1:metadata$maxage[i]],
        ef = ef[1:metadata$maxage[i]],
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

      fwrite(array_x, "emi/FC_FINAL.csv", append = TRUE)
    }
  }
}


# 5 Cold Start ####
cat("\nCold Exhaust Fuel Consumption\n")

metadata_cold <- metadata[metadata$fuel_eea_old %in% "G" &
                            metadata$v_eea_old %in% c("PC", "LCV"), ]


for(k in seq_along(reg)) {

  cat(reg[k],  "\n")


  for(i in seq_along(metadata_cold$vehicles)) {

    # cat("\n", metadata$vehicles[i],
    #     rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])))


    x <- readRDS(paste0("veh/", metadata_cold$vehicles[i], ".rds"))

    x[is.na(x)] <- 0

    x <- x[region == reg[k], ]

    x$region <- NULL

    setDF(x)

    # euro
    cate <- suppressWarnings(
      as.character(as.roman(gsub("Euro ", "",
                                 euro[[metadata_cold$vehicles[i]]]))))
    cate[is.na(cate)] <- "PRE"

    dm <- pmonth[region == reg[k] &
                   fuel == metadata_cold$fuel[i]]$consumption_t


    ta <- met[capitals == unique(capitals[k])]$Temperature

    for(j in seq_along(pol)){

      # cat(pol[j])

      ltrip <- add_lkm(metadata_cold$km_cycle[i])

      a <- cold_mileage(ltrip = ltrip, ta = celsius(ta))

      (ef <- ef_ldv_speed(v = metadata_cold$v_eea_old[i],
                          t = metadata_cold$t_eea_old[i],
                          cc = metadata_cold$cc_eea_old[i],
                          f = metadata_cold$fuel_eea_old[i],
                          p = "FC",
                          eu = cate,
                          speed = Speed(metadata_cold$speed[i])))

      (efcold <- ef_ldv_cold(ta = matrix(ta, nrow = 1),
                             cc = ifelse(metadata_cold$cc_eea_old[i] == "<3.5",
                                         ">2000",
                                         metadata_cold$cc_eea_old[i]),
                             f = metadata_cold$fuel_eea_old[i],
                             p = "FC",
                             eu = cate,
                             speed = Speed(metadata_cold$speed[i])))

      nrow(x) ==  nrow(ef)

      ef$speed <- NULL

      array_x <- emis_cold_td(
        veh = x,
        lkm = mileage[[metadata_cold$vehicles[i]]],
        ef = ef[, 1:ncol(x)],
        efcold = efcold[, 1:ncol(x)],
        fortran = TRUE,
        beta = matrix(a, nrow = 1),
        nt = vein::check_nt()/2,
        pro_month = dm,
        verbose = verbose,
        params = list(
          veh = metadata_cold$vehicles[i],
          size = metadata_cold$size[i],
          fuel = metadata_cold$fuel[i],
          pollutant = pol[j],
          type_emi = "Cold",
          subtype_emi = "Exhaust",
          baseyear = year_selected,
          month = rep(1:12, each = ncol(x))
        )
      )


      array_x$region <- reg[k]

      fwrite(array_x, "emi/FC_FINAL.csv", append = TRUE)


    }
  }
}




switch(language,
       "portuguese" = message("\nArquivos em:"),
       "english" = message("\nFiles in:"),
       "spanish" = message("\nArchivos en:")
)

cat(paste0(getwd(), "/emi/*\n"))

# data.table ####
fuel$estimation_t_initial <- fuel$estimation_t
fuel$estimation_t <- NULL
dt <- fread("emi/FC_FINAL.csv")

dt$pollutant <- as.character(dt$pollutant)
dt$g <- units::set_units(dt$emissions, "g")
dt$t <- units::set_units(dt$g, t)

dt0 <- dt[pollutant == "FC",
          round(sum(t), 2),
          by = .(fuel,
                 region)
]

names(dt0)[ncol(dt0)] <- "estimation_t"

dtf <- merge(dt0, fuel, by = c("fuel", "region"))

setorderv(dtf, cols = c("fuel", "region"))

dtf$estimation_consumption <- dtf$estimation_t / dtf$consumption_t

print(dtf[, c("region", "fuel", "estimation_t", "consumption_t", "estimation_consumption")])




switch(language,
       "portuguese" = message("\nArquivos em:"),
       "english" = message("\nFiles in:"),
       "spanish" = message("\nArchivos en:")
)


switch(language,
       "portuguese" = message("\nArquivos em:"),
       "english" = message("\nFiles in:"),
       "spanish" = message("\nArchivos en:")
)

cat(paste0(getwd(), "/emi/*\n"))


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
