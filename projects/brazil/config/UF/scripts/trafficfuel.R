
switch (language,
        "portuguese" = message("\nGerando frota calibrada com combustivel\n"),
        "english" = message("\nGenerating fuel-calibrated fleet\n"),
        "spanish" = message("\nGenerando flota vehicular calibrada con combustible\n"))

inte <- intersect(metadata$vehicles, names(veh))
if(length(inte) != length(metadata$vehicles)){
  
  
  switch (language,
          "portuguese" = cat( "veh precisa ter mesmos veiculos de metadata$vehicles:\n"),
          "english" = cat( "veh needs the same vehicles as metadata$vehicles:\n"),
          "spanish" = cat( "veh necesita los mismos vehiculos de metadata$vehicles:\n"))
  
  
  stop()
}


# apagando arquivos
switch (language,
        "portuguese" = message("Apagando veh/*.rds\n"),
        "english" = message("Deleting veh/*.rds\n"),
        "spanish" = message("Borrando veh/*.rds\n"))

arquivos <- list.files(path = "veh", pattern = ".rds", full.names = TRUE)
file.remove(arquivos)

# fleet age
veh[is.na(veh)] <- 0

# identicar nomes de grupos
nveh <- names(veh)
n_PC <- nveh[grep(pattern = "PC", x = nveh)]
n_LCV <- nveh[grep(pattern = "LCV", x = nveh)]
n_TRUCKS <- nveh[grep(pattern = "TRUCKS", x = nveh)]
n_BUS <- nveh[grep(pattern = "BUS", x = nveh)]
n_MC <- nveh[grep(pattern = "MC", x = nveh)]

# PC ####
# apply survival functions
for(i in seq_along(metadata$vehicles)) {
  veh[[metadata$vehicles[i]]] <-   age(x = veh[[metadata$vehicles[i]]], 
                                       type = metadata$survival[i], 
                                       a = metadata$survival_param_a[i],
                                       b = metadata$survival_param_b[i])
}
#
# calculate proportion in PC 
#

kPC_G <- sum(veh$PC_G)/sum(veh[, n_PC])
kPC_E <- sum(veh$PC_E)/sum(veh[, n_PC])
kPC_FG <- sum(veh$PC_FG)/sum(veh[, n_PC])
kPC_FE <- sum(veh$PC_FE)/sum(veh[, n_PC])
kPC <- c(kPC_G, kPC_E, kPC_FG, kPC_FE)

if(verbose) {
  cat(paste0(
    "PC_G  is ", round(kPC_G, 3), " of PC\n",
    "PC_E  is ", round(kPC_E, 3), " of PC\n",
    "PC_FG is ", round(kPC_FG, 3), " of PC\n",
    "PC_FE is ", round(kPC_FE, 3), " of PC\n",
    "sum: ", kPC_G + kPC_E + kPC_FG +  kPC_FE, "\n\n"))
}

# fuel
kf <- c(k_G, k_E, k_G, k_E)
  
for(i in seq_along(n_PC)) {
  x <- veh[[n_PC[i]]]*kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_PC[i], ".rds"))
}

# LCV ####
#
# calculate proportion in LCV
#

kLCV_G <- sum(veh$LCV_G)/sum(veh[, n_LCV])
kLCV_E <- sum(veh$LCV_E)/sum(veh[, n_LCV])
kLCV_FG <- sum(veh$LCV_FG)/sum(veh[, n_LCV])
kLCV_FE <- sum(veh$LCV_FE)/sum(veh[, n_LCV])
kLCV_D <- sum(veh$LCV_D)/sum(veh[, n_LCV])
kLCV <- c(kLCV_G, kLCV_E, kLCV_FG, kLCV_FE, kLCV_D) 

if(verbose) {
    cat(paste0(
  "LCV_G  é ", round(kLCV_G, 3), " de LCV\n",
  "LCV_E  é ", round(kLCV_E, 3), " de LCV\n",
  "LCV_FG é ", round(kLCV_FG, 3), " de LCV\n",
  "LCV_FE é ", round(kLCV_FE, 3), " de LCV\n",
  "LCV_D  é ", round(kLCV_D, 3), " de LCV\n\n",
  "sum: ", kLCV_G + kLCV_FG +  kLCV_FE +  kLCV_E + kLCV_D, "\n"))
}

# fuel
kf <- c(k_G, k_E, k_G, k_E, k_D)

for(i in seq_along(n_LCV)) {
  x <- veh[[n_LCV[i]]]*kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_LCV[i], ".rds"))
}

# TRUCKS ####
#
# calculate proportion in TRUCKS
#

kTRUCKS_SL_D <- sum(veh$TRUCKS_SL_D)/sum(veh[, n_TRUCKS])
kTRUCKS_L_D <- sum(veh$TRUCKS_L_D)/sum(veh[, n_TRUCKS])
kTRUCKS_M_D <- sum(veh$TRUCKS_M_D)/sum(veh[, n_TRUCKS])
kTRUCKS_SH_D <- sum(veh$TRUCKS_SH_D)/sum(veh[, n_TRUCKS])
kTRUCKS_H_D <- sum(veh$TRUCKS_H_D)/sum(veh[, n_TRUCKS])
kTRUCKS <- c(kTRUCKS_SL_D, kTRUCKS_L_D, kTRUCKS_M_D, kTRUCKS_SH_D, kTRUCKS_H_D)

if(verbose) {
  cat(paste0(
  "TRUCKS_G  é ", round(kTRUCKS_SL_D, 3), " de TRUCKS\n",
  "TRUCKS_FG é ", round(kTRUCKS_L_D, 3), " de TRUCKS\n",
  "TRUCKS_FE é ", round(kTRUCKS_M_D, 3), " de TRUCKS\n",
  "TRUCKS_E  é ", round(kTRUCKS_SH_D, 3), " de TRUCKS\n",
  "TRUCKS_D  é ", round(kTRUCKS_H_D, 3), " de TRUCKS\n\n",
  "sum: ", kTRUCKS_SL_D + kTRUCKS_L_D +  kTRUCKS_M_D +  kTRUCKS_SH_D + kTRUCKS_H_D, "\n"))
}

# fuel
kf <- rep(k_D, 5)

for(i in seq_along(n_TRUCKS)) {
  x <- veh[[n_TRUCKS[i]]]*kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_TRUCKS[i], ".rds"))
}

# BUS ####
#
# calculate proportion in BUS
#

kBUS_URBAN_D <- sum(veh$BUS_URBAN_D)/sum(veh[, n_BUS])
kBUS_MICRO_D <- sum(veh$BUS_MICRO_D)/sum(veh[, n_BUS])
kBUS_COACH_D <- sum(veh$BUS_COACH_D)/sum(veh[, n_BUS])
kBUS <- c(kBUS_URBAN_D, kBUS_MICRO_D, kBUS_COACH_D)

if(verbose) {
  cat(paste0(
  "BUS_URBAN_D  é ", round(kBUS_URBAN_D, 3), " de BUS\n",
  "BUS_MICRO_D  é ", round(kBUS_MICRO_D, 3), " de BUS\n",
  "BUS_URBAN_D  é ", round(kBUS_COACH_D, 3), " de BUS\n",
  "sum: ", kBUS_URBAN_D + kBUS_MICRO_D +  kBUS_COACH_D, "\n\n"))
}

# fuel
kf <- rep(k_D, 3)

for(i in seq_along(n_BUS)) {
  x <- veh[[n_BUS[i]]]*kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_BUS[i], ".rds"))
}

# MC ####
#
# calculate proportion in MC
#

kMC_150_G <- sum(veh$MC_150_G)/sum(veh[, n_MC])
kMC_150_500_G <- sum(veh$MC_150_500_G)/sum(veh[, n_MC])
kMC_500_G <- sum(veh$MC_500_G)/sum(veh[, n_MC])
kMC_150_FG <- sum(veh$MC_150_FG)/sum(veh[, n_MC])
kMC_150_500_FG <- sum(veh$MC_150_500_FG)/sum(veh[, n_MC])
kMC_500_FG <- sum(veh$MC_500_FG)/sum(veh[, n_MC])
kMC_150_FE <- sum(veh$MC_150_FE)/sum(veh[, n_MC])
kMC_150_500_FE <- sum(veh$MC_150_500_FE)/sum(veh[, n_MC])
kMC_500_FE <- sum(veh$MC_500_FE)/sum(veh[, n_MC])
kMC <- c(kMC_150_G, kMC_150_500_G, kMC_500_G,
         kMC_150_FG, kMC_150_500_FG, kMC_500_FG,
         kMC_150_FE, kMC_150_500_FE, kMC_500_FE)

if(verbose) {
  cat(paste0(
  "MC_150_G  é ", round(kMC_150_G, 3), " de MC\n",
  "MC_150_500_G  é ", round(kMC_150_500_G, 3), " de MC\n",
  "MC_500_G  é ", round(kMC_500_G, 3), " de MC\n",
  "MC_150_FG  é ", round(kMC_150_FG, 3), " de MC\n",
  "MC_150_500_FG  é ", round(kMC_150_500_FG, 3), " de MC\n",
  "MC_500_FG  é ", round(kMC_500_FG, 3), " de MC\n",
  "MC_150_FE  é ", round(kMC_150_FE, 3), " de MC\n",
  "MC_150_500_FE  é ", round(kMC_150_500_FE, 3), " de MC\n",
  "MC_500_FE  é ", round(kMC_500_FE, 3), " de MC\n",
  "sum: ", 
  kMC_150_G + kMC_150_500_G +  kMC_500_G +
    kMC_150_FG + kMC_150_500_FG + kMC_500_FG +
    kMC_150_FE + kMC_150_500_FE + kMC_500_FE, "\n\n"))
}

# fuel
kf <- c(rep(k_G, 6), rep(k_E, 3))

for(i in seq_along(n_MC)) {
  x <- veh[[n_MC[i]]]*kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_MC[i], ".rds"))
}
invisible(gc())
