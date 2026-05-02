suppressWarnings(file.remove("emi/evaporatives.csv"))

# adjustments
metadata_evap <- metadata[metadata$fuel_eea_old %in% "G" &
                            metadata$v_eea_old %in% c("PC", "LCV", "Motorcycle"), ]

metadata_evap$v_eea_old <- ifelse(metadata_evap$v_eea_old == "LCV", "PC", metadata_evap$v_eea_old)
metadata_evap$cc_eea_old <- ifelse(metadata_evap$cc_eea_old == "<3.5", "1400_2000", metadata_evap$cc_eea_old)
metadata_evap$cc_eea_old <- ifelse(metadata_evap$cc_eea_old == ">=50", ">50", metadata_evap$cc_eea_old)
metadata_evap[metadata_evap$cc_eea_old == ">50",]$v_eea_old <- "Motorcycle_2S"

# assuming Euro from Euro 2 erforfi, before erhotc


# Running Losses ####
cat("\n\n Evaporative emissions: Running Losses...")

for (i in seq_along(metadata_evap$vehicles)) {
  
  cat("\n", metadata_evap$vehicles[i],
      rep("", max(nchar(metadata_evap$vehicles) + 1) - nchar(metadata_evap$vehicles[i]))
  )
  
  veh <- readRDS(paste0("veh/", metadata_evap$vehicles[i], ".rds"))
  
  # euro
  cate <- suppressWarnings(
    as.character(as.roman(gsub("Euro ", "",  euro[[metadata_evap$vehicles[i]]]))))
  cate[is.na(cate)] <- "PRE"
  
  numeu <- suppressWarnings(as.numeric(gsub("Euro ", "",  euro[[metadata_evap$vehicles[i]]])))
  
  numeu[is.na(numeu)] <- 0
  
  evrl <- ifelse(numeu > 2, "erhotfi", "erhotc")
  
  ef <- ef_evap(ef = evrl[1:ncol(veh)],
                v = metadata_evap$v_eea_old[i], 
                cc = metadata_evap$cc_eea_old[i], 
                dt = matrix(met$value, nrow = 1),
                ca = ifelse(metadata_evap$cc_eea_old[i] == "<=1400", "small", 
                            ifelse(metadata_evap$cc_eea_old[i] == ">2000", "large",  
                                   ifelse(metadata_evap$cc_eea_old[i] == "1400_2000", "medium", "no"))), 
                show = F, 
                ltrip = add_lkm(metadata_evap$km_cycle[i]), 
                pollutant = "NMHC")
  
  ef <- EmissionFactors(as.numeric(ef))
  array_x <- emis(
    veh = veh,
    lkm = net$lkm,
    ef = ef,
    profile = tfs[[metadata$vehicles[i]]], 
    fortran = TRUE,
    nt = check_nt() / 2,
    simplify = TRUE,
    verbose = verbose
  )
  
  x_DF <- emis_post(
    arra = array_x,
    veh = metadata_evap$vehicles[i],
    size = metadata_evap$size[i],
    fuel = metadata_evap$fuel[i],
    pollutant = "NMHC",
    type_emi = "Evaporatives Running Losses",
    by = "veh"
  )
  
  fwrite(x_DF, "emi/evaporatives.csv", append = TRUE)
  
  x_ST <- emis_post(
    arra = array_x,
    by = "streets"
  )
  x_ST$fuel <- metadata$fuel[i]
  
  fwrite(x_ST, paste0("emi/", metadata$vehicles[i], "/EVAP_RUNNING_LOSSES.csv"), append = FALSE)
  
}


# Hot Soak ####
cat("\n\n Evaporative emissions: Hot Soak...")

for (i in seq_along(metadata_evap$vehicles)) {
  
  cat("\n", metadata_evap$vehicles[i],
      rep("", max(nchar(metadata_evap$vehicles) + 1) - nchar(metadata_evap$vehicles[i]))
  )
  
  veh <- readRDS(paste0("veh/", metadata_evap$vehicles[i], ".rds"))
  
  # euro
  cate <- suppressWarnings(
    as.character(as.roman(gsub("Euro ", "",  euro[[metadata_evap$vehicles[i]]]))))
  cate[is.na(cate)] <- "PRE"
  
  numeu <- suppressWarnings(as.numeric(gsub("Euro ", "",  euro[[metadata_evap$vehicles[i]]])))
  
  numeu[is.na(numeu)] <- 0
  
  evrl <- ifelse(numeu > 2, "eshotfi", "eshotc")
  
  ef <- ef_evap(ef = evrl[1:ncol(veh)],
                v = metadata_evap$v_eea_old[i], 
                cc = metadata_evap$cc_eea_old[i], 
                dt = matrix(met$value, nrow = 1),
                ca = ifelse(metadata_evap$cc_eea_old[i] == "<=1400", "small", 
                            ifelse(metadata_evap$cc_eea_old[i] == ">2000", "large",  
                                   ifelse(metadata_evap$cc_eea_old[i] == "1400_2000", "medium", "no"))), 
                show = F, 
                ltrip = add_lkm(metadata_evap$km_cycle[i]), 
                pollutant = "NMHC")
  
  ef <- EmissionFactors(as.numeric(ef))
  array_x <- emis(
    veh = veh,
    lkm = net$lkm,
    ef = ef,
    profile = tfs[[metadata$vehicles[i]]], 
    fortran = TRUE,
    nt = check_nt() / 2,
    simplify = TRUE,
    verbose = verbose
  )
  
  x_DF <- emis_post(
    arra = array_x,
    veh = metadata_evap$vehicles[i],
    size = metadata_evap$size[i],
    fuel = metadata_evap$fuel[i],
    pollutant = "NMHC",
    type_emi = "Evaporatives Hot Soak",
    by = "veh"
  )
  
  fwrite(x_DF, "emi/evaporatives.csv", append = TRUE)
  
  x_ST <- emis_post(
    arra = array_x,
    by = "streets"
  )
  x_ST$fuel <- metadata$fuel[i]
  
  fwrite(x_ST, paste0("emi/", metadata$vehicles[i], "/EVAP_HOT_SOAK.csv"), append = FALSE)
  
}


# Diurnal ####
cat("\n\n Evaporative emissions: Diurnal...")

for (i in seq_along(metadata_evap$vehicles)) {
  
  cat("\n", metadata_evap$vehicles[i],
      rep("", max(nchar(metadata_evap$vehicles) + 1) - nchar(metadata_evap$vehicles[i]))
  )
  
  veh <- readRDS(paste0("veh/", metadata_evap$vehicles[i], ".rds"))
  
  # euro
  cate <- suppressWarnings(
    as.character(as.roman(gsub("Euro ", "",  euro[[metadata_evap$vehicles[i]]]))))
  cate[is.na(cate)] <- "PRE"
  
  numeu <- suppressWarnings(as.numeric(gsub("Euro ", "",  euro[[metadata_evap$vehicles[i]]])))
  
  numeu[is.na(numeu)] <- 0
  
  evrl <- ifelse(numeu > 2, "ed", "ed")
  
  ef <- ef_evap(ef = evrl[1:ncol(veh)],
                v = metadata_evap$v_eea_old[i], 
                cc = metadata_evap$cc_eea_old[i], 
                dt = matrix(met$value, nrow = 1),
                ca = ifelse(metadata_evap$cc_eea_old[i] == "<=1400", "small", 
                            ifelse(metadata_evap$cc_eea_old[i] == ">2000", "large",  
                                   ifelse(metadata_evap$cc_eea_old[i] == "1400_2000", "medium", "no"))), 
                show = F, 
                kmday = mean(mileage[[metadata_evap$vehicles[i]]]/365), 
                pollutant = "NMHC")
  
  ef <- EmissionFactors(as.numeric(ef))
  array_x <- emis(
    veh = veh,
    lkm = net$lkm,
    ef = ef,
    profile = tfs[[metadata$vehicles[i]]], 
    fortran = TRUE,
    nt = check_nt() / 2,
    simplify = TRUE,
    verbose = verbose
  )
  
  x_DF <- emis_post(
    arra = array_x,
    veh = metadata_evap$vehicles[i],
    size = metadata_evap$size[i],
    fuel = metadata_evap$fuel[i],
    pollutant = "NMHC",
    type_emi = "Evaporatives Diurnal",
    by = "veh"
  )
  
  fwrite(x_DF, "emi/evaporatives.csv", append = TRUE)
  
  x_ST <- emis_post(
    arra = array_x,
    by = "streets"
  )
  x_ST$fuel <- metadata$fuel[i]
  
  fwrite(x_ST, paste0("emi/", metadata$vehicles[i], "/EVAP_DIURNAL.csv"), append = FALSE)
  
  
}


switch(language,
       "portuguese" = message("\nEmissões em: /emi/evaporative.csv:"),
       "english" = message("\nEmissions in: /emi/evaporative.csv"),
       "spanish" = message("\nEmisiones en: /emi/evaporative.csv")
)


switch(language,
       "portuguese" = message("Limpando..."),
       "english" = message("Cleaning..."),
       "spanish" = message("Limpiando...")
)

suppressWarnings(
  rm(
    i, j, pol,
    n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC,
    ns, ln, p, df, dl, cores
  )
)

ls()


invisible(gc())
