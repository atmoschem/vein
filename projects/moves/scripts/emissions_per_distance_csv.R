
file.remove(list.files(path = "emi/emissions_rate_per_distance/", full.names = T))


setDT(metadata)
metadata_original <- metadata

# me parece que la mejor opcion es filtrar por 
# 1) Identify pollutants in data.base

# estimation ####

cat("Reading emission factors ...\n")
eff <- fread(path_ef_distance)

eff[ratePerDistance > 0,
   unique(pollutantID)] -> unique_pol_all

unique_pol <- unique_pol_all[!unique_pol_all%in% c(91, #ENERGY 
                                                   1, #THC
                                                   118)] #NMHC
# we keep pollutant 70 in case the user wants to applies a different speciation
# 79 NMHC, for instance, MOVES

data(decoder)

# Emissions per distance ####
i = 1

for(i in seq_along(unique_pol)) {
  
  
  eff[pollutantID == unique_pol[i] & 
       ratePerDistance > 0,
     mean(ratePerDistance, na.rm = TRUE),
     by = .(MOVESRunID, 
            pollutantID, 
            modelYearID, 
            processID,
            fuelTypeID, 
            avgSpeedBinID, 
            hourID, 
            sourceTypeID)] -> ef
  
  names(ef)
  names(ef)[ncol(ef)] <- "EF" 
  
  cat(paste0("Estimating emissions ...\n",
             round(i/length(unique_pol)*100, 2), " %\n\n" ))
  
  moves_rpdy_meta(
    metadata = metadata, 
    lkm = lkm, 
    ef = ef, 
    fuel_type = fuel_type, 
    speed_bin = speed_bin, 
    profile = tfs,
    simplify = T) -> emi
  
  
  data.table::fwrite(emi$streets, 
                     paste0("emi/emissions_rate_per_distance/STREETS_pollutantID_",
                            unique_pol[i], 
                            ".csv.gz"), 
                     append = T)
  
  data.table::fwrite(emi$veh, "emi/emissions_rate_per_distance/DF.csv.gz", append = T)
  rm(ef, emi)
  gc()
}

# 3) tire and break wear emissions for electric cars ####


switch(language,
       "portuguese" = message("\nEmissões de freio e pneus de veículos elétricos\n"),
       "english" = message("Vehicular emissions of electric cars for tire and break wear\n"),
       "spanish" = message("Emisiones de vehículos electricos por desgaste de frenos y neumáticos\n")
)


metadata <- metadata_original[fuel == "ELEC"]

for(i in seq_along(metadata$vehicles)) {
  
  eff[sourceTypeID == metadata$sourceTypeID[i] & 
       ratePerDistance > 0 &
        fuelTypeID == 1 &
       processID %in% 9:10,
     mean(ratePerDistance, na.rm = TRUE),
     by = .(MOVESRunID, 
            pollutantID, 
            modelYearID,
            fuelTypeID, 
            avgSpeedBinID, 
            hourID, 
            sourceTypeID, 
            processID)] -> ef
  
  names(ef)
  names(ef)[ncol(ef)] <- "EF" 
  
  cat(paste0("\nEstimating emissions ...\n",
             "sourceTypeID:   ", metadata$sourceTypeID[i], "\n",
             "fuelTypeID:      ", metadata$fuelTypeID[i], "\n",
             round(i/nrow(metadata)*100, 2), " %\n" ))
  
  moves_rpdy_sf(
    veh = readRDS(paste0("veh/", metadata$vehicles[i], ".rds")),
    lkm = lkm, 
    ef = ef, 
    speed_bin = speed_bin,
    profile = tfs[[metadata$vehicles[i]]],
    source_type_id = metadata$sourceTypeID[i], 
    vehicle = metadata$vein_name[i], 
    vehicle_type = metadata$name[i], 
    fuel_subtype = metadata$fuel[i]
  ) -> emi
  
  data.table::fwrite(emi$streets, 
                     paste0("emi/emissions_rate_per_distance/STREETS_",
                            metadata$vehicles[i], 
                            ".csv.gz"), 
                     append = T)
  data.table::fwrite(emi$veh, "emi/emissions_rate_per_distance/DF_ELEC.csv.gz", append = T)
  
}


switch(language,
       "portuguese" = message("\n\nArquivos em: /emi/*:"),
       "english" = message("\nFiles in: /emi/*"),
       "spanish" = message("\nArchivos en: /emi/*")
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
    ns, ln, p, df, dl, cores, eff
  )
)

invisible(gc())
