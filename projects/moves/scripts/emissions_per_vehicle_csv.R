
file.remove(list.files(path = "emi/emissions_rate_per_vehicle/", full.names = T))


setDT(metadata)
metadata_original <- metadata


metadata <- metadata_original[fuel != "ELEC"]

metadata$km_trip <- units::set_units(metadata$km_trip, "km")
metadata$km_trip <- units::set_units(metadata$km_trip, "miles")

# estimation ####

cat("Reading emission factors ...\n")
eff <- fread(path_ef_vehicle)

eff[ratePerVehicle > 0 &
      pollutantID != 91,
    unique(pollutantID)] -> unique_pol_all

unique_pol <- unique_pol_all[!unique_pol_all%in% c(91, #ENERGY 
                                                   1, #THC
                                                   118)] #NMHC
unique_pol <- data.table(pollutantID = unique_pol)
#
# 2) Obtener EF
for(pols in 1:nrow(unique_pol)) {
        
        eff[pollutantID == unique_pol$pollutantID[pols] & 
              ratePerVehicle > 0,
            mean(ratePerVehicle, na.rm = TRUE),
            by = .(MOVESRunID, 
                   pollutantID, 
                   modelYearID, 
                   processID,
                   fuelTypeID, 
                   hourID, 
                   sourceTypeID)] -> ef
        
        names(ef)
        names(ef)[ncol(ef)] <- "EF" 
        
        
        cat(paste0("Estimating emissions ...\n",
                   round(pols/nrow(unique_pol)*100, 2), " %\n" ))
        
        moves_rpsy_meta(
                metadata = metadata, 
                lkm = rep(1, length(lkm)), 
                ef = ef, 
                fuel_type = fuel_type, 
                profile = tfs,
                colkt = F,
                simplify = T, 
                verbose = verbose) -> emi
        
        
        data.table::fwrite(emi$streets,
                           paste0("emi/emissions_rate_per_vehicle/STREETS_pollutantID_",
                                  unique_pol$pollutantID[pols],
                                  ".csv.gz"),
                           append = F)

        data.table::fwrite(emi$veh, "emi/emissions_rate_per_vehicle/DF.csv.gz", append = T)
        rm(ef, emi)
        gc()
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
