
file.remove(list.files(path = "emi/emissions_rate_per_distance/", full.names = T))


# switch(language,
#        "portuguese" = cat("Filtrando carro electrico: \n"),
#        "english" = cat("Filtering electric car\n"),
#        "spanish" = cat("Filtrando autos electricos\n")
# )
setDT(metadata)
metadata_original <- metadata

# me parece que la mejor opcion es filtrar por 
# contaminante y por proceso.
# 1) por ejemplo, leer EF para pollutantID = 2
# 2) luego filtrar por processID y relacionar con metadata
# 3) Luego, leer los veiculos con metadata
# 4) y estimar las eisiones para el processID

# 1) Identify pollutants in data.base

# estimation ####
con <- DBI::dbConnect(RMariaDB::MariaDB(),
                      host = host,
                      db = moves_db,
                      user = user,
                      password = password,
                      port = port)

RMariaDB::dbGetQuery(con,paste0("
SELECT DISTINCT pollutantID
FROM `rateperdistance`
WHERE  rateperdistance > 0
")) -> unique_pol_all
RMariaDB::dbDisconnect(con)

setDT(unique_pol_all)
unique_pol <- unique_pol_all[!pollutantID %in% c(91, #ENERGY 
                                                 1, #THC
                                                 118)] #NMHC

# we keep pollutant 70 in case the user wants to applies a different speciation
# 79 NMHC, for instance, MOVES

data(decoder)
# 2) Obtener EF

# 2) Emissions per distance ####

for(i in 1:nrow(unique_pol)) {
    
    cat("Detecting pollutants and process in SQL ...\n")
    
    # pollutantID = i
    con <- DBI::dbConnect(RMariaDB::MariaDB(),
                          host = host,
                          db = moves_db,
                          user = user,
                          password = password,
                          port = port)
    
    RMariaDB::dbGetQuery(con,paste0("
SELECT pollutantID, sourceTypeID, fuelTypeID, processID,
modelYearID, hourID, avgSpeedBinID, ratePerDistance, AVG(ratePerDistance)
FROM `rateperdistance`
WHERE pollutantID = ", unique_pol$pollutantID[i], " AND
rateperdistance > 0
GROUP BY MOVESRunID, pollutantID, modelYearID, processID,
fuelTypeID, avgSpeedBinID, hourID, sourceTypeID
")) -> ef
    
    names(ef)[ncol(ef)] <- "EF" 
    setDT(ef)
    
    RMariaDB::dbDisconnect(con)
    
    cat(paste0("Estimating emissions ...\n",
               round(i/nrow(unique_pol)*100, 2), " %\n\n" ))
    
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
                              unique_pol$pollutantID[i], 
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
    
    
    if(verbose) cat("Detecting pollutants and process in SQL ...\n")
    
    con <- DBI::dbConnect(RMariaDB::MariaDB(),
                          host = host,
                          db = moves_db,
                          user = user,
                          password = password,
                          port = port)
    
    RMariaDB::dbGetQuery(con,paste0("
SELECT pollutantID, sourceTypeID, fuelTypeID, processID,
modelYearID, hourID, avgSpeedBinID, ratePerDistance, AVG(ratePerDistance)
FROM `rateperdistance`
WHERE rateperdistance > 0 AND
sourceTypeID = ", metadata$sourceTypeID[i]," AND
fuelTypeID = 1  AND
(processID = 9 OR processID = 10)
GROUP BY MOVESRunID, pollutantID, modelYearID,
fuelTypeID, avgSpeedBinID, hourID, sourceTypeID, processID
")) -> ef
    
    setDT(ef)
    names(ef)[ncol(ef)] <- "EF"
    RMariaDB::dbDisconnect(con)
    
    
    
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
        ns, ln, p, df, dl, cores
    )
)

invisible(gc())
