
file.remove(list.files(path = "emi/emissions_rate_per_vehicle/", full.names = T))


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

metadata <- metadata_original[fuel != "ELEC"]

metadata$km_trip <- units::set_units(metadata$km_trip, "km")
metadata$km_trip <- units::set_units(metadata$km_trip, "miles")

# estimation ####
con <- DBI::dbConnect(RMariaDB::MariaDB(),
                      host = host,
                      db = moves_db,
                      user = user,
                      password = password,
                      port = port)

RMariaDB::dbGetQuery(con,paste0("
SELECT DISTINCT pollutantID
FROM `ratepervehicle`
WHERE  ratepervehicle > 0 AND
pollutantID != 91
")) -> unique_pol_all
RMariaDB::dbDisconnect(con)

setDT(unique_pol_all)
unique_pol <- unique_pol_all[!pollutantID %in% c(91, #ENERGY 
                                                 1, #THC
                                                 118)] #Composite 

# we keep pollutant 70 in case the user wants to applies a different speciation
# 79 NMHC, for instance, MOVES

data(decoder)
# 2) Obtener EF
for(pols in 1:nrow(unique_pol)) {
        
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
modelYearID, hourID, ratePerVehicle, AVG(ratePerVehicle)
FROM `rateperVehicle`
WHERE pollutantID = ",unique_pol$pollutantID[pols], " AND
ratepervehicle > 0
GROUP BY MOVESRunID, pollutantID, modelYearID, processID,
fuelTypeID, hourID, sourceTypeID
")) -> ef
        
        names(ef)[ncol(ef)] <- "EF" 
        setDT(ef)
        
        RMariaDB::dbDisconnect(con)
        
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
                ns, ln, p, df, dl, cores
        )
)

invisible(gc())
