
file.remove("emi/emissions_rate_per_distance/STREETS_pollutantID_NMHC_BY_TYPE.csv.gz")

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

# select unique fuels ####
cat("Detecting unique fuels for NMHC ...\n")

con <- DBI::dbConnect(RMariaDB::MariaDB(),
                      host = host,
                      db = moves_db,
                      user = user,
                      password = password,
                      port = port)

RMariaDB::dbGetQuery(con,paste0("
SELECT DISTINCT fuelTypeID
FROM `rateperdistance`
WHERE pollutantID = 79 AND
rateperdistance > 0
")) -> fuelTypeID

names(fuelTypeID)[ncol(fuelTypeID)] <- "EF" 
setDT(fuelTypeID)

RMariaDB::dbDisconnect(con)

for(i in seq_along(fuelTypeID$EF)) {
    
cat(paste0("Detecting NMHC and process for fuelTypeID ", fuelTypeID$EF[i], " in SQL ...\n"))
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
WHERE pollutantID = 79 AND
fuelTypeID = ", fuelTypeID$EF[i], " AND
rateperdistance > 0
GROUP BY MOVESRunID, pollutantID, modelYearID, processID,
fuelTypeID, avgSpeedBinID, hourID, sourceTypeID
")) -> ef

names(ef)[ncol(ef)] <- "EF" 
setDT(ef)

RMariaDB::dbDisconnect(con)

unique(ef$fuelT)

moves_rpdy_meta(
        metadata = metadata, 
        lkm = lkm, 
        ef = ef, 
        fuel_type = fuel_type, 
        speed_bin = speed_bin, 
        profile = tfs,
        simplify = F) -> emi


metadata$vtype <- c(rep("LDV", 13), rep("HDV", 39-14))
id_ldv <- unique(metadata[vtype == "LDV"]$sourceTypeID)
id_hdv <- unique(metadata[vtype == "HDV"]$sourceTypeID)

emi[, family := fifelse(
        sourceTypeID %in% id_ldv, "LDV", "HDV"
        )]

emi[, family := fifelse(
        fuel %in% "CNG", "ALL", family
)]

data(decoder)

names(decoder$emission_process) <- c("processID", "process")

emi <- merge(emi, 
             decoder$emission_process, 
             by = "processID",
             all.x = TRUE)

# we need 
setDT(decoder$emission_process)

pros <- decoder$emission_process[processID %in% unique(emi$processID)]

# Assuming Refuelling as evap
exhaust <- unique(grep(pattern = "Exhaust", 
                       x = emi$process, 
                       value = T))

emi[, process2 := fifelse(process %in% exhaust, "exhaust", "evap")]

    
emi[, 
    sum(age_total),
    by = .(id, hour, family, fuel, process2)] -> emi2

streets <- data.table::dcast.data.table(emi2, 
                                        formula = id +family + fuel + process2 ~ hour, 
                                        value.var = "V1")

names(streets)[5:ncol(streets)] <- paste0("H", names(streets)[5:ncol(streets)])

# Need streets results by 

data.table::fwrite(streets, 
                   "emi/emissions_rate_per_distance/STREETS_pollutantID_NMHC_BY_TYPE.csv.gz", 
                   append = T)

}

suppressWarnings(
        rm(
                i, j, pol,
                n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC,
                ns, ln, p, df, dl, cores, streets, emi, emi2
        )
)

invisible(gc())
