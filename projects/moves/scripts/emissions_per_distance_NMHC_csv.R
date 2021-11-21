
file.remove("emi/emissions_rate_per_distance/STREETS_pollutantID_NMHC_BY_TYPE.csv.gz")

setDT(metadata)
metadata_original <- metadata

# estimation ####

cat("Reading emission factors ...\n")
eff <- fread(path_ef_distance)

# select unique fuels ####
cat("Detecting unique fuels for NMHC ...\n")

eff[ratePerDistance > 0 &
      pollutantID == 79,
    unique(fuelTypeID)] -> fuelTypeIDEF

fuelTypeIDEF <- as.data.table(fuelTypeIDEF)

names(fuelTypeIDEF)[ncol(fuelTypeIDEF)] <- "EF" 


for(i in seq_along(fuelTypeIDEF$EF)) {

eff[pollutantID == 79 & 
      fuelTypeID == fuelTypeIDEF$EF[i] &
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
