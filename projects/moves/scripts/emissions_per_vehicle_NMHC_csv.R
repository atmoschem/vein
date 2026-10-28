
file.remove("emi/emissions_rate_per_vehicle/STREETS_pollutantID_NMHC_BY_TYPE.csv.gz")


setDT(metadata)
metadata_original <- metadata



metadata <- metadata_original[fuel != "ELEC"]

data(decoder)

# estimation ####
cat("Reading emission factors ...\n")
eff <- fread(path_ef_vehicle)

# select unique fuels ####

eff[ratePerVehicle > 0 &
      pollutantID == 79,
    unique(fuelTypeID)] -> fuelTypeIDEF

fuelTypeIDEF <- as.data.table(fuelTypeIDEF)

names(fuelTypeIDEF)[ncol(fuelTypeIDEF)] <- "EF" 

# light (MC, PC, PT, LCT) or heavy (BUS, TRUCKS) from the full metadata,
# computed once outside the loop so that `metadata` keeps the non-electric
# filter in every iteration
meta_all <- readRDS("config/metadata.rds")
setDT(meta_all)
meta_all$vtype <- ifelse(meta_all$family %in% c("MC", "PC", "PT", "LCT"), "LDV", "HDV")
id_ldv <- unique(meta_all[vtype == "LDV"]$sourceTypeID)
id_hdv <- unique(meta_all[vtype == "HDV"]$sourceTypeID)

for(i in seq_along(fuelTypeIDEF$EF)) {
    

eff[pollutantID == 79 & 
      fuelTypeID == fuelTypeIDEF$EF[i] &
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

# WARNING (methodological, not fixed): MOVES rates per vehicle (starts,
# evaporative, hotelling; g/vehicle/hour) are applied here to the traffic flow
# of every street link (lkm = 1). A vehicle is then counted on every link it
# drives through, so these emissions scale with network density rather than
# with the vehicle population. Expect an overestimate.
moves_rpsy_meta(
    metadata = metadata, 
    lkm = rep(1, length(lkm)), 
    ef = ef, 
    fuel_type = fuel_type, 
    profile = tfs,
    colkt = F,
    simplify = F, 
    verbose = verbose) -> emi


#NMHC

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
# ssuming Refuelling as evap
exhaust <- unique(grep(pattern = "Exhaust", 
                       x = emi$process, 
                       value = T))

emi[, process2 := fifelse(process %in% exhaust, "exhaust", "evap")]


emi[, 
    sum(age_total, na.rm = T),
    by = .(id, hour, family, fuel, process2)] -> emi2

streets <- data.table::dcast.data.table(emi2, 
                                        formula = id +family + fuel + process2 ~ hour, 
                                        value.var = "V1")

names(streets)[5:ncol(streets)] <- paste0("H", names(streets)[5:ncol(streets)])

# Need streets results by 

data.table::fwrite(streets, 
                   "emi/emissions_rate_per_vehicle/STREETS_pollutantID_NMHC_BY_TYPE.csv.gz", 
                   append = T)

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
