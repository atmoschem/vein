
# filtrando veiculos otto
meta_ev <- metadata[metadata$fuel != "D", ]
veh_ev <- meta_ev$vehicles

# checks
type_emis <- c("Diurnal", "Running Losses", "Hot Soak")
name_file_evap <- c("/DIURNAL_", "/RUNNING_LOSSES_", "/HOT_SOAK_")
ef_evaps <- c(diurnal_ef, running_losses_ef, hot_soak_ef)

# Evaporativas  ####
for(i in seq_along(veh_ev)) {
  for(j in seq_along(ef_evaps)){
    
  cat("Estimando emissões evaporativas ", type_emis[j], " de:", veh_ev[i], "...\n")
    
  x <- readRDS(paste0("veh/", veh_ev[i], ".rds"))

    ef <- ef_cetesb(p = ef_evaps[j], 
                    veh = veh_ev[i], 
                    year = year,
                    agemax = ncol(x),
                    verbose = verbose)/(mileage[[veh_ev[i]]]/365) # quilometragem medio diario
    
  # diurnal g/day, other g/trip
  # g/day * day/km = g/km
  # g/trip * trip/day * day/km = g/km
  if(type_emis[j] != "Diurnal") ef <- ef*meta_ev$trips_day[i]
  
  # muda NaNaN para 0
  ef[is.na(ef)] <- 0
  
  # adicionar unidades
  ef <- EmissionFactors(ef)
  
  array_x <- emis(veh = x, 
                  lkm = lkm, 
                  ef = ef, 
                  profile = tfs[[veh_ev[i]]], 
                  fortran = TRUE, 
                  simplify = TRUE,
                  verbose = verbose)
  
  x_DF <- emis_post(arra = array_x, 
                    veh = veh_ev[i], 
                    size = metadata$size[i],
                    fuel = metadata$fuel[i], 
                    pollutant = "NMHC", 
                    type_emi = type_emis[j],
                    by = 'veh')
  
  
  saveRDS(x_DF, 
          file = paste0('emi/', 
                        veh_ev[i] , name_file_evap[j],
                        veh_ev[i] ,'_', 
                        'EVAP_NMHC_DF.rds'))
  
  x_STREETS <- emis_post(arra = array_x, 
                         pollutant = veh_ev[j], 
                         by = 'streets') 
  saveRDS(x_STREETS, 
          file = paste0('emi/', 
                        veh_ev[i] , name_file_evap[j],
                        veh_ev[i] ,'_', 
                        'EVAP_HCNM_STREETS.rds'))
  
  rm(array_x, ef, x, x_DF, x_STREETS)
  }
}


# Evaporativas running losses ####
for(i in seq_along(veh_ev)) {
  
  cat("Estimando emissões evaporativas running losses de:", veh_ev[i], "...\n")
  x <- readRDS(paste0("veh/", veh_ev[i], ".rds"))
  
  ef <- ef_cetesb(p = running_losses_ef, 
                  veh = veh_ev[i], 
                  year = year,
                  agemax = ncol(x),
                  verbose = verbose)*meta_ev$trips_day[i]/(mileage[[veh_ev[i]]]/365) # quilometragem medio diario
  
  # muda NaNaN para 0
  ef[is.na(ef)] <- 0
  
  # adicionar unidades
  ef <- EmissionFactors(ef)
  
  array_x <- emis(veh = x, 
                  lkm = lkm, 
                  ef = ef, 
                  profile = tfs[[veh_ev[i]]], 
                  fortran = TRUE, 
                  simplify = TRUE,
                  verbose = verbose)
  
  x_DF <- emis_post(arra = array_x, 
                    veh = veh_ev[i], 
                    size = metadata$size[i],
                    fuel = metadata$fuel[i], 
                    pollutant = "NMHC", 
                    type_emi = "Running Losses",
                    by = 'veh')
  
  saveRDS(x_DF, 
          file = paste0('emi/', 
                        veh_ev[i] ,'/RUNNING_LOSSES_', 
                        veh_ev[i] ,'_', 
                        'EVAP_NMHC_DF.rds'))
  
  x_STREETS <- emis_post(arra = array_x, 
                         pollutant = veh_ev[j], 
                         by = 'streets') 
  saveRDS(x_STREETS, 
          file = paste0('emi/', 
                        veh_ev[i] ,'/RUNNING_LOSSES_', 
                        veh_ev[i] ,'_', 
                        'EVAP_NMHC_STREETS.rds'))
  
  rm(array_x, ef, x, x_DF, x_STREETS)
}


# Evaporativas hot soak ####
for(i in seq_along(veh_ev)) {
  
  cat("Estimando emissões evaporativas hot soak de:", veh_ev[i], "...\n")
  x <- readRDS(paste0("veh/", veh_ev[i], ".rds"))
  
  ef <- ef_cetesb(p = hot_soak_ef, 
                  veh = veh_ev[i], 
                  year = year,
                  agemax = ncol(x),
                  verbose = verbose)*meta_ev$trips_day[i]/(mileage[[veh_ev[i]]]/365) # quilometragem medio diario
  
  # muda NaNaN para 0
  ef[is.na(ef)] <- 0
  
  # adicionar unidades
  ef <- EmissionFactors(ef)
  
  array_x <- emis(veh = x, 
                  lkm = lkm, 
                  ef = ef, 
                  profile = tfs[[veh_ev[i]]], 
                  fortran = TRUE, 
                  simplify = TRUE,
                  verbose = verbose)
  
  x_DF <- emis_post(arra = array_x, 
                    veh = veh_ev[i], 
                    size = metadata$size[i],
                    fuel = metadata$fuel[i], 
                    pollutant = "NMHC", 
                    type_emi = "Hot Soak",
                    by = 'veh')

  saveRDS(x_DF, 
          file = paste0('emi/', 
                        veh_ev[i] ,'/HOT_SOAK_', 
                        veh_ev[i] ,'_', 
                        'EVAP_NMHC_DF.rds'))
  
  x_STREETS <- emis_post(arra = array_x, 
                         pollutant = veh_ev[j], 
                         by = 'streets') 
  saveRDS(x_STREETS, 
          file = paste0('emi/', 
                        veh_ev[i] ,'/HOT_SOAK_', 
                        veh_ev[i] ,'_', 
                        'EVAP_NMHC_STREETS.rds'))
  
  rm(array_x, ef, x, x_DF, x_STREETS)
}


cat(paste0("Arquivos em ", getwd(), "/emi/*\n"))
cat("Limpando... \n")
rm(i, mileage, meta_ev, veh_ev, year,
   diurnal_ef, hot_soak_ef, running_losses_ef)

# deberiamos ter al menos: "composition" "net"         "veh"         "year"       
ls()   
gc()
