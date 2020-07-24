# temperature
te <- met$Temperature
bb <-   (0+15)/2
cc <- (10+25)/2
dd <- (20+35)/2 

tem <- ifelse(
  te <= bb, "0_15",
  ifelse(
    te > bb & te <= cc, "10_25",
    "20_35"))

nmonth <- ifelse(nchar(seq_along(te)) < 2, 
                 paste0(0, seq_along(te)), 
                 seq_along(te))

# filtrando veiculos otto
meta_ev <- metadata[metadata$fuel != "D", ]
veh_ev <- meta_ev$vehicles

# checks name
type_emis <- c("Diurnal", "Running Losses", "Hot Soak")
name_file_evap <- c("/DIURNAL_", "/RUNNING_LOSSES_", "/HOT_SOAK_")

ef_d  <- paste0("D_", tem)
ef_rl <- paste0("R_", tem)
ef_hs <- paste0("H_", tem)


switch (language,
        "portuguese" = cat("Emissões evaporativas diurnal\n"),
        "english" = cat("Evporative diurnal emissions\n"),
        "chinese" = cat("蒸发昼间排放\n"),
        "spanish" = cat("Emisiones evaporativas diurnal\n"))

if(progress == "bar") pb = txtProgressBar(min = 0, 
                                          max = length(veh_ev), 
                                          initial = 0, 
                                          style = 3) 

# Evaporativas  ####
for(i in seq_along(veh_ev)) {

  if(progress == "bar") {
    setTxtProgressBar(pb,i)
  }  else {
    cat("\n", veh_ev[i], 
        rep("", max(nchar(veh_ev) + 1) - nchar(veh_ev[i])))
  }

    
  for(j in seq_along(te)){
    
    if(progress != "bar") cat(nmonth[j], " ")
    
    x <- readRDS(paste0("veh/", veh_ev[i], ".rds"))
    
    ef <- ef_cetesb(p = ef_d[j], 
                    veh = veh_ev[i], 
                    year = year,
                    agemax = ncol(x),
                    # diurnal: g/day* day/km= g/km
                    verbose = verbose)/(mileage[[veh_ev[i]]]/365) # mean daily mileage
    
    # muda NaNaN para 0
    ef[is.na(ef)] <- 0
    
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
                          veh_ev[i] , 
                          name_file_evap[1],
                          nmonth[j] ,'_', 
                          'EVAP_NMHC_DF.rds'))
    
    x_STREETS <- emis_post(arra = array_x, 
                           pollutant = veh_ev[j], 
                           by = 'streets') 
    saveRDS(x_STREETS, 
            file = paste0('emi/', 
                          veh_ev[i] ,
                          name_file_evap[1],
                          nmonth[j] ,'_', 
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
                  # g/trip * trip/day * day/km = g/km
                  verbose = verbose)*meta_ev$trips_day[i]/(mileage[[veh_ev[i]]]/365)
  
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
