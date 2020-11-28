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
ef_hs <- paste0("S_", tem)

# plot
n_PC <- metadata[metadata$family == "PC", ]$vehicles
n_LCV <- metadata[metadata$family == "LCV", ]$vehicles[1:4]
n_MC <- metadata[metadata$family == "MC", ]$vehicles

ns <- c("PC", "LCV",  "MC",
        "PC", "LCV",  "MC",
        "PC", "LCV",  "MC")
ln  <- list(n_PC, n_LCV, n_MC,
            n_PC, n_LCV, n_MC,
            n_PC, n_LCV, n_MC)
laby <- c("g/day", "g/day", "g/day", 
          "g/trip","g/trip","g/trip",
          "g/trip","g/trip","g/trip")
ev <- c("DIURNAL","DIURNAL","DIURNAL",
        "RUNNING_LOSSES", "RUNNING_LOSSES","RUNNING_LOSSES",
        "HOT_SOAK", "HOT_SOAK", "HOT_SOAK")


# plotting
switch (language,
        "portuguese" = cat("Plotando EF\n"),
        "english" = cat("Plotting EF\n"),
        "spanish" = cat("Plotando EF\n"))

for(i in seq_along(ns)) {
  
  dl <- lapply(seq_along(ef_d), function(j){
    data.frame(ef_cetesb(p = ef_d[j], veh = ln[[i]], year = year,
                         agemax = 40, verbose = verbose),month = nmonth[j])
  })
  
  dl <- rbindlist(dl)
  df <- wide_to_long(df = dl, 
                     column_with_data = ln[[i]], 
                     column_fixed = "month")
  df$age <- 1:40
  setDT(df)
  names(df) <- c("ef", "month", "veh", "age")
  p <- ggplot(df[df$ef > 0, ], 
              aes(x = age, y = ef, colour = veh)) +
    geom_line() +
    facet_wrap(~month) +
    ylim(0, NA) +
    labs(y = laby[i], title = ev[i]) +
    # scale_y_log10() +
    theme_bw()
  
  png(filename =  paste0("images/EF_", ev[i], "_", ns[i], ".png"),
      width = 2100, height = 1500, units ="px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
}

switch (language,
        "portuguese" = message("\nFiguras em /images\n"),
        "english" = message("\nFigures in /image\n"),
        "spanish" = message("\nFiguras en /images\n"))




# Diurnal  ####
switch (language,
        "portuguese" = cat("\nEmissões evaporativas diurnal\n"),
        "english" = cat("\nEvaporative diurnal emissions\n"),
        "chinese" = cat("\n蒸发昼间排放\n"),
        "spanish" = cat("\nEmisiones evaporativas diurnal\n"))

for(i in seq_along(veh_ev)) {
  
  cat("\n", veh_ev[i], 
      rep("", max(nchar(veh_ev) + 1) - nchar(veh_ev[i])))
  
  x <- readRDS(paste0("veh/", veh_ev[i], ".rds"))

    lef <- lapply(seq_along(te), function(j){
      ef_cetesb(p = ef_d[j], 
                      veh = veh_ev[i], 
                      year = year,
                      agemax = ncol(x),
                      # diurnal: g/day* day/km= g/km
                      verbose = verbose)/(mileage[[veh_ev[i]]]/365) # mean daily mileage
    })
    lef <- EmissionFactors(as.data.frame(do.call("rbind", lef)))
    

    # muda NaNaN para 0
    lef[is.na(lef)] <- 0
    
    array_x <- emis_hot_td(veh = x, 
                           lkm = mileage[[metadata$vehicles[i]]], 
                           ef = lef, 
                           pro_month = pmonth[[metadata$vehicles[i]]],
                           fortran = TRUE,
                           verbose = verbose)
    
    x_DF <- emis_post(arra = array_x, 
                      veh = veh_ev[i], 
                      size = metadata$size[i],
                      fuel = metadata$fuel[i], 
                      pollutant = "NMHC", 
                      type_emi = type_emis[1],
                      by = 'veh')
    
    saveRDS(x_DF, 
            file = paste0('emi/', 
                          veh_ev[i] , 
                          name_file_evap[1],
                          veh_ev[i], '_',
                          'EVAP', '_',
                          'NMHC_DF.rds'))
    setDT(array_x)
    
    x_STREETS <- array_x[, sum(emissions), by = .(rows, month)]
    
    saveRDS(x_STREETS, 
            file = paste0('emi/', 
                          veh_ev[i] , 
                          name_file_evap[1],
                          veh_ev[i], '_',
                          'EVAP', '_',
                          'NMHC_STREETS.rds'))
    
    rm(array_x, lef, x, x_DF, x_STREETS)
}



# Running Losses ####
switch (language,
        "portuguese" = cat("\nEmissões evaporativas running-losses\n"),
        "english" = cat("\nEvaporative running-losses emissions\n"),
        "chinese" = cat("\n蒸发流失排放\n"),
        "spanish" = cat("\nEmisiones evaporativas running-loses\n"))

for(i in seq_along(veh_ev)) {
  
  cat("\n", veh_ev[i], 
      rep("", max(nchar(veh_ev) + 1) - nchar(veh_ev[i])))
  
  x <- readRDS(paste0("veh/", veh_ev[i], ".rds"))
  
  lef <- lapply(seq_along(te), function(j){
    ef_cetesb(p = ef_rl[j], 
              veh = veh_ev[i], 
              year = year,
              agemax = ncol(x),
              # g/trip * trip/day * day/km = g/km
              verbose = verbose)*meta_ev$trips_day[i]/(mileage[[veh_ev[i]]]/365)
  })
  lef <- EmissionFactors(as.data.frame(do.call("rbind", lef)))
  
    
    # muda NaNaN para 0
    lef[is.na(lef)] <- 0
    
    array_x <- emis_hot_td(veh = x, 
                           lkm = mileage[[metadata$vehicles[i]]], 
                           ef = lef, 
                           pro_month = pmonth[[metadata$vehicles[i]]],
                           fortran = TRUE,
                           verbose = verbose)
    
    x_DF <- emis_post(arra = array_x, 
                      veh = veh_ev[i], 
                      size = metadata$size[i],
                      fuel = metadata$fuel[i], 
                      pollutant = "NMHC", 
                      type_emi = type_emis[2],
                      by = 'veh')
    
    saveRDS(x_DF, 
            file = paste0('emi/', 
                          veh_ev[i] , 
                          name_file_evap[2],
                          veh_ev[i], '_',
                          'EVAP', '_',
                          'NMHC_DF.rds'))
    setDT(array_x)
    
    x_STREETS <- array_x[, sum(emissions), by = .(rows, month)]
    
    saveRDS(x_STREETS, 
            file = paste0('emi/', 
                          veh_ev[i] , 
                          name_file_evap[2],
                          veh_ev[i], '_',
                          'EVAP', '_',
                          'NMHC_STREETS.rds'))
    
    rm(array_x, lef, x, x_DF, x_STREETS)
}



# Hot Soak ####
switch (language,
        "portuguese" = cat("\nEmissões evaporativas hot-soak\n"),
        "english" = cat("\nEvaporative hot-soak emissions\n"),
        "chinese" = cat("\n蒸发式热浸排放\n"),
        "spanish" = cat("\nEmisiones evaporativas hot-soak\n"))

for(i in seq_along(veh_ev)) {
  
  cat("\n", veh_ev[i], 
      rep("", max(nchar(veh_ev) + 1) - nchar(veh_ev[i])))
  
  
  x <- readRDS(paste0("veh/", veh_ev[i], ".rds"))

  
  lef <- lapply(seq_along(te), function(j){
    ef_cetesb(p = ef_hs[j], 
              veh = veh_ev[i], 
              year = year,
              agemax = ncol(x),
              verbose = verbose)*meta_ev$trips_day[i]/(mileage[[veh_ev[i]]]/365) # quilometragem medio diario
  })
  lef <- EmissionFactors(as.data.frame(do.call("rbind", lef)))
    
  # muda NaNaN para 0
    lef[is.na(lef)] <- 0
    
    array_x <- emis_hot_td(veh = x, 
                           lkm = mileage[[metadata$vehicles[i]]], 
                           ef = lef, 
                           pro_month = pmonth[[metadata$vehicles[i]]],
                           fortran = TRUE,
                           verbose = verbose)
    
    x_DF <- emis_post(arra = array_x, 
                      veh = veh_ev[i], 
                      size = metadata$size[i],
                      fuel = metadata$fuel[i], 
                      pollutant = "NMHC", 
                      type_emi = type_emis[3],
                      by = 'veh')
    
    saveRDS(x_DF, 
            file = paste0('emi/', 
                          veh_ev[i] , 
                          name_file_evap[3],
                          veh_ev[i], '_',
                          'EVAP', '_',
                          'NMHC_DF.rds'))
    setDT(array_x)
    
    x_STREETS <- array_x[, sum(emissions), by = .(rows, month)]
    
    saveRDS(x_STREETS, 
            file = paste0('emi/', 
                          veh_ev[i] , 
                          name_file_evap[3],
                          veh_ev[i], '_',
                          'EVAP', '_',
                          'NMHC_STREETS.rds'))
    
    rm(array_x, lef, x, x_DF, x_STREETS)
}


cat(paste0("Arquivos em ", getwd(), "/emi/*\n"))
cat("Limpando... \n")
suppressWarnings(
rm(i, mileage, meta_ev, veh_ev, year,
   diurnal_ef, hot_soak_ef, running_losses_ef)
)
# deberiamos ter al menos: "composition" "net"         "veh"         "year"       
ls()   
invisible(gc())
