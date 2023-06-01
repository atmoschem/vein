year               <- as.numeric(substr(x = getwd(), start = nchar(getwd()) - 6, stop = nchar(getwd()) - 3))

suppressWarnings(file.remove("emi/evaporatives.csv"))
# temperature
t_sc <- split(met, met$scenario)
sc <- names(t_sc)
for(tt in seq_along(t_sc)) {
  met <- t_sc[[tt]]
  
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
  
if(plot_ef){
  
  # plotting
  switch (language,
          "portuguese" = cat("Plotando EF\n"),
          "english" = cat("Plotting EF\n"),
          "spanish" = cat("Plotando EF\n"))
  
  for(i in seq_along(ns)) {
    
    dl <- lapply(
      seq_along(ef_d), function(j){
        data.frame(
          ef_cetesb(
            p = ef_d[j], 
            veh = ln[[i]], 
            year = year,
            scale = scale, 
            agemax = 40, 
            verbose = verbose),
          month = nmonth[j])
      })
    
    dl <- rbindlist(dl)
    df <- wide_to_long(
      df = dl, 
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
  
}

  # Diurnal  ####
  switch (language,
          "portuguese" = cat("\nEmissões evaporativas diurnal\n"),
          "english" = cat("\nEvaporative diurnal emissions\n"),
          "spanish" = cat("\nEmisiones evaporativas diurnal\n"))
  
  for(i in seq_along(veh_ev)) {
    if(verbose) {
      cat("\n", veh_ev[i], 
          rep("", max(nchar(veh_ev) + 1) - nchar(veh_ev[i])))
    }
    
    
    x <- readRDS(paste0("veh/", veh_ev[i], ".rds"))
    
    lef <- lapply(seq_along(te), function(j){
      if(verbose) cat(nmonth[j], " ")
      ef_cetesb(
        p = ef_d[j], 
        veh = veh_ev[i], 
        year = year,
        scale = scale, 
        agemax = ncol(x),
        # diurnal: g/day* day/km= g/km
        verbose = verbose)/(mileage[[veh_ev[i]]]/365) # mean daily mileage
    }) 
    lef <- EmissionFactors(as.data.frame(do.call("rbind", lef)))
    
    # muda NaNaN para 0
    lef[is.na(lef)] <- 0
    
    array_x <- emis_hot_td(
      veh = x,
      lkm = mileage[[veh_ev[i]]], 
      ef = lef, 
      pro_month = as.numeric(pmonth[fuel == meta_ev$fuel[i]]$m3),
      fortran = TRUE, 
      nt = check_nt()*0.9,
      verbose = verbose,
      params = list(
        veh = veh_ev[i],
        size = meta_ev$size[i],
        fuel = meta_ev$fuel[i],
        pollutant = "NMHC",
        type_emi = paste0("Evaporatives ", sc[tt]),
        subtype_emi = "Diurnal",
        baseyear = year))
    
    fwrite(array_x, "emi/evaporatives.csv", append = TRUE)
  } 
  cat("\n")
  
  
  # Running Losses ####
  switch (language,
          "portuguese" = cat("\nEmissões evaporativas running-losses\n"),
          "english" = cat("\nEvaporative running-losses emissions\n"),
          "spanish" = cat("\nEmisiones evaporativas running-loses\n"))
  
  for(i in seq_along(veh_ev)) {
    
    if(verbose) {
      cat("\n", veh_ev[i], 
          rep("", max(nchar(veh_ev) + 1) - nchar(veh_ev[i])))
    }
    
    
    
    x <- readRDS(paste0("veh/", veh_ev[i], ".rds"))
    
    lef <- lapply(seq_along(te), function(j){
      
      if(verbose) cat(nmonth[j], " ")
      ef_cetesb(
        p = ef_rl[j], 
        veh = veh_ev[i], 
        year = year,
        scale = scale, 
        agemax = ncol(x),
        # g/trip * trip/day * day/km = g/km
        verbose = verbose)*meta_ev$trips_day[i]/(mileage[[veh_ev[i]]]/365)
    })
    lef <- EmissionFactors(as.data.frame(do.call("rbind", lef)))
    
    
    # muda NaNaN para 0
    lef[is.na(lef)] <- 0
    
    array_x <- emis_hot_td(
      veh = x,
      lkm = mileage[[veh_ev[i]]], 
      ef = lef, 
      pro_month = as.numeric(pmonth[fuel == meta_ev$fuel[i]]$m3),
      fortran = TRUE,
      nt = check_nt()*0.9,
      verbose = verbose, 
      params = list(
        veh = veh_ev[i],
        size = meta_ev$size[i],
        fuel = meta_ev$fuel[i],
        pollutant = "NMHC",
        type_emi = paste0("Evaporatives ", sc[tt]),
        subtype_emi = "Running Losses",
        baseyear = year))
    
    fwrite(array_x, "emi/evaporatives.csv", append = TRUE)
  }
  
  cat("\n")
  
  
  # Hot Soak ####
  switch (language,
          "portuguese" = cat("\nEmissões evaporativas hot-soak\n"),
          "english" = cat("\nEvaporative hot-soak emissions\n"),
          "spanish" = cat("\nEmisiones evaporativas hot-soak\n"))
  
  for(i in seq_along(veh_ev)) {
    
    
    if(verbose) {
      cat("\n", veh_ev[i], 
          rep("", max(nchar(veh_ev) + 1) - nchar(veh_ev[i])))
    }
    
    
    
    x <- readRDS(paste0("veh/", veh_ev[i], ".rds"))
    
    lef <- lapply(seq_along(te), function(j){
      
      if(verbose) cat(nmonth[j], " ")
      ef_cetesb(
        p = ef_hs[j], 
        veh = veh_ev[i], 
        year = year,
        agemax = ncol(x),
        verbose = verbose)*meta_ev$trips_day[i]/(mileage[[veh_ev[i]]]/365) # quilometragem medio diario
    })
    lef <- EmissionFactors(as.data.frame(do.call("rbind", lef)))
    
    #muda NaNaN para 0
    lef[is.na(lef)] <- 0
    
    array_x <- emis_hot_td(
      veh = x,
      lkm = mileage[[veh_ev[i]]], 
      ef = lef, 
      pro_month = as.numeric(pmonth[fuel == meta_ev$fuel[i]]$m3),
      fortran = TRUE,
      nt = check_nt()*0.9,
      verbose = verbose, 
      params = list(
        veh = veh_ev[i],
        size = meta_ev$size[i],
        fuel = meta_ev$fuel[i],
        pollutant = "NMHC",
        type_emi = paste0("Evaporatives ", sc[tt]),
        subtype_emi = "Hot Soak",
        baseyear = year))
    
    fwrite(array_x, "emi/evaporatives.csv", append = TRUE)
  }
}

cat("\n")


switch (language,
        "portuguese" = message("\n\nArquivos em: /emi/evaporatives.csv:"),
        "english" = message("\n\nFiles in: /emi/evaporatives.csv"),
        "spanish" = message("\n\nArchivos en: /emi/evaporatives.csv"))


switch (language,
        "portuguese" = message("Limpando..."),
        "english" = message("Cleaning..."),
        "spanish" = message("Limpiando..."))
suppressWarnings(
  rm(i, mileage, meta_ev, veh_ev, 
     diurnal_ef, hot_soak_ef, running_losses_ef)
)
# deberiamos ter al menos: "composition" "net"         "veh"         "year"       
ls()   
invisible(gc())
