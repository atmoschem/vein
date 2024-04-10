year  <- as.numeric(substr(x = getwd(), 
                           start = nchar(getwd()) - 3, 
                           stop = nchar(getwd()) ))
year_selected <- year

# year_selected <- 2000

# fatores de emissão
n_PC <- metadata[metadata$family == "PC", ]$vehicles
n_LCV <- metadata[metadata$family == "LCV", ]$vehicles
n_TRUCKS <- metadata[metadata$family == "TRUCKS", ]$vehicles
n_BUS <- metadata[metadata$family == "BUS", ]$vehicles
n_MC <- metadata[metadata$family == "MC", ]$vehicles

ns <- c("PC", "LCV", "TRUCKS", "BUS", "MC")
ln <- list(n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC)

suppressWarnings(file.remove("emi/exhaust.csv"))
if(plot_ef){
  
  
  # plotting
  switch(language,
         "portuguese" = cat("Plotando EF\n"),
         "english" = cat("Plotting EF\n"),
         "spanish" = cat("Plotando EF\n")
  )
  
  # fatores de emissão
  n_PC <- metadata[metadata$family == "PC", ]$vehicles
  n_LCV <- metadata[metadata$family == "LCV", ]$vehicles
  n_TRUCKS <- metadata[metadata$family == "TRUCKS", ]$vehicles
  n_BUS <- metadata[metadata$family == "BUS", ]$vehicles
  n_MC <- metadata[metadata$family == "MC", ]$vehicles
  
  ns <- c("PC", "LCV", "TRUCKS", "BUS", "MC")
  ln <- list(n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC)
  
  for (i in seq_along(ns)) {
    dl <- lapply(seq_along(pol), function(j) {
      
      if(pol[j] == "SO2") {
        cat("SO2\n")  
        dfef <- data.frame(ef_cetesb(
          p = "FC",
          veh = ln[[i]],
          year = year,
          agemax = 40,
          verbose = verbose))*s[ln[[i]]]* 2 * 1e-06
        
      } else {
        dfef <- data.frame(ef_cetesb(
          p = pol[j],
          veh = ln[[i]],
          year = year,
          agemax = 40,
          verbose = verbose))
        
        
        if(ns[i] == "LCV") {
          
          if(pol[j] == "CO") {
            
            dfef[["LCV_D"]] <- dfef[["LCV_D"]]*factors[factors$veh == "LCV_D" & 
                                                         factors$pol == "CO", ]$factor
            
          } else if (pol[j] %in% c("NO", "NO2", "NOx")){
            
            dfef[["LCV_D"]] <- dfef[["LCV_D"]]*factors[factors$veh == "LCV_D" & 
                                                         factors$pol == "NOX", ]$factor
            
          } else if(pol[j] %in% c("HC", "NMHC", "ETOH")) {
            
            dfef[["LCV_D"]] <- dfef[["LCV_D"]]*factors[factors$veh == "LCV_D" & 
                                                         factors$pol == "HC", ]$factor
            
          } else if(pol[j] == "PM"){
            dfef[["LCV_D"]] <- dfef[["LCV_D"]]*factors[factors$veh == "LCV_D" & 
                                                         factors$pol == "PM", ]$factor
            
          }
          
        } else if(ns[i] %in% c("BUS", "TRUCKS")){
          
          if(pol[j] == "CO") {
            
            dfef <- dfef*factors[factors$veh == "HDV" & 
                                   factors$pol == "CO", ]$factor
            
          } else if (pol[j] %in% c("NO", "NO2", "NOx")){
            
            dfef <- dfef*factors[factors$veh == "HDV" & 
                                   factors$pol == "NOX", ]$factor
            
          } else if(pol[j] %in% c("HC", "NMHC", "ETOH")) {
            
            dfef <- dfef*factors[factors$veh == "HDV" & 
                                   factors$pol == "HC", ]$factor
            
          } else if(pol[j] == "PM") {
            
            dfef <- dfef*factors[factors$veh == "HDV" & 
                                   factors$pol == "PM", ]$factor
            
          }
          
          
        }
        
        
        
      }
      dfef$pol <- pol[j]
      dfef
    })
    
    dl <- do.call("rbind", dl)
    df <- wide_to_long(
      df = dl,
      column_with_data = ln[[i]],
      column_fixed = "pol"
    )
    names(df) <- c("ef", "pol", "veh")
    df$age <- 1:40
    p <- ggplot(
      df[df$ef > 0, ],
      aes(x = age, y = ef, colour = veh)
    ) +
      geom_line() +
      facet_wrap(~pol, scales = "free_y") +
      ylim(0, NA) +
      labs(y = "g/km") +
      # scale_y_log10() +
      theme_bw()
    
    png(
      filename = paste0("images/EF_", ns[i], ".png"),
      width = 2100, height = 1500, units = "px", pointsize = 12,
      bg = "white", res = 300
    )
    print(p)
    dev.off()
  }
  
  
  switch (language,
          "portuguese" = message("\nFiguras em /images\n"),
          "english" = message("\nFigures in /image\n"),
          "spanish" = message("\nFiguras en /images\n"))
  
}

# Escapamento ####
switch (language,
        "portuguese" = cat("Estimando emissões\n"),
        "english" = cat("Estimating emissions\n"),
        "spanish" = cat("Estimando emisiones\n"))


reg <- unique(veh$region)

# Exhaust ####
for(k in seq_along(reg)) {
  
  cat(reg[k],  " ")
  
  
  for(i in seq_along(metadata$vehicles)) {
    
    # cat("\n", metadata$vehicles[i],
    #     rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])))
    
    
    x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
    
    x[is.na(x)] <- 0
    
    x <- x[region == reg[k], ]
    
    x$region <- NULL
    
    setDF(x)
    
    dm <- pmonth[region == reg[k] &
                   fuel == metadata$fuel[i]]$consumption_t
    
    for (j in seq_along(pol)) {
      cat(pol[j], " ")
      
      if(pol[j] == "SO2") {
        
        ef <- ef_cetesb(
          p = "FC",
          veh = metadata$vehicles[i],
          year = year,
          agemax = ncol(x),
          verbose = verbose,
          scale = scale
        )*s[[metadata$vehicles[i]]] * 2 * 1e-06
        
      } else {
        ef <- ef_cetesb(
          p = pol[j],
          veh = metadata$vehicles[i],
          year = year,
          agemax = ncol(x),
          verbose = verbose,
          scale = scale
        )
        
        
        
        
        if(metadata$vehicles[i] == "LCV_D") {
          
          if(pol[j] == "CO") {
            
            ef <- ef*factors[factors$veh == "LCV_D" & 
                               factors$pol == "CO", ]$factor
            
          } else if (pol[j] %in% c("NO", "NO2", "NOx")){
            
            ef <- ef*factors[factors$veh == "LCV_D" & 
                               factors$pol == "NOX", ]$factor
            
          } else if(pol[j] %in% c("HC", "NMHC", "ETOH")) {
            
            ef <- ef*factors[factors$veh == "LCV_D" & 
                               factors$pol == "HC", ]$factor
            
          } else if(pol[j] == "PM"){
            
            ef <- ef*factors[factors$veh == "LCV_D" & 
                               factors$pol == "PM", ]$factor
            
          }
          
        } else if(metadata$vehicles[i] %in% c("TRUCKS_SL_D",
                                              "TRUCKS_L_D",
                                              "TRUCKS_M_D",
                                              "TRUCKS_SH_D",
                                              "TRUCKS_H_D",
                                              "BUS_URBAN_D",
                                              "BUS_MICRO_D",
                                              "BUS_COACH_D"
        )){
          
          if(pol[j] == "CO") {
            
            ef <- ef*factors[factors$veh == "HDV" & 
                               factors$pol == "CO", ]$factor
            
          } else if (pol[j] %in% c("NO", "NO2", "NOx")){
            
            ef <- ef*factors[factors$veh == "HDV" & 
                               factors$pol == "NOX", ]$factor
            
          } else if(pol[j] %in% c("HC", "NMHC", "ETOH")) {
            
            ef <- ef*factors[factors$veh == "HDV" & 
                               factors$pol == "HC", ]$factor
            
          } else if(pol[j] == "PM") {
            
            ef <- ef*factors[factors$veh == "HDV" & 
                               factors$pol == "PM", ]$factor
            
          }
          
          
        }
        
        
        
      }
      
      
      
      array_x <- emis_hot_td(
        veh = x,
        lkm = mileage[[metadata$vehicles[i]]],
        ef = ef,
        fortran = TRUE,
        pro_month = dm,
        verbose = verbose,
        params = list(veh = metadata$vehicles[i],
                      size = metadata$size[i],
                      fuel = metadata$fuel[i],
                      pollutant = pol[j],
                      type_emi = "Exhaust",
                      subtype_emi = "Exhaust",
                      baseyear = year_selected))
      
      array_x$region <- reg[k]
      
      fwrite(array_x, "emi/exhaust.csv", append = TRUE)
      rm(array_x)
      gc()
    }
  }
}

switch (language,
        "portuguese" = message("\n\nArquivos em: emi/exhaust.csv:"),
        "english" = message("\n\nFiles in: emi/exhaust.csv"),
        "spanish" = message("\n\nArchivos en: emi/exhaust.csv"))


switch (language,
        "portuguese" = message("Limpando..."),
        "english" = message("Cleaning..."),
        "spanish" = message("Limpiando..."))

