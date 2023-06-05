year_selected               <- as.numeric(substr(x = getwd(), start = nchar(getwd()) - 6, stop = nchar(getwd()) - 3))

year_selected <- 2000
suppressWarnings(file.remove("emi/evaporatives.csv"))

# filtrando veiculos otto
meta_ev <- metadata[metadata$fuel != "D", ]
veh_ev <- meta_ev$vehicles

# temperature
t_sc <- split(met, met$scenario)
sc <- names(t_sc)

reg <- unique(met$region)

switch (language,
        "portuguese" = cat("\nEmissões evaporativas\n"),
        "english" = cat("\nEvaporative emissions\n"),
        "spanish" = cat("\nEmisiones evaporativas\n"))


for(tt in seq_along(t_sc)) {
  met <- t_sc[[tt]]
  
  
  reg <- unique(met$rr)
  
  te <- met$Temperature
  bb <-   (0+15)/2
  cc <- (10+25)/2
  dd <- (20+35)/2 
  
  met$tem <- ifelse(
    te <= bb, "0_15",
    ifelse(
      te > bb & te <= cc, "10_25",
      "20_35"))
  
  # checks name
  type_emis <- c("Diurnal", "Running Losses", "Hot Soak")
  name_file_evap <- c("/DIURNAL_", "/RUNNING_LOSSES_", "/HOT_SOAK_")
  
  met$ef_d  <- paste0("D_", met$tem)
  met$ef_rl <- paste0("R_", met$tem)
  met$ef_hs <- paste0("S_", met$tem)
  
  
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
  
  dd <- c(met$ef_d,
          met$ef_rl,
          met$ef_hs)
  
  tdf <- c(rep("Diurnal", length(met$ef_d)),
           rep("Running Losses", length(met$ef_rl)),
           rep("Hoat Soak", length(met$ef_hs)))
  
  
  
  rbindlist(lapply(
    seq_along(meta_ev$vehicles), function(i){
      
      rbindlist(lapply(
        seq_along(dd), function(j){
          
          df <- as.data.frame(
            matrix(ef_cetesb(
              p = dd[j], 
              veh = meta_ev$vehicles[i], 
              year = year_selected,
              scale = scale, 
              agemax = 40, 
              verbose = verbose), nrow  = 1) )
            
          df$region <- rep(met$region, 3)[j]
          df$month <- rep(met$Month, 3)[j]
          df$veh <- meta_ev$vehicles[i]
          df$te <- rep(met$Temperature, 3)[j]
          df$tdf <- tdf[j]
            df
        }))
    })) -> df
  
 

    
    if(plot_ef){
      
      # plotting
      switch (language,
              "portuguese" = cat("Plotando EF\n"),
              "english" = cat("Plotting EF\n"),
              "spanish" = cat("Plotando EF\n"))
      
      rbindlist(lapply(
        seq_along(meta_ev$vehicles), function(i){
          
          rbindlist(lapply(
            seq_along(dd), function(j){
              data.frame(
                ef = ef_cetesb(
                  p = dd[j], 
                  veh = meta_ev$vehicles[i], 
                  year = year_selected,
                  scale = scale, 
                  agemax = 40, 
                  verbose = verbose),
                month = rep(met$Month, 3)[j],
                veh = meta_ev$vehicles[i],
                te = rep(met$Temperature, 3)[j],
                age = 1:maxage,
                tdf = tdf[j])
            }))
        })) -> dl
      
      # dl
      dl$ef <- as.numeric(dl$ef)
      p <- ggplot(dl[ef > 0, ], 
                  aes(x = age, 
                      y = ef, 
                      colour = as.factor(month))) +
        geom_line() +
        facet_grid(tdf~ veh) +
        ylim(0, NA) +
        labs(title = reg[rr])+
        theme_bw(base_size = 17)
      
      png(filename =  paste0("images/EF_EVAP", 
                             "_",
                             reg[rr],
                             "_",
                             gsub(" ","", sc[tt]),
                             ".png"),
          width = 2100, 
          height = 3500, 
          units ="px", 
          pointsize = 12,
          bg = "white",  
          res = 300)
      print(p)
      dev.off()
      rm(dl)
      gc()
      
      switch (language,
              "portuguese" = message("\nFiguras em /images\n"),
              "english" = message("\nFigures in /image\n"),
              "spanish" = message("\nFiguras en /images\n"))
    }
    

    
    
      for(i in seq_along(veh_ev)) {
        
        x <- readRDS(paste0("veh/", veh_ev[i], ".rds"))
        
        x$region <- NULL
        
        setDF(x)
        
        dm <- pmonth[fuel == meta_ev$fuel[i]]$consumption_t
        dm <- rep(dm/sum(dm), 3)
        
        
        for(eva in seq_along(dd)) {
          
          xx <- ef_cetesb(
            p = dd[eva], 
            veh = veh_ev[i], 
            year = year_selected,
            scale = scale, 
            agemax = ncol(x),
            verbose = verbose)
          
          if(tdf[eva] == "Diurnal") {
            # diurnal: g/day* day/km= g/km
            xx <- xx/(mileage[[veh_ev[i]]]/365) # mean daily mileage
            
          } else {
            # hot soak
            #running losses
            xx <- xx*meta_ev$trips_day[i]/(mileage[[veh_ev[i]]]/365) # quilometragem medio diario
          }
          
          # muda NaNaN para 0
          xx[is.na(xx)] <- 0
          
          array_x <- emis_hot_td(
            veh = x*dm[eva],
            lkm = mileage[[veh_ev[i]]], 
            ef = xx, 
            pro_month = 
            fortran = TRUE, 
            nt = check_nt()*0.9,
            verbose = verbose,
            params = list(
              veh = veh_ev[i],
              size = meta_ev$size[i],
              fuel = meta_ev$fuel[i],
              pollutant = "NMHC",
              type_emi = paste0("Evaporatives ", sc[tt]),
              subtype_emi = tdf[eva],
              baseyear = year_selected))
          
          array_x$region <- reg[rr]
          setDT(array_x)
          array_x <- array_x[, 
                             c("emissions",
                               "rows",
                               "age",
                               "month",
                               "veh",
                               "size",
                               "fuel",
                               "pollutant",
                               "type_emi",
                               "subtype_emi",
                               "baseyear",
                               "region"),
                             with = FALSE]
          fwrite(array_x, "emi/evaporatives.csv", 
                 append = TRUE)
          rm(array_x)
          gc()
        } 
      }
    }
  }  
}
cat("\n")


switch (language,
        "portuguese" = message("\n\nArquivos em: emi/evaporatives.csv:"),
        "english" = message("\n\nFiles in: emi/evaporatives.csv"),
        "spanish" = message("\n\nArchivos en: emi/evaporatives.csv"))

