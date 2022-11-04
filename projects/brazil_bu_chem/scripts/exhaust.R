suppressWarnings(file.remove("emi/EXHAUST_DF.csv"))
suppressWarnings(file.remove("emi/EXHAUST_STREETS.csv"))

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

switch(language,
       "portuguese" = message("\nFiguras em /images\n"),
       "english" = message("\nFigures in /image\n"),
       "spanish" = message("\nFiguras en /images\n")
)

# Escapamento ####
switch(language,
       "portuguese" = cat("Estimando emissões\n"),
       "english" = cat("Estimating emissions\n"),
       "spanish" = cat("Estimando emisiones\n")
)



for (i in seq_along(metadata$vehicles)) {
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
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
      
    }
    
    
    
    if(IM) {
      im_ok <- readRDS("config/im_ok.rds")
      im_co <- readRDS("config/im_co.rds")
      im_hc <- readRDS("config/im_hc.rds")
      im_nox <- readRDS("config/im_nox.rds")
      im_pm <- readRDS("config/im_pm.rds")
      
      #IM
      ok <- im_ok[[metadata$vehicles[i]]][1:ncol(x)]
      nok <- 1 - ok
      
      if(pol[j] == "CO") fim <- im_co[[metadata$vehicles[i]]][1:ncol(x)]
      
      if(pol[j] %in% c("HC", "NMHC")) fim <- im_hc[[metadata$vehicles[i]]][1:ncol(x)]
      
      if(pol[j] %in% c("NO", "NO2", "NOx")) fim <- im_nox[[metadata$vehicles[i]]][1:ncol(x)]
      
      if(pol[j] %in% c("PM")) fim <- im_pm[[metadata$vehicles[i]]][1:ncol(x)]
      
      # check that fim is only 1
      
      if(length(unique(fim)) == 1){
        if(unique(fim) == 1){
          ok <- 1                                
        }
      } 
      
      ef <- as.numeric(ef)*ok + as.numeric(ef)*fim*nok
      ef[(length(ef)-8):length(ef)] <- max(ef[(length(ef)-8):length(ef)])
      ef <- EmissionFactors(ef)
    }
    
    array_x <- emis(
      veh = x,
      lkm = lkm,
      ef = ef,
      profile = tfs[[metadata$vehicles[i]]],
      fortran = TRUE,
      nt = check_nt() / 2,
      simplify = TRUE,
      verbose = verbose
    )
    
    x_DF <- emis_post(
      arra = array_x,
      veh = metadata$vehicles[i],
      size = metadata$size[i],
      fuel = metadata$fuel[i],
      pollutant = pol[j],
      type_emi = "Exhaust",
      by = "veh"
    )
    
    fwrite(x_DF, 
           "emi/EXHAUST_DF.csv", 
           append = TRUE)

    x_STREETS <- emis_post(
      arra = array_x,
      pollutant = pol[j],
      by = "streets"
    )
    
    x_STREETS$id <- 1:nrow(net)
    x_STREETS$family <- metadata$family[i]
    x_STREETS$vehicles <- metadata$vehicles[i]
    x_STREETS$fuel <- metadata$fuel[i]
    x_STREETS$pol <- pol[j]
    
    fwrite(x_STREETS, 
           "emi/EXHAUST_STREETS.csv", 
           append = TRUE)
    
  }
  rm(array_x, ef, x, x_DF, x_STREETS)
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