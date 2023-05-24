suppressWarnings(file.remove("emi/WEAR_DF.csv"))
suppressWarnings(file.remove("emi/WEAR_STREETS.csv"))


switch(language,
       "portuguese" = cat("Estimando emissões\n"),
       "english" = cat("Estimating emissions\n"),
       "spanish" = cat("Estimando emisiones\n")
)


# Wear ####
wear <- c("tyre", "break", "road")

for (i in seq_along(metadata$vehicles)) {
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
  pro <- tfs[[metadata$vehicles[i]]]
  veh <- temp_veh(x = x, tfs = pro)
  
  for (j in seq_along(pol)) {
    cat(" ",pol[j], " ")
    
    for (k in seq_along(wear)) {
      cat(wear[k], " ")
      
      ef <- ef_wear(wear = wear[k], 
                    type = metadata$family[i],
                    pol = pol[j], 
                    speed = metadata$speed[i])
      ef <- rep(ef[[1]], ncol(x))
      
      
      array_x <- emis(
        veh = x,
        lkm = net$lkm,
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
        type_emi = "Wear",
        by = "veh"
      )
      
      
      fwrite(x_DF, 
             "emi/WEAR_DF.csv", 
             append = TRUE)
      
      
      x_STREETS <- emis_post(
        arra = array_x,
        pollutant = veh_ev[j],
        by = "streets"
      )
      
      
      x_STREETS$id <- 1:nrow(net)
      x_STREETS$family <- metadata$family[i]
      x_STREETS$vehicles <- metadata$vehicles[i]
      x_STREETS$fuel <- metadata$fuel[i]
      x_STREETS$pol <- pol[j]
      
      fwrite(x_STREETS, 
             "emi/WEAR_STREETS.csv", 
             append = TRUE)
      
    }
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

gc()