suppressWarnings(file.remove("emi/WEAR.csv"))

year_select  <- as.numeric(substr(x = getwd(), 
                                  start = nchar(getwd()) - 6, 
                                  stop = nchar(getwd()) - 3))


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
      
      
      array_x <- emis_hot_td(veh = x, 
                             lkm = mileage[[metadata$vehicles[i]]], 
                             ef = ef, 
                             pro_month = as.numeric(pmonth[fuel == metadata$fuel[i]]$m3),
                             fortran = TRUE,
                             nt = check_nt()*0.9, 
                             verbose = verbose, 
                             params = list(veh = metadata$vehicles[i],
                                           size = metadata$size[i],
                                           fuel = metadata$fuel[i],
                                           pollutant = pol[j],
                                           type_emi = "Wear",
                                           subtype_emi = wear[k],
                                           baseyear = year_select))
      
      fwrite(array_x, "emi/wear.csv", append = TRUE)
      
      
    }
  }
  rm(array_x, ef)
  gc()
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