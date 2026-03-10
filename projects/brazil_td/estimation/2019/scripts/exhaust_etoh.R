year  <- as.numeric(substr(x = getwd(), 
                           start = nchar(getwd()) - 3, 
                           stop = nchar(getwd()) ))
year_selected <- year

# year_selected <- 2000


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
    
    for(j in seq_along(pol)){
      
      # cat(pol[j])
      
      
      ef <- ef_cetesb(p = pol[j],
                      veh = metadata$vehicles[i],
                      year = year_selected,
                      agemax = ncol(x),
                      sppm = metadata$sppm[i],
                      verbose = verbose)
      
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

