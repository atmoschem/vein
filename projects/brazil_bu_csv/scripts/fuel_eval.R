# calibração combustivel http://dadosenergeticos.energia.sp.gov.br/portalcev2/intranet/PetroGas/index.html

# tfs
tfs <- as.data.frame(tfs)

# Deleting existing DF.csv to avoid appending more emissions and double counting
unlink("emi/FC.csv")
# Escapamento ####
for(i in seq_along(metadata$vehicles)) {
  
  cat("\n", metadata$vehicles[i], 
      rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])))
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
  for(j in seq_along(pol)){
    
    cat(pol[j], " ")
    
    ef <- ef_cetesb(p = pol[j], 
                    veh = metadata$vehicles[i], 
                    year = year,
                    agemax = nrow(x), 
                    verbose = verbose, 
                    scale = scale)
    
    array_x <- emis(veh = x, 
                    lkm = lkm, 
                    ef = ef, 
                    profile = tfs[[metadata$vehicles[i]]], 
                    fortran = TRUE,
                    simplify = TRUE,
                    verbose = verbose)
    
    x_DF <- emis_post(arra = array_x, 
                      veh = metadata$vehicles[i], 
                      size = metadata$size[i],
                      fuel = metadata$fuel[i], 
                      pollutant = pol[j],
                      type_emi = "Exhaust",
                      by = 'veh')
    
    
    fwrite(x = x_DF, 
           file ='emi/FC.csv',
           append = TRUE)
    
  }
  rm(array_x, ef, x, x_DF)
}

switch (language,
        "portuguese" = message("\nArquivo em: /emi/FC.csv:"),
        "english" = message("\nFile in: /emi/FC.csv"),
        "chinese" = message("\n文件位于: /emi/FC.csv"),
        "spanish" = message("\nArchivo en: /emi/FC.csv"))

# data.table ####
dt <- fread("emi/DF.csv")
dt$pollutant <- as.character(dt$pollutant)
dt$t <- units::set_units(units::set_units(dt$g, g), t)

dt0 <- dt[pollutant == "FC", round(sum(t)*factor_emi, 2), by = .(fuel)]
data.table::setkey(dt0, "fuel")

names(dt0)[2] <- "estimation_t" 

dtf <- dt0[fuel]
dtf$density_tm3 <- units::set_units(dtf$density_tm3, "t/m^3")
dtf$consumption_lt <- units::set_units(dtf$consumption_lt, "l")
dtf$consumption_m3 <- units::set_units(dtf$consumption_lt, "m^3")
dtf$consumption_t <- dtf$consumption_m3*dtf$density_tm3
dtf$estimation_consumption <- dtf$estimation_t/dtf$consumption_t
print(dtf)

switch (language,
        "portuguese" = message("Limpando..."),
        "english" = message("Cleaning..."),
        "chinese" = message("清洁用品..."),
        "spanish" = message("Limpiando..."))

suppressWarnings(rm(i, j, pol, dt, dt0, dtf, factor_emi, fuel))

ls()   
gc()
