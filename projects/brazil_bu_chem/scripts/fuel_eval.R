# calibração combustivel http://dadosenergeticos.energia.sp.gov.br/portalcev2/intranet/PetroGas/index.html

# tfs
tfs <- as.data.frame(tfs)

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
                    nt = check_nt()/2
                    simplify = TRUE,
                    verbose = verbose)
    
    x_DF <- emis_post(arra = array_x, 
                      veh = metadata$vehicles[i], 
                      size = metadata$size[i],
                      fuel = metadata$fuel[i], 
                      pollutant = pol[j],
                      type_emi = "Exhaust",
                      by = 'veh')
    

    saveRDS(x_DF, 
            file = paste0('emi/', 
                          metadata$vehicles[i] ,'/', 
                          metadata$vehicles[i] ,'_', 
                          pol[j], 
                          '_DF.rds'))
    
  }
  rm(array_x, ef, x, x_DF)
}

switch (language,
        "portuguese" = message("\nArquivos em: /emi/*:"),
        "english" = message("\nFiles in: /emi/*"),
        "spanish" = message("\nArchivos en: /emi/*"))

# data.table ####
dt <- data.table::rbindlist(
  lapply(seq_along(pol), function(i){
    pols <- ifelse(pol[i] == "HC", "_HC", pol[i])
    emis_merge(pols, what = 'DF.rds', FALSE, verbose = FALSE)
  })
)
dt$pollutant <- as.character(dt$pollutant)
dt$t <- units::set_units(dt$g, t)

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
        "spanish" = message("Limpiando..."))

suppressWarnings(rm(i, j, pol, dt, dt0, dtf, factor_emi, fuel))

ls()   
invisible(gc())
