
suppressWarnings(file.remove("emi/cold.csv"))

# det_factor() and apply_det()
source("scripts/deterioration.R", encoding = "UTF-8")

# Escapamento ####
switch(language,
       "portuguese" = cat("Estimando emissões Cold Start\n"),
       "english" = cat("Estimating emissions Cold Start\n"),
       "spanish" = cat("Estimando emisiones Cold Start\n")
)


# Cold Start ####

metadata_cold <- metadata[metadata$fuel_eea_old %in% "G" &
                            metadata$v_eea_old %in% c("PC", "LCV"), ]

# remove stale COLD_* files from vehicles without cold start ####
cold_stale <- list.files(
  path = "emi", pattern = "^COLD_", recursive = TRUE, full.names = TRUE
)
cold_stale <- cold_stale[
  !basename(dirname(cold_stale)) %in% metadata_cold$vehicles
]
if (length(cold_stale) > 0) {
  file.remove(cold_stale)
  switch(
    language,
    "portuguese" = message(
      "\nCOLD antigos removidos: ", length(cold_stale), "\n"
    ),
    "english" = message(
      "\nStale COLD files removed: ", length(cold_stale), "\n"
    ),
    "spanish" = message(
      "\nArchivos COLD antiguos eliminados: ", length(cold_stale), "\n"
    )
  )
}

for (i in seq_along(metadata_cold$vehicles)) {
  
  cat("\n", metadata_cold$vehicles[i],
      rep("", max(nchar(metadata_cold$vehicles) + 1) - nchar(metadata_cold$vehicles[i]))
  )
  
  veh <- readRDS(paste0("veh/", metadata_cold$vehicles[i], ".rds"))
  
  # euro
  cate <- suppressWarnings(
    as.character(as.roman(gsub("Euro ", "", 
                               euro[[metadata_cold$vehicles[i]]]))))
  cate[is.na(cate)] <- "PRE"
  
  for (j in seq_along(pol)) {
    
    # local names that are not columns of fcorr (data.table scoping)
    pj <- pol[j]
    vj <- metadata_cold$v_eea_old_fuel[i]
    f_fcorr <- fcorr[veh == vj & pol == pj]
    
    # deterioration factor, one per age (scripts/deterioration.R)
    efdet <- det_factor(
      p = pol[j],
      family = metadata_cold$family[i],
      fuel = metadata_cold$fuel_eea_old[i],
      cc = metadata_cold$cc_eea_old[i],
      eu = cate[1:ncol(veh)],
      km = cumsum(mileage[[metadata_cold$vehicles[i]]])[1:ncol(veh)]
    )
    
    cat(pol[j], " ")
    
    ltrip <- add_lkm(metadata_cold$km_cycle[i])
    ta <- met$value
    a <- cold_mileage(ltrip = ltrip, ta = celsius(ta))
    
    ef <- lapply(seq_along(cate), function(eu) {
      ef_ldv_speed(v = metadata_cold$v_eea_old[i],
                       t = metadata_cold$t_eea_old[i],
                       cc = metadata_cold$cc_eea_old[i],
                       f = if(metadata_cold$fuel_eea_old[i] == "LPG" & pol[j] == "PM") "G" else  metadata_cold$fuel_eea_old[i],  
                       p = pol[j],
                       eu = cate[eu],
                       # speed = Speed(metadata$speed[i]),
                       fcorr = if(nrow(f_fcorr) == 0) rep(1, 8) else f_fcorr$value)
      })
    # applied across the age columns; `veh` itself is not modified
    veh_j <- apply_det(veh, efdet)
    
    # cold/hot ratios exist for NOx and HC: NO and NO2 use the NOx ratio,
    # NMHC the HC ratio (the hot EF above is already pollutant specific)
    poly <- ifelse(
      pol[j] %in% c("NO", "NO2"), "NOx",
      ifelse(
        pol[j] %in% c("NMHC"), "HC",
        pol[j]
      ))
    
    
    efcold <- lapply(seq_along(cate), function(eu) {
      ef_ldv_cold(ta = ta,
                          cc = ifelse(metadata_cold$cc_eea_old[i] == "<3.5",
                                      ">2000",
                                      metadata_cold$cc_eea_old[i]),
                          f = metadata_cold$fuel_eea_old[i],   
                          p = poly,
                          eu = cate[eu],
                          # speed = Speed(metadata_cold$speed[i]),
                          fcorr = if(nrow(f_fcorr) == 0) rep(1, 8) else f_fcorr$value)
    })
    nrow(veh) ==  nrow(ef)
    ef$speed <- NULL
    
    if(IM) {
      # ef is a list of speed functions here, so the IM correction used in the
      # top-down projects (ef * ok + ef * fim * nok) cannot be applied as is
      stop("Aun no ha sido implementado el efecto de IM :(")
    }
    
    
    array_x <- emis_cold(
      veh = veh_j, 
      lkm = net$lkm, 
      ef = ef, 
      efcold = efcold, 
      beta = matrix(a, ncol= ncol(veh)), 
      speed = speed, 
      agemax = 40, 
      simplify = T,
      profile = tfs[[metadata_cold$vehicles[i]]], 
    )
    
    x_DF <- emis_post(
      arra = array_x,
      veh = metadata_cold$vehicles[i],
      size = metadata_cold$size[i],
      fuel = metadata_cold$fuel[i],
      pollutant = pol[j],
      type_emi = "Cold",
      by = "veh"
    )
    
    fwrite(x_DF, "emi/cold.csv", append = TRUE)
    
    x_ST <- emis_post(
      arra = array_x,
      veh = metadata_cold$vehicles[i],
      size = metadata_cold$size[i],
      fuel = metadata_cold$fuel[i],
      pollutant = pol[j],
      type_emi = "Cold",
      by = "streets"
    )
    
    x_ST$fuel <- metadata_cold$fuel[i]
    
    fwrite(x_ST, paste0("emi/", metadata_cold$vehicles[i], "/COLD_", pol[j], ".csv"), append = FALSE)
    
    
  }
}


switch(language,
       "portuguese" = message("\nEmissões em: /emi/exhaust.csv:"),
       "english" = message("\nEmissions in: /emi/exhaust.csv"),
       "spanish" = message("\nEmisiones en: /emi/exhaust.csv")
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

ls()


invisible(gc())