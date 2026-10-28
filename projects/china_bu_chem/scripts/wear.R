suppressWarnings(file.remove("emi/DF_WEAR.csv"))


switch(language,
       "portuguese" = cat("Estimando emissões\n"),
       "english" = cat("Estimating emissions\n"),
       "spanish" = cat("Estimando emisiones\n")
)


# Wear ####
# metadata <- metadata[51, ]
metadata$vwear <- ifelse(
  metadata$v_eea_2016 == "PC", "PC",
  ifelse(
    metadata$v_eea_2016 == "Motorcycle", "2W","HDV"))

# number of ages of the vehicle matrices written by traffic.R
agemax <- ncol(readRDS(paste0("veh/", metadata$vehicles[1], ".rds")))

s  <- rbindlist(lapply(1:ncol(speed), function(i) {
  as.data.frame(replicate(agemax, speed[, i]))
}))

wear <- c("tyre", "break", "road")

for (i in seq_along(metadata$vehicles)) {
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
  pro <- tfs[[metadata$vehicles[i]]]
  
      for (j in seq_along(pol)) {
        cat("\n",pol[j], " ")
        for (k in seq_along(wear)) {
          cat(wear[k], " ")
      
      ef <- ef_wear(wear = wear[k], 
                    type = metadata$vwear[i],
                    pol = pol[j], 
                    speed = as.data.frame(s))
      if(wear[k] == "road") {
        efx <- s
        efx[] <- ef
        ef <- efx
      }
      array_x <- emis_long(x = x, 
                           lkm = chnet$lkm, 
                           ef = ef,
                           tfs = pro,
                           speed = speed,
                           array = T,
                           verbose = verbose)
      
      x_DF <- emis_post(
        arra = array_x,
        veh = metadata$vehicles[i],
        size = metadata$t[i],
        fuel = metadata$f[i],
        pollutant = pol[j],
        type_emi = wear[k],
        by = "veh"
      )
      
      fwrite(x = x_DF, 
             file = "emi/DF_WEAR.csv", 
             append = TRUE)
      
      # saveRDS(x_DF,
      #         file = paste0(
      #           "emi/",
      #           metadata$vehicles[i], "/",
      #           metadata$vehicles[i], "_",
      #           pol[j],
      #           "_DF.rds"
      #         )
      # )
      
      x_STREETS <- emis_post(
        arra = array_x,
        pollutant = pol[j],
        by = "streets"
      )
      saveRDS(x_STREETS,
              file = paste0(
                "emi/",
                metadata$vehicles[i], "/",
                metadata$vehicles[i], "_",
                pol[j], "_",
                wear[k], # one file per wear type, otherwise tyre and break are overwritten
                "_STREETS.rds"
              )
      )
    }
  }
  suppressWarnings(rm(array_x, ef,  x_DF, x_STREETS))
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

suppressWarnings(rm(i, j, k, s, wear, agemax, pro, x))
invisible(gc())
