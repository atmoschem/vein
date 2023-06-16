suppressWarnings(file.remove("emi/DF_WEAR.csv"))
suppressWarnings(file.remove("emi/DF_WEAR_STREET.csv"))


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

wear <- c("tyre", "break", "road")

for (i in seq_along(metadata$vehicles)) {
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )
  
  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
  
      for (j in seq_along(pol)) {
        cat("\n",pol[j], " ")
        
        for (k in seq_along(wear)) {
          cat(wear[k], " ")
      
      ef <- ef_wear(wear = wear[k], 
                    type = metadata$vwear[i],
                    pol = pol[j], 
                    speed = as.data.frame(speed))
      # if(wear[k] == "road") {
      #  efx <- s
      #   efx[] <- ef
      #   ef <- efx
      # }
      
      E <- Emissions(do.call("cbind", 
                             lapply(1:ncol(x), function(i) {
        x[, i] * net$lkm *ef[[1]]
      })))
      
      
      x_DF <- data.frame(
        g = colSums(E, na.rm  = T),
        veh = metadata$vehicles[i],
        size = metadata$t[i],
        fuel = metadata$f[i],
        pollutant = pol[j],
        type_emi = "Exhaust",
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
      
      x_STREETS <- data.frame(
        id = net$id,
        g = rowSums(E, na.rm  = T),
        pollutant = pol[j],
        veh = metadata$vehicles[i],
        size = metadata$t[i],
        fuel = metadata$f[i],
        pollutant = pol[j],
        type_emi = "Exhaust",
        by = "veh"
      )
      
      fwrite(x = x_STREETS, 
             file = "emi/DF_WEAR_STREETS.csv", 
             append = TRUE)
      
    }
  }
  suppressWarnings(rm(array_x, ef,  x_DF, x_STREETS))
  gc()
}
cat("\n")
