year               <- as.numeric(substr(x = getwd(), start = nchar(getwd()) - 6, stop = nchar(getwd()) - 3))

suppressWarnings(file.remove("emi/ressus.csv"))

# Exhaust ####
for(i in seq_along(metadata$vehicles)) {
   
   cat("\n", metadata$vehicles[i], 
       rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])))
   
   x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
   
   for(j in seq_along(pol)){
      if(verbose) cat(pol[j], " ")
      
      ef <- ef_cetesb(p = pol[j], 
                      veh = metadata$vehicles[i], 
                      year = year,
                      agemax = ncol(x), 
                      verbose = verbose)
      
      array_x <- emis_hot_td(veh = x, 
                             lkm = mileage[[metadata$vehicles[i]]], 
                             ef = ef, 
                             pro_month = pmonth[[metadata$vehicles[i]]],
                             fortran = TRUE,
			     nt = check_nt()*0.9,
                             verbose = verbose, 
                             params = list(veh = metadata$vehicles[i],
                                           size = metadata$size[i],
                                           fuel = metadata$fuel[i],
                                           pollutant = pol[j],
                                           type_emi = "Paved Roads",
                                           subtype_emi = "Ressuspenssion",
                                           baseyear = year))
      
      fwrite(array_x, "emi/ressus.csv", append = TRUE)
      
   }
}


switch (language,
        "portuguese" = message("\n\nArquivos em: /emi/ressus.csv:"),
        "english" = message("\n\nFiles in: /emi/ressus.csv"),
        "spanish" = message("\n\nArchivos en: /emi/ressus.csv"))


suppressWarnings(
   rm(ADT, 
      PC, LCV, TRUCKS, BUS, MC,
      n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC, 
      tf_PC, tf_LCV, tf_TRUCKS, tf_BUS, tf_MC, 
      emi, g, lkm, 
      metadata,  tfs,veh, 
      net, nveh,  pol, 
      verbose, 
      vk, vkbus, vkhgv, vklcv, vkmc, vkpc, W, x_DF,
      ef_cetesb2, ef_evaps, i, j, k, mileage, name_file_evap, num_vein, 
      q, type_emis, vein_version, x, cores, fuel,
      sL1,sL2, sL3, sL4)
)

