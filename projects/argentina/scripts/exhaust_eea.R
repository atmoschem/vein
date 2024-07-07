file.remove("emi/exhaust.csv")

# Escapamento ####
switch(language,
       "portuguese" = cat("Estimando emissões\n"),
       "english" = cat("Estimating emissions\n"),
       "spanish" = cat("Estimando emisiones\n")
)

metadata_original <- metadata
metadata <- metadata[metadata$fuel != "ELEC", ]



# Hot Exhaust ####
for (i in seq_along(metadata$vehicles)) {
        
        cat(
                "\n", metadata$vehicles[i],
                rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
        )
        
        x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
        
        # euro
        cate <- as.character(as.roman(gsub("Euro ", "", euro[[metadata$vehicles[i]]])))
        cate[is.na(cate)] <- "PRE"
        
        for (j in seq_along(pol)) {
                cat(pol[j], " ")
                if(metadata$v_eea_old[i] %in% c("PC", "LCV", "Motorcycle")) {
                        
                        ef <- ef_ldv_speed(v = metadata$v_eea_old[i],
                                           t = metadata$t_eea_old[i],
                                           cc = metadata$cc_eea_old[i],
                                           f = if(metadata$fuel_eea_old[i] == "LPG" & pol[j] == "PM") "G" else  metadata$fuel_eea_old[i],  
                                           p = pol[j],
                                           eu = cate,
                                           speed = Speed(metadata$speed[i]))
                        
                        
                        if(metadata$fuel_eea_old[i] == "LPG" & pol[j] == "PM") ef <- ef*0
                        
                } else {
                        ef <- ef_hdv_speed(v = metadata$v_eea_old[i],
                                           t = metadata$t_eea_old[i],
                                           g = metadata$cc_eea_old[i],
                                           eu = cate,
                                           gr = 0,
                                           l = 0.5,
                                           p = pol[j],
                                           speed = Speed(metadata$speed[i]))
                }
                
                array_x <- emis_hot_td(
                        veh = x,
                        lkm = mileage[[metadata$vehicles[i]]],
                        ef = ef[, 1:ncol(x)],
                        fortran = TRUE,
                        nt = check_nt() / 2,
                        pro_month = pmonth[[metadata$vehicles[i]]],
                        verbose = verbose,
                        params = list(
                                veh = metadata$vehicles[i],
                                size = metadata$size[i],
                                fuel = metadata$fuel[i],
                                pollutant = pol[j],
                                type_emi = "Hot",
                                subtype_emi = "Exhaust",
                                baseyear = year
                        )
                )
                
                fwrite(array_x, "emi/exhaust.csv", append = TRUE)
        }
}



# Cold Start ####
cat("Cold Exhaust Fuel Consumption\n ")

metadata_cold <- metadata[metadata$fuel_eea_old %in% "G" &
                                  metadata$v_eea_old %in% c("PC", "LCV"), ]

for (i in seq_along(metadata_cold$vehicles)) {
        
        cat("\n", metadata_cold$vehicles[i],
                rep("", max(nchar(metadata_cold$vehicles) + 1) - nchar(metadata_cold$vehicles[i]))
        )
        
        x <- readRDS(paste0("veh/", metadata_cold$vehicles[i], ".rds"))

        # euro
        cate <- suppressWarnings(
                as.character(as.roman(gsub("Euro ", "", 
                                           euro[[metadata_cold$vehicles[i]]]))))
        cate[is.na(cate)] <- "PRE"
        
        for (j in seq_along(pol)) {
                cat(pol[j], " ")
                
                
                ltrip <- add_lkm(metadata_cold$km_cycle[i])
                ta <- met$value
                a <- cold_mileage(ltrip = ltrip, ta = celsius(ta))
                
                (ef <- ef_ldv_speed(v = metadata_cold$v_eea_old[i],
                                    t = metadata_cold$t_eea_old[i],
                                    cc = metadata_cold$cc_eea_old[i],
                                    f = metadata_cold$fuel_eea_old[i],   
                                    p = "FC",
                                    eu = cate,
                                    speed = Speed(metadata_cold$speed[i])))
                
                (efcold <- ef_ldv_cold(ta = matrix(ta, nrow = 1),
                                       cc = ifelse(metadata_cold$cc_eea_old[i] == "<3.5",
                                                   ">2000",
                                                   metadata_cold$cc_eea_old[i]),
                                       f = metadata_cold$fuel_eea_old[i],   
                                       p = "FC",
                                       eu = cate,
                                       speed = Speed(metadata_cold$speed[i])))
                
                nrow(x) ==  nrow(ef)
                ef$speed <- NULL
                
                array_x <- emis_cold_td(
                        veh = x,
                        lkm = mileage[[metadata_cold$vehicles[i]]],
                        ef = ef[, 1:ncol(x)],
                        efcold = efcold[, 1:ncol(x)],
                        fortran = TRUE,
                        beta = matrix(a, nrow = 1),
                        nt = check_nt() / 2,
                        pro_month = pmonth[[metadata_cold$vehicles[i]]],
                        verbose = verbose,
                        params = list(
                                veh = metadata_cold$vehicles[i],
                                size = metadata_cold$size[i],
                                fuel = metadata_cold$fuel[i],
                                pollutant = pol[j],
                                type_emi = "Cold",
                                subtype_emi = "Exhaust",
                                baseyear = year,
                                month = rep(1:12, each = ncol(x))
                        )
                )
                
                
                fwrite(array_x, "emi/exhaust.csv", append = TRUE)
        }
}


switch(language,
       "portuguese" = message("\n\nArquivos em: /emi/exhaust.csv:"),
       "english" = message("\n\nFiles in: /emi/exhaust.csv"),
       "spanish" = message("\n\nArchivos en: /emi/exhaust.csv")
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