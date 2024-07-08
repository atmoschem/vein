file.remove("emi/exhaust.csv")

# switch(language,
#         "portuguese" = cat("Plotando EF\n"),
#         "english" = cat("Plotting EF\n"),
#         "spanish" = cat("Plotando EF\n")
# )
# 
# # emission factors
# n_PC <- metadata[metadata$family == "PC", ]$vehicles
# n_LCV <- metadata[metadata$family == "LCV", ]$vehicles
# n_TRUCKS <- metadata[metadata$family == "TRUCKS", ]$vehicles
# n_BUS <- metadata[metadata$family == "BUS", ]$vehicles
# n_MC <- metadata[metadata$family == "MC", ]$vehicles
# 
# ns <- c("PC", "LCV", "TRUCKS", "BUS", "MC")
# ln <- list(n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC)
# 
# for (i in seq_along(ns)) {
#         dl <- lapply(seq_along(pol), function(j) {
#           
#           if(metadata$v_eea_old[i] %in% c("PC", "LCV", "Motorcycle")) {
#             
#             ef <- ef_ldv_speed(v = metadata$v_eea_old[i],
#                                t = metadata$t_eea_old[i],
#                                cc = metadata$cc_eea_old[i],
#                                f = metadata$fuel_eea_old[i],   
#                                p = "FC",
#                                eu = cate,
#                                speed = Speed(metadata$speed[i]))
#             
#           } else {
#             ef <- ef_hdv_speed(v = metadata$v_eea_old[i],
#                                t = metadata$t_eea_old[i],
#                                g = metadata$cc_eea_old[i],
#                                eu = cate,
#                                gr = 0,
#                                l = 0.5,
#                                p = "FC",
#                                speed = Speed(metadata$speed[i]))
#           }
#           
#                 data.frame(as.vector(ef)[1:40],
#                 pol = pol[j]
#                 )
#         })
# 
#         dl <- do.call("rbind", dl)
#         df <- wide_to_long(
#                 df = dl,
#                 column_with_data = ln[[i]],
#                 column_fixed = "pol"
#         )
#         names(df) <- c("ef", "pol", "veh")
#         df$age <- 1:40
#         p <- ggplot(
#                 df[df$ef > 0, ],
#                 aes(x = age, y = ef, colour = veh)
#         ) +
#                 geom_line() +
#                 facet_wrap(~pol, scales = "free_y") +
#                 ylim(0, NA) +
#                 labs(y = "g/km") +
#                 # scale_y_log10() +
#                 theme_bw()
# 
#         png(
#                 filename = paste0("images/EF_", ns[i], ".png"),
#                 width = 2100, height = 1500, units = "px", pointsize = 12,
#                 bg = "white", res = 300
#         )
#         print(p)
#         dev.off()
# }
# switch(language,
#         "portuguese" = message("\nFiguras em /images\n"),
#         "english" = message("\nFigures in /image\n"),
#         "spanish" = message("\nFiguras en /images\n")
# )

# Escapamento ####
switch(language,
        "portuguese" = cat("Estimando emissões\n"),
        "english" = cat("Estimating emissions\n"),
        "spanish" = cat("Estimando emisiones\n")
)
metadata_original <- metadata
metadata <- metadata[metadata$fuel != "ELEC", ]

# Exhaust ####
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
                        ef = as.vector(ef)[, 1:ncol(x)],
                        # pro_month = pmonth[[metadata$vehicles[i]]],
                        fortran = TRUE,
                        nt = check_nt() / 2,
                        verbose = verbose,
                        params = list(
                                veh = metadata$vehicles[i],
                                size = metadata$size[i],
                                fuel = metadata$fuel[i],
                                pollutant = pol[j],
                                type_emi = "Exhaust",
                                subtype_emi = "Exhaust",
                                baseyear = year,
                                month = 1:12,
                                provincia = toupper(provincia)
                                
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