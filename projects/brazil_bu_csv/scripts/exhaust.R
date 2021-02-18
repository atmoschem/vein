# plotting
switch(language,
        "portuguese" = cat("Plotando EF\n"),
        "english" = cat("Plotting EF\n"),
        "spanish" = cat("Plotando EF\n")
)

# fatores de emissão
n_PC <- metadata[metadata$family == "PC", ]$vehicles
n_LCV <- metadata[metadata$family == "LCV", ]$vehicles
n_TRUCKS <- metadata[metadata$family == "TRUCKS", ]$vehicles
n_BUS <- metadata[metadata$family == "BUS", ]$vehicles
n_MC <- metadata[metadata$family == "MC", ]$vehicles

ns <- c("PC", "LCV", "TRUCKS", "BUS", "MC")
ln <- list(n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC)

for (i in seq_along(ns)) {
        dl <- lapply(seq_along(pol), function(j) {
                data.frame(ef_cetesb(
                        p = pol[j], veh = ln[[i]], year = year,
                        agemax = 40, verbose = verbose
                ), pol = pol[j])
        })

        dl <- do.call("rbind", dl)
        df <- wide_to_long(
                df = dl,
                column_with_data = ln[[i]],
                column_fixed = "pol"
        )
        names(df) <- c("ef", "pol", "veh")
        df$age <- 1:40
        p <- ggplot(
                df[df$ef > 0, ],
                aes(x = age, y = ef, colour = veh)
        ) +
                geom_line() +
                facet_wrap(~pol, scales = "free_y") +
                ylim(0, NA) +
                labs(y = "g/km") +
                # scale_y_log10() +
                theme_bw()

        png(
                filename = paste0("images/EF_", ns[i], ".png"),
                width = 2100, height = 1500, units = "px", pointsize = 12,
                bg = "white", res = 300
        )
        print(p)
        dev.off()
}

switch(language,
        "portuguese" = message("\nFiguras em /images\n"),
        "english" = message("\nFigures in /image\n"),
        "spanish" = message("\nFiguras en /images\n")
)

# Escapamento ####
switch(language,
        "portuguese" = cat("Estimando emissões\n"),
        "english" = cat("Estimating emissions\n"),
        "spanish" = cat("Estimando emisiones\n")
)

# unlink
unlink("emi/DF_EXHAUST.csv")
unlink("emi/STREETS_EXHAUST.csv")
for (i in seq_along(metadata$vehicles)) {
        cat(
                "\n", metadata$vehicles[i],
                rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
        )

        x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))

        for (j in seq_along(pol)) {
                cat(pol[j], " ")

                ef <- ef_cetesb(
                        p = pol[j],
                        veh = metadata$vehicles[i],
                        year = year,
                        agemax = ncol(x),
                        verbose = verbose,
                        scale = scale
                )

                array_x <- emis(
                        veh = x,
                        lkm = lkm,
                        ef = ef,
                        profile = tfs[[metadata$vehicles[i]]],
                        fortran = TRUE,
                        nt = check_nt() / 2,
                        simplify = TRUE,
                        verbose = verbose
                )

                x_DF <- emis_post(
                        arra = array_x,
                        veh = metadata$vehicles[i],
                        size = metadata$size[i],
                        fuel = metadata$fuel[i],
                        pollutant = pol[j],
                        type_emi = "Exhaust",
                        by = "veh"
                )


                data.table::fwrite(x_DF,
                        file = "emi/DF_EXHAUST.csv",
                        append = TRUE
                )

                x_STREETS <- emis_post(
                        arra = array_x,
                        pollutant = pol[j],
                        by = "streets"
                )

                x_STREETS$id <- 1:nrow(x_STREETS)
                x_STREETS$veh <- metadata$vehicles[i]
                x_STREETS$size <- metadata$size[i]
                x_STREETS$fuel <- metadata$fuel[i]
                x_STREETS$pollutant <- pol[j]
                x_STREETS$type_emi <- "Exhaust"

                data.table::fwrite(x_STREETS,
                        file = "emi/STREETS_EXHAUST.csv",
                        append = TRUE
                )
        }
        rm(array_x, ef, x, x_DF, x_STREETS)
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

invisible(gc())