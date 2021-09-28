# apagando dados ####
a <- list.files(path = "config", pattern = ".rds", full.names = T)
file.remove(a)

# configuracao ####
metadata <- as.data.frame(metadata)
mileage <- as.data.frame(mileage)
mileage[, metadata$vehicles] <- add_lkm(mileage[, metadata$vehicles])/12
# Este proyecto es mensual, entonces hay que pasar quilometraje por ano a mes
tfs <- as.data.frame(tfs)
veh <- as.data.frame(veh)[1:agemax, ]

fuel <- as.data.frame(fuel)
met <- as.data.frame(met)
pmonth <- as.data.frame(pmonth)
euro <- as.data.frame(euro)
tech <- as.data.frame(tech)

# we have fuel for each month
# It suppose that fuel by month is better than monthly profile
# but I will leave this hear to check in the future
for (i in 2:ncol(pmonth)) {
        pmonth[[i]] <- 100 * pmonth[[i]] / sum(pmonth[[i]])
}

# checkar metadata$vehicles ####
switch(language,
        "portuguese" = cat("Metadata$Vehicles é:\n"),
        "english" = cat("Metadata$Vehicles is:\n"),
        "spanish" = cat("Metadata$Vehicles es:\n")
)

# cat( "Metadata$Vehicles é:\n")
print(metadata$vehicles)

# checar nomes mileage ####
if (!length(intersect(metadata$vehicles, names(mileage))) == length(metadata$vehicles)) {
        switch(language,
                "portuguese" = stop(
                        "Precisa adicionar coluna ",
                        setdiff(metadata$vehicles, names(mileage)),
                        " em `mileage`"
                ),
                "english" = stop(
                        "You need to add column ",
                        setdiff(metadata$vehicles, names(mileage)),
                        " in `mileage`"
                ),
                "spanish" = stop(
                        "Necesitas agregar la columna ",
                        setdiff(metadata$vehicles, names(mileage)),
                        " en `mileage`"
                )
        )
}

# checar nomes tfs ####
if (!length(intersect(metadata$vehicles, names(tfs))) == length(metadata$vehicles)) {
        switch(language,
                "portuguese" = stop(
                        "Precisa adicionar coluna ",
                        setdiff(metadata$vehicles, names(mileage)),
                        " em `tfs`"
                ),
                "english" = stop(
                        "You need to add column ",
                        setdiff(metadata$vehicles, names(mileage)),
                        " in `tfs`"
                ),
                "spanish" = stop(
                        "Necesitas agregar la columna ",
                        setdiff(metadata$vehicles, names(mileage)),
                        " en `tfs`"
                )
        )
}

# checar nomes veh ####
if (!length(intersect(metadata$vehicles, names(veh))) == length(metadata$vehicles)) {
        switch(language,
                "portuguese" = stop(
                        "Precisa adicionar coluna ",
                        setdiff(metadata$vehicles, names(mileage)),
                        " em `veh`"
                ),
                "english" = stop(
                        "You need to add column ",
                        setdiff(metadata$vehicles, names(mileage)),
                        " in `veh`"
                ),
                "spanish" = stop(
                        "Necesitas agregar la columna ",
                        setdiff(metadata$vehicles, names(mileage)),
                        " en `veh`"
                )
        )
}

# checar Year ####
if (!"Year" %in% names(veh)) {
        switch(language,
                "portuguese" = stop("Não estou enxergando a coluna 'Year' em `veh`"),
                "english" = stop("I'm not seeing column 'Year' in `veh`"),
                "spanish" = stop("No estoy viendo la columna 'Year' in `veh`")
        )
}

# checar ano base
if (veh$Year[1] != year) {
        switch(language,
                "portuguese" = stop(paste0("O ano base é ", year, " mas o primeiro ano em `veh` é ", veh$Year[1])),
                "english" = stop(paste0("The base year is ", year, " but the first year in `veh` is ", veh$Year[1])),
                "spanish" = stop(paste0("El año base es ", year, " pero el primer año de `veh` es ", veh$Year[1]))
        )
}

switch(language,
        "portuguese" = message("Arquivos em: ", getwd(), "/config/*\n"),
        "english" = message("Files in: ", getwd(), "/config/*\n"),
        "spanish" = message("Archivos en: ", getwd(), "/config/*\n")
)
# checar fuel
if (!"region" %in% names(fuel)) {
        switch(language,
               "portuguese" = stop("Não estou enxergando a coluna 'region' em `fuel`"),
               "english" = stop("I'm not seeing column 'region' in `fuel`"),
               "spanish" = stop("No estoy viendo la columna 'region' in `fuel`")
        )
}

setDT(fuel)
fuel <- fuel[Year == year] #all the months! and all the provinces

saveRDS(metadata, "config/metadata.rds")
saveRDS(mileage, "config/mileage.rds")
saveRDS(tfs, "config/tfs.rds")
saveRDS(veh, "config/fleet_age.rds")
saveRDS(fuel, "config/fuel.rds")
saveRDS(met, "config/met.rds")
saveRDS(pmonth, "config/pmonth.rds")
saveRDS(euro, "config/euro.rds")
saveRDS(tech, "config/tech.rds")

# pastas
if (delete_directories) {
        choice <- 1

        if (language == "portuguese") {
                # choice <- utils::menu(c("Sim", "Não"), title="Apagar pastas csv, emi, images, notes, post e veh??")
                if (choice == 1) {
                        message("Apagando pastas `emi`, `images`, `notes`, `post` e `veh`")
                        unlink("emi", recursive = T)
                        unlink("images", recursive = T)
                        unlink("notes", recursive = T)
                        unlink("post", recursive = T)
                        unlink("veh", recursive = T)
                }
        } else if (language == "english") {
                # choice <- utils::menu(c("Yes", "No"), title="Delete folders `csv`, `emi`, `images`, `notes`, `post` e `veh`??")
                if (choice == 1) {
                        message("Deleting folders `emi`, `images`, `notes`, `post` and `veh`")
                        unlink("emi", recursive = T)
                        unlink("images", recursive = T)
                        unlink("notes", recursive = T)
                        unlink("post", recursive = T)
                        unlink("veh", recursive = T)
                }
        } else if (language == "spanish") {
                # choice <- utils::menu(c("Si", "No"), title="Borrar carpetas `csv`, `emi`, `images`, `notes`, `post` y `veh`??")
                if (choice == 1) {
                        message("Borrando carpetas `emi`, `images`, `notes`, `post` y `veh`")
                        unlink("emi", recursive = T)
                        unlink("notes", recursive = T)
                        unlink("images", recursive = T)
                        unlink("post", recursive = T)
                        unlink("veh", recursive = T)
                }
        }
}

dir.create(path = "emi", showWarnings = FALSE)
dir.create(path = "images", showWarnings = FALSE)
dir.create(path = "notes", showWarnings = FALSE)
dir.create(path = "post", showWarnings = FALSE)
dir.create(path = "veh", showWarnings = FALSE)

switch(language,
        "portuguese" = message("Novas pastas:"),
        "english" = message("New directories:"),
        "spanish" = message("Nuevas carpetas")
)

message("images\nveh\nemi\nnotes\npost\n")

# names groups ####
n_PC <- metadata$vehicles[grep(pattern = "PC", x = metadata$vehicles)]
n_LCV <- metadata$vehicles[grep(pattern = "LCV", x = metadata$vehicles)]
n_TRUCKS <- metadata$vehicles[grep(pattern = "TRUCKS", x = metadata$vehicles)]
n_BUS <- metadata$vehicles[grep(pattern = "BUS", x = metadata$vehicles)]
n_MC <- metadata$vehicles[grep(pattern = "MC", x = metadata$vehicles)]
n_veh <- list(
        PC = n_PC,
        LCV = n_LCV,
        TRUCKS = n_TRUCKS,
        BUS = n_BUS,
        MC = n_MC
)
# Fuel ####

switch(language,
        "portuguese" = cat("Plotando combustivel \n"),
        "english" = cat("Plotting fuel \n"),
        "spanish" = cat("Plotando combustible \n")
)

png("images/FUEL.png", width = 2000, height = 1000, units = "px", res = 300)
ggplot(fuel, 
       aes(x = Month, 
           y = consumption_lt/1000000, 
           colour = fuel,
           shape = fuel)) +
  geom_line() +
  facet_wrap(.~region, 
             scale = "free_y" # comenta esta linea para comparar provincias
             )+
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:12)+
  labs(title = "Ecuador")+
  theme_bw() -> p
print(p)
dev.off()


# Fleet ####
switch(language,
        "portuguese" = cat("Plotando frota \n"),
        "english" = cat("Plotting fleet \n"),
        "spanish" = cat("Plotando flota \n")
)

for (i in seq_along(n_veh)) {
        df_x <- veh[, n_veh[[i]]]
        png(
                paste0(
                        "images/FLEET_",
                        names(n_veh)[i],
                        ".png"
                ),
                2000, 1500, "px",
                res = 300
        )
        colplot(
                df = df_x,
                cols = n_veh[[i]],
                xlab = "Age",
                ylab = "veh/h",
                main = paste0("Fleet ", names(n_veh)[i]),
                type = "l",
                pch = NULL,
                lwd = 1,
                theme = theme,
                spl = 8
        )
        dev.off()
}

# TFS ####

switch(language,
        "portuguese" = cat("Plotando perfis `tfs`\n"),
        "english" = cat("Plotting profiles `tfs`\n"),
        "spanish" = cat("Plotando perfiles `tfs`\n")
)

for (i in seq_along(n_veh)) {
        df_x <- tfs[, n_veh[[i]]]
        png(
                paste0(
                        "images/TFS_",
                        names(n_veh)[i],
                        ".png"
                ),
                2000, 1500, "px",
                res = 300
        )
        colplot(
                df = df_x,
                cols = n_veh[[i]],
                xlab = "Hour",
                ylab = "",
                main = paste0("TFS ", names(n_veh)[i]),
                type = "l",
                pch = NULL,
                lwd = 1,
                theme = theme,
                spl = 8
        )
        dev.off()
}


# Mileage ####

switch(language,
        "portuguese" = cat("Plotando quilometragem \n"),
        "english" = cat("Plotting mileage `tfs`\n"),
        "spanish" = cat("Plotando kilometraje `tfs`\n")
)

for (i in seq_along(n_veh)) {
        df_x <- mileage[, n_veh[[i]]]
        png(
                paste0(
                        "images/MILEAGE_",
                        names(n_veh)[i],
                        ".png"
                ),
                2000, 1500, "px",
                res = 300
        )
        colplot(
                df = df_x,
                cols = n_veh[[i]],
                xlab = "Age of use",
                ylab = "[km/year]",
                main = paste0("Mileage ", names(n_veh)[i]),
                type = "l",
                pch = NULL,
                lwd = 1,
                theme = theme,
                spl = 8
        )
        dev.off()
}


# Temperature ####
units(celsius(1))$numerator
png("images/Temperature.png",
        2000, 1500, "px",
        res = 300
)
colplot(
        df = met,
        cols = "Temperature",
        xlab = "Months",
        ylab = units(celsius(1))$numerator,
        main = "Temperature",
        type = "l",
        pch = NULL,
        lwd = 1,
        theme = theme,
        spl = 8
)
dev.off()

# month
for (i in seq_along(n_veh)) {
        df_x <- pmonth[, n_veh[[i]]]
        png(
                paste0(
                        "images/PMONTH_",
                        names(n_veh)[i],
                        ".png"
                ),
                2000, 1500, "px",
                res = 300
        )
        colplot(
                df = df_x,
                cols = n_veh[[i]],
                xlab = "Month",
                ylab = "%",
                main = names(n_veh)[i],
                type = "l",
                pch = NULL,
                lwd = 1,
                theme = theme,
                spl = 8
        )
        dev.off()
}



# Notes ####
switch(language,
        "portuguese" = cat("\nFazendo anotações\n"),
        "english" = cat("\nTaking some notes\n"),
        "spanish" = cat("\nEscribiendo notas\n")
)

vein_notes(
        notes = c("Default notes for vein::get_project"),
        file = "notes/README",
        title = paste0("Ecuador province ", year),
        approach = "Top-Down",
        traffic = "Statistics",
        composition = "Ecuador",
        ef = "EEA, vehiculos pesados con gasolina, asumidos PC grandes. 
        Vehiculos hibridos asumidos PC Euro 4 (es el unico disponible en los EF).
        Es necesario colocar 0 en estos vehiculos donde no estaban en circulacion",
        cold_start = "NO",
        evaporative = "NO",
        standards = "Euro",
        mileage = "Bruni and Bales 2013, Brazil"
)
# saveRDS

switch(language,
        "portuguese" = message("\nArquivos em:"),
        "english" = message("\nFiles in:"),
        "spanish" = message("\nArchivos en:")
)

message(
        "config/metadata.rds\n",
        "config/mileage.rds\n",
        "config/tfs.rds\n",
        "config/fleet_age.rds\n",
        "config/fuel.rds\n"
)

switch(language,
        "portuguese" = message("\nFiguras em \n"),
        "english" = message("\nFigures in \n"),
        "spanish" = message("\nFiguras en \n")
)
message("/images")

switch(language,
        "portuguese" = message("Limpando..."),
        "english" = message("Cleaning..."),
        "spanish" = message("Limpiando...")
)


suppressWarnings(
        rm(
                i, choice, pa, metadata, po, tfs, veh, mileage, fuel, theme,
                n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC, df_x, ef, cores, vkm, ef, a, rota
        )
)