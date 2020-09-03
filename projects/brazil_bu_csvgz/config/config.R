# apagando dados ####
a <- list.files(path = "config", pattern = ".rds", full.names = T)
file.remove(a)

# configuracao ####
metadata <- as.data.frame(metadata)
mileage <- as.data.frame(mileage)
mileage[, metadata$vehicles] <- add_lkm(mileage[, metadata$vehicles])
tfs <- as.data.frame(tfs)
veh <- as.data.frame(veh)
fuel <- as.data.frame(fuel)
met <- as.data.frame(met)

# checkar metadata$vehicles ####
switch (language,
        "portuguese" = cat( "Metadata$Vehicles é:\n"),
        "english" = cat( "Metadata$Vehicles is:\n"),
        "spanish" = cat( "Metadata$Vehicles es:\n"))

# cat( "Metadata$Vehicles é:\n")
print(metadata$vehicles)

# checar nomes mileage ####
if(!length(intersect(metadata$vehicles, names(mileage))) == length(metadata$vehicles)) {
  switch (language,
          "portuguese" = stop("Precisa adicionar coluna ",
                              setdiff(metadata$vehicles, names(mileage)),
                              " em `mileage`"),
          "english" = stop("You need to add column ",
                           setdiff(metadata$vehicles, names(mileage)),
                           " in `mileage`"),
           "spanish" = stop("Necesitas agregar la columna ",
                           setdiff(metadata$vehicles, names(mileage)),
                           " en `mileage`"))
  
}

# checar nomes tfs ####
if(!length(intersect(metadata$vehicles, names(tfs))) == length(metadata$vehicles)) {
  switch (language,
          "portuguese" = stop("Precisa adicionar coluna ",
                              setdiff(metadata$vehicles, names(mileage)),
                              " em `tfs`"),
          "english" = stop("You need to add column ",
                           setdiff(metadata$vehicles, names(mileage)),
                           " in `tfs`"),
          "spanish" = stop("Necesitas agregar la columna ",
                           setdiff(metadata$vehicles, names(mileage)),
                           " en `tfs`"))
}

# checar nomes veh ####
if(!length(intersect(metadata$vehicles, names(veh))) == length(metadata$vehicles)) {
  switch (language,
          "portuguese" = stop("Precisa adicionar coluna ",
                              setdiff(metadata$vehicles, names(mileage)),
                              " em `veh`"),
          "english" = stop("You need to add column ",
                           setdiff(metadata$vehicles, names(mileage)),
                           " in `veh`"),
           "spanish" = stop("Necesitas agregar la columna ",
                           setdiff(metadata$vehicles, names(mileage)),
                           " en `veh`"))
}

#checar Year ####
if(!"Year" %in% names(veh)){
  switch (language,
          "portuguese" = stop("Não estou enxergando a coluna 'Year' em `veh`"),
          "english" = stop("I'm not seeing column 'Year' in `veh`"),
          "spanish" = stop("No estoy viendo la columna 'Year' in `veh`"))
  
} 
if(!"Year" %in% names(mileage)) {
  switch (language,
          "portuguese" = stop("Não estou enxergando a coluna 'Year' em `mileage`"),
          "english" = stop("I'm not seeing column 'Year' in `mileage`"),
          "spanish" = stop("No estoy viendo la columna 'Year' in `mileage`"))
  
}

# checar ano base
if(veh$Year[1] != year) {
  switch (language,
          "portuguese" = stop(paste0("O ano base é ", year, " mas o primeiro ano em `veh` é ", veh$Year[1])),
          "english" = stop(paste0("The base year is ", year, " but the first year in `veh` is ", veh$Year[1])),
          "spanish" = stop(paste0("El año base es ", year, " pero el primer año de `veh` es ", veh$Year[1])))
  
}
if(mileage$Year[1] != year) {
  switch (language,
          "portuguese" = stop(paste0("O ano base é ", year, " mas o primeiro ano em `mileage` é ", mileage$Year[1])),
          "english" = stop(paste0("The base year is ", year, " but the first year in `mileage` is ", veh$Year[1])),
          "spanish" = stop(paste0("El año base es ", year, " pero el primer año de `mileage` es ", mileage$Year[1])))
  
}


switch (language,
        "portuguese" = message("Arquivos em: ", getwd(), "/config/*\n"),
        "english" = message("Files in: ", getwd(), "/config/*\n"),
        "spanish" = message("Archivos en: ", getwd(), "/config/*\n"))

saveRDS(metadata, "config/metadata.rds")
saveRDS(mileage, "config/mileage.rds")
saveRDS(tfs, "config/tfs.rds")
saveRDS(veh, "config/fleet_age.rds")
saveRDS(fuel, "config/fuel.rds")
saveRDS(met, "config/met.rds")

# pastas
if(delete_directories){
  choice <- 1
  
  if(language == "portuguese") {
    # choice <- utils::menu(c("Sim", "Não"), title="Apagar pastas csv, emi, images, notes, post e veh??")
    if(choice == 1){
      message("Apagando pastas `emi`, `images`, `notes`, `post` e `veh`")
      unlink("csv", recursive = T)
      unlink("emi", recursive = T)
      unlink("images", recursive = T)
      unlink("notes", recursive = T)
      unlink("post", recursive = T)
      unlink("veh", recursive = T)
    } 
  } else if(language == "english"){
    # choice <- utils::menu(c("Yes", "No"), title="Delete folders `csv`, `emi`, `images`, `notes`, `post` e `veh`??")
    if(choice == 1){
      message("Deleting folders `emi`, `images`, `notes`, `post` and `veh`")
      unlink("csv", recursive = T)
      unlink("emi", recursive = T)
      unlink("images", recursive = T)
      unlink("notes", recursive = T)
      unlink("post", recursive = T)
      unlink("veh", recursive = T)
    } 
    

  } else if(language == "spanish"){
    # choice <- utils::menu(c("Si", "No"), title="Borrar carpetas `csv`, `emi`, `images`, `notes`, `post` y `veh`??")
    if(choice == 1){
      message("Borrando carpetas `emi`, `images`, `notes`, `post` y `veh`")
      unlink("csv", recursive = T)
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
dir.create(path = "post/datatable", showWarnings = FALSE)
dir.create(path = "post/streets", showWarnings = FALSE)
dir.create(path = "post/grids", showWarnings = FALSE)
dir.create(path = "veh", showWarnings = FALSE)

# for(i in seq_along(metadata$vehicles)) dir.create(path = paste0("emi/", metadata$vehicles[i]))


pa <- list.dirs(path = "emi", full.names = T, recursive = T)
po <- list.dirs("post", full.names = T, recursive = T)

switch (language,
        "portuguese" = message("Novas pastas:"),
        "english" = message("New folders:"),
        "spanish" = message("Nuevas carpetas"))

message("csv\n")
message("images\n")
message(paste0(po,"\n"))
message(paste0(pa,"\n"))
message("veh\n")

# names groups ####
n_PC <- metadata$vehicles[grep(pattern = "PC", x = metadata$vehicles)]
n_LCV <- metadata$vehicles[grep(pattern = "LCV", x = metadata$vehicles)]
n_TRUCKS <- metadata$vehicles[grep(pattern = "TRUCKS", x = metadata$vehicles)]
n_BUS <- metadata$vehicles[grep(pattern = "BUS", x = metadata$vehicles)]
n_MC <- metadata$vehicles[grep(pattern = "MC", x = metadata$vehicles)]
n_veh <- list(PC = n_PC, 
              LCV = n_LCV, 
              TRUCKS = n_TRUCKS, 
              BUS = n_BUS, 
              MC = n_MC)

# Fleet ####
switch (language,
        "portuguese" = cat("Plotando frota \n"),
        "english" = cat("Plotting fleet \n"),
        "spanish" = cat("Plotando flota \n"))

for(i in seq_along(n_veh)) {
  df_x <- veh[, n_veh[[i]]]
  png(
    paste0("images/FLEET_", 
           names(n_veh)[i],
           ".png"), 
    2000, 1500, "px",res = 300)
  colplot(df = df_x,
          cols = n_veh[[i]],
          xlab = "Age",
          ylab = "veh/h",
          main = names(n_veh)[i],
          type = "l",
          pch = NULL,
          lwd =1,
          theme = theme,
          spl = 8)
  dev.off()
}

#TFS ####

switch (language,
        "portuguese" = cat("Plotando perfis `tfs`\n"),
        "english" = cat("Plotting profiles `tfs`\n"),
        "spanish" = cat("Plotando perfiles `tfs`\n"))

for(i in seq_along(n_veh)) {
  df_x <- tfs[, n_veh[[i]]]
  png(
    paste0("images/TFS_", 
           names(n_veh)[i],
           ".png"), 
    2000, 1500, "px",res = 300)
  colplot(df = df_x,
          cols = n_veh[[i]],
          xlab = "Hour",
          ylab = "",
          main = paste0("TFS ", names(n_veh)[i]),
          type = "l",
          pch = NULL,
          lwd =1,
          theme = theme,
          spl = 8)
  dev.off()
}


# Mileage ####

switch (language,
        "portuguese" = cat("Plotando quilometragem \n"),
        "english" = cat("Plotting mileage `tfs`\n"),
        "spanish" = cat("Plotando kilometraje `tfs`\n"))

for(i in seq_along(n_veh)) {
  df_x <- veh[, n_veh[[i]]]
  png(
    paste0("images/MILEAGE_", 
           names(n_veh)[i],
           ".png"), 
    2000, 1500, "px",res = 300)
  colplot(df = df_x,
          cols = n_veh[[i]],
          xlab = "Age of use",
          ylab = "[km/year]",
          main = paste0("Mileage ", names(n_veh)[i]),
          type = "l",
          pch = NULL,
          lwd =1,
          theme = theme,
          spl = 8)
  dev.off()
}


# Temperature ####
units(celsius(1))$numerator
png("images/Temperature.png", 
    2000, 1500, "px",res = 300)
colplot(df = met,
        cols = "Temperature",
        xlab = "Months",
        ylab = units(celsius(1))$numerator,
        main = "Temperature",
        type = "l",
        pch = NULL,
        lwd =1,
        theme = theme,
        spl = 8)
dev.off()

# Notes ####
switch (language,
        "portuguese" = cat("\nFazendo anotações\n"),
        "english" = cat("\nTaking some notes\n"),
        "spanish" = cat("\nEscribiendo notas\n"))

vein_notes(notes = c("Default notes for vein::get_project",
                     "This project generates a emi/DF.csv to be read by data.table::fread",
                     "All the spatial emissions are stored in a postgis db",
                     "An also, fuel consumption FC in emi/FC.csv"), 
           file = "notes/README", 
           title = paste0("Metropolitan Area of São Paulo (MASP) ", year), 
           approach = 'Bottom-up', 
           traffic = "Samples of travel demand models for MASP", 
           composition = "CETESB", 
           ef = paste0("CETESB ", scale), 
           cold_start = "Not Applicable", 
           evaporative = "Running Losses, Diurnal and Hot Soak", 
           standards = "PROCONVE, PROMOT", 
           mileage = "Bruni and Bales 2013")
# saveRDS

switch (language,
        "portuguese" = message("\nArquivos em:"),
        "english" = message("\nFiles in:"),
        "spanish" = message("\nArchivos en:"))

message("config/metadata.rds\n",
        "config/mileage.rds\n",
        "config/tfs.rds\n",
        "config/fleet_age.rds\n",
        "config/fuel.rds\n")

switch (language,
        "portuguese" = message("\nFiguras em /images\n"),
        "english" = message("\nFigures in /image\n"),
        "spanish" = message("\nFiguras en /images\n"))

switch (language,
        "portuguese" = message("Limpando..."),
        "english" = message("Cleaning..."),
        "spanish" = message("Limpiando..."))


suppressWarnings(
  rm(i, choice, pa, metadata, po, tfs, veh, mileage, fuel, theme,
     n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC, df_x, ef, cores, vkm, ef, a, rota)
)
