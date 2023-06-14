
maxage       <- 40

year_select        <- as.numeric(substr(x = getwd(), 
                                        start = nchar(getwd()) - 6, 
                                        stop = nchar(getwd()) - 3))
# year_select <- 2000

#           <- basename(getwd())

switch (language,
        "portuguese" = cat( "Ano: ", year_select, "\n"),
        "english" =  cat( "Year: ", year_select, "\n"),
        "spanish" = cat( "Año: ", year_select, "\n"))

# apagando dados ####
a <- list.files(path = "config", pattern = ".rds", full.names = T)
file.remove(a)

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

# veh ####
if(maxage > 40) {
  switch (language,
          "portuguese" = stop(paste0("`maxage` tem que ser menor ou igual que 40 ")),
          "english" = stop(paste0("`maxage`  cannot be bigger than 40 ")),
          "spanish" = stop(paste0("`maxage` no puede ser mas que 40 ")))
  
}

# configuracao ####
setDT(metadata)
setDT(mileage)
setDT(tfs)
setDT(veh)
setDT(fuel)
setDT(met)

# setorder
setorderv(veh, 
          cols = c("region", "Year"),
          order = c(1, -1))

setorderv(fuel, 
          cols = c("region", "Year"),
          order = c(1, -1))

# mileage
v <- metadata$vehicles
for(i in seq_along(v)){
  mileage[[v[i]]] <- add_lkm(mileage[[v[i]]])
}

# veh 
veh <- veh[Year <= year_select]

veh[is.na(veh)] <- 0
# veh$PC_E <- ifelse(year_select %in% 1978:2006, veh$PC_E, 0)
# 
# veh$LCV_E <- ifelse(year_select %in% 1978:2006, veh$PC_E, 0)

veh$PC_FG <- ifelse(year_select <2003, 0, veh$PC_FG)

veh$PC_FE <- ifelse(year_select <2003, 0, veh$PC_FE)

veh$LCV_FG <- ifelse(year_select <2003, 0, veh$LCV_FG)

veh$LCV_FE <- ifelse(year_select <2003, 0, veh$LCV_FE)

veh$MC_150_FG <- ifelse(year_select <2009, 0, veh$MC_150_FG)

veh$MC_150_500_FG <- ifelse(year_select <2009, 0, veh$MC_150_500_FG)

veh$MC_500_FE <- ifelse(year_select <2009, 0, veh$MC_500_FG)

veh$MC_150_FE <- ifelse(year_select <2009, 0, veh$MC_150_FE)

veh$MC_150_500_FE <- ifelse(year_select <2009, 0, veh$MC_150_500_FE)

veh$MC_500_FE <- ifelse(year_select <2009, 0, veh$MC_500_FE)

# In this way, all vehicles are considered with a 40 year_selects lifetime.

# mileage ####
#take care of mileage in EF

# pmonth ####
setDT(pmonth)

pmonth <- pmonth[Year == ifelse(
  year_select < 2000, 2000, 
  ifelse(
    year_select > 2022, 2022,
    year_select))]

pmonth$Year <- year_select

# met ####
head(met)
met$date <- ISOdate(met$Year, met$Month, 1, 0,0,0)
met <- met[Year == year_select]

# fuel ####
fuel <- fuel[Year == year_select]
fuel$kinitial <- 1

# saving RDS ####
saveRDS(metadata, "config/metadata.rds")
saveRDS(mileage, "config/mileage.rds")
saveRDS(tfs, "config/tfs.rds")
saveRDS(as.data.frame(veh), "config/fleet_age.rds")
saveRDS(fuel, "config/fuel.rds")
saveRDS(met, "config/met.rds")
saveRDS(pmonth, "config/pmonth.rds")

switch (language,
        "portuguese" = message("Arquivos em: ", getwd(), "/config/*\n"),
        "english" = message("Files in: ", getwd(), "/config/*\n"),
        "spanish" = message("Archivos en: ", getwd(), "/config/*\n"))


# pastas
if(delete_directories){
  choice <- 1
  
  if(language == "portuguese") {
    # choice <- utils::menu(c("Sim", "Não"), title="Apagar pastas csv, emi, images, notes, post e veh??")
    if(choice == 1){
      message("Apagando pastas `emi`, `images`, `notes`, `post` e `veh`")
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

switch (language,
        "portuguese" = message("Novas pastas:"),
        "english" = message("New directories:"),
        "spanish" = message("Nuevas carpetas"))

message("images\nveh\nemi\nnotes\npost\n")

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
# Plot Fuel ####
switch (language,
        "portuguese" = cat("Plotando combustivel \n"),
        "english" = cat("Plot fuel \n"),
        "spanish" = cat("Plotando combustible \n"))

p <- ggplot(pmonth,
            aes(x = m,
                y = as.numeric(m3),
                fill = fuel)) +
  geom_bar(stat = "identity") +
  labs(y = "m3") +
  facet_wrap(~ region, 
             scales = "free_y") +
  theme_bw(base_size = 16) +
  theme(legend.position = c(0.8, 0.05),
        legend.direction="horizontal")

png("images/FUEL.png", 
    width = 3000, 
    height = 2000, 
    units = "px",
    res = 300)
print(p)
dev.off()

# Plot Fleet ####
switch (language,
        "portuguese" = cat("Plotando frota \n"),
        "english" = cat("Plot fleet \n"),
        "spanish" = cat("Plotando flota \n"))
setDT(veh)
setDT(metadata)
vv <- melt.data.table(data = veh[, c(metadata$vehicles, 
                                     "region", 
                                     "Year"), 
                                 with = F], 
                      id.vars = c("Year", "region"), 
                      variable.name = "vehicles",
                      value.name = "veh")
vv <- merge(vv, 
            metadata[, 
                     c("vehicles", 
                       "family",
                       "fuel",
                       "size"),
                     with = F],
            by = "vehicles",
            all.x = T)

vv$sf <- paste(vv$size, vv$fuel)
fam <- unique(metadata$family)

for(i in seq_along(fam)) {
  
  if(any(fam[i] %in% c("BUS", "TRUCKS"))) {
    ggplot(vv[family == fam[i] &
                veh> 0], 
           aes(x = Year,
               y = veh,
               colour = vehicles)) +
      geom_point() +
      facet_wrap(~ region, 
                 scales = "free_y") -> p
    
  } else if (any(fam[i] %in% c("PC", "LCV"))){
    ggplot(vv[family == fam[i] &
                veh> 0], 
           aes(x = Year,
               y = veh,
               colour = fuel)) +
      geom_point() +
      facet_wrap(~ region, 
                 scales = "free_y") -> p
    
  } else {
    ggplot(vv[family == fam[i] &
                veh> 0], 
           aes(x = Year,
               y = veh,
               colour = sf)) +
      geom_point() +
      facet_wrap(~ region, 
                 scales = "free_y") -> p
    
  }
  
  png(paste0("images/FLEET_", fam[i], ".png"), 
      width = 3000, 
      height = 2500, 
      "px",
      res = 300)
  print(p)  
  dev.off()  
}


#Plot TFS ####

switch (language,
        "portuguese" = cat("Plotando perfis `tfs`\n"),
        "english" = cat("Plot profiles `tfs`\n"),
        "spanish" = cat("Plotando perfiles `tfs`\n"))

for(i in seq_along(n_veh)) {
  df_x <- tfs[, n_veh[[i]], with = F]
  png(
    paste0("images/TFS_", 
           names(n_veh)[i],
           ".png"), 
    2000, 1500, "px",res = 300)
  colplot(df = df_x,
          cols = n_veh[[i]],
          xlab = "Hour",
          ylab = "",
          main = paste0("TFS ", names(n_veh)[i], " ", year_select),
          type = "l",
          pch = NULL,
          lwd =1,
          theme = theme)
  dev.off()
}


# Plot Mileage ####

switch (language,
        "portuguese" = cat("Plotando quilometragem \n"),
        "english" = cat("Plot mileage `tfs`\n"),
        "spanish" = cat("Plotando kilometraje `tfs`\n"))

for(i in seq_along(n_veh)) {
  df_x <- mileage[, n_veh[[i]], with = F]
  png(
    paste0("images/MILEAGE_",
           names(n_veh)[i],
           ".png"),
    2000, 1500, "px",res = 300)
  colplot(df = df_x,
          cols = n_veh[[i]],
          xlab = "Age of use",
          ylab = "[km/year]",
          main = paste0("Mileage ", names(n_veh)[i], " ", " ", year_select),
          type = "l",
          pch = NULL,
          lwd =1,
          theme = theme)
  dev.off()
}


# Plot Temperature ####
units(celsius(1))$numerator

for(i in 1){
  
  ggplot(met, 
         aes(x = date,
             y = Temperature,
             colour = scenario)) +
    geom_line() +
    labs(title = year_select) + 
    facet_wrap(~ region) + 
    theme_bw(base_size = 16) -> p
  
  png("images/Temperature.png", 
      width = 2000, 
      height = 1500, 
      "px",
      res = 300)
  print(p)  
  dev.off()
}

# # Plot month #####
# pmonth <- as.data.frame(pmonth)
# for(i in seq_along(n_veh)) {
#   df_x <- pmonth[, n_veh[[i]]]
#   png(
#     paste0("images/PMONTH_",
#            names(n_veh)[i],
#            ".png"),
#     2000, 1500, "px",res = 300)
#   colplot(df = df_x,
#           cols = n_veh[[i]],
#           xlab = "Month",
#           ylab = "%",
#           main = paste0(names(n_veh)[i], " ", , " ", year_select),
#           type = "l",
#           pch = NULL,
#           lwd =1,
#           theme = theme)
#   dev.off()
# }
# 


# Notes ####
switch (language,
        "portuguese" = cat("\nFazendo anotações\n"),
        "english" = cat("\nTaking some notes\n"),
        "spanish" = cat("\nEscribiendo notas\n"))

vein_notes(notes = c("Default notes for vein::get_project"), 
           file = "notes/README", 
           title = paste0("Brazil by state ", year_select), 
           approach = 'Top-Down', 
           traffic = "DENATRAN", 
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
        "portuguese" = message("\nFiguras em \n"),
        "english" = message("\nFigures in \n"),
        "spanish" = message("\nFiguras en \n"))
message("/images")

switch (language,
        "portuguese" = message("Limpando..."),
        "english" = message("Cleaning..."),
        "spanish" = message("Limpiando..."))

