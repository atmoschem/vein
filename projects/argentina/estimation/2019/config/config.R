
year_select        <- as.numeric(basename(getwd()))

#           <- basename(getwd())

switch (language,
        "portuguese" = cat( "Ano: ", year_select, "\n"),
        "english" =  cat( "Year: ", year_select, "\n"),
        "spanish" = cat( "Año: ", year_select, "\n"))

# apagando dados ####
a <- list.files(path = "config", pattern = ".rds", full.names = T)
file.remove(a)

# configuracao ####
setDT(metadata)
mileage <- as.data.frame(mileage)
mileage[, metadata$vehicles] <- add_lkm(mileage[, metadata$vehicles])
setDT(mileage)
setDT(tfs)
setDT(veh)

# vehicle ####
reg <- unique(fuel_month$region)
if(!any(grepl("region", names(veh)))) {
if(add_reg_veh) {
  switch (language,
          "portuguese" = cat( "Adicionando `region` em `veh`\n"),
          "english" =  cat( "Adding `region` column in  `veh`\n"),
          "spanish" = cat( "Agregando columna `region` en `veh`\n"))

  veh <- rbindlist(lapply(seq_along(reg), function(i){
    veh$region <- reg[i]
    veh
  }))

}}

fuel_month[is.na(UF), ]$UF <- "CF"
unique(fuel_month$region)
unique(fuel_month$UF)
# fuel ####
setDT(fuel_month)

# necesita columnas Year, Month, FUEL_M3 *density_tm3
fuel_month[, date := ISOdate(Year, Month, 1, 0,0,0)]

fuel_month[, consumption_t := FUEL_M3 *density_tm3]

# manual
fuel_month[, type := "data"]

pmonth <- fuel_month

fuel_month[, sum(consumption_t),
  by = .(region,
         Year,
         fuel,
         type,
         density_tm3)
] -> fuel

names(fuel)[ncol(fuel)] <- "consumption_t"

fuel$consumption_t <- units::set_units(fuel$consumption_t, "t")
fuel$density_tm3 <- units::set_units(fuel$density_tm3, "t/m3")

setDT(fuel_spec)
setDT(met)
setDT(euro)
setDT(tech)


setDT(im_ok)
setDT(im_co)
setDT(im_nox)
setDT(im_hc)
setDT(im_pm)



# regions con mayuscula

met$region <- toupper(met$region)
pmonth$region <- toupper(pmonth$region)
rain$region <- toupper(rain$region)

# pmonth ####
switch (language,
  "portuguese" = cat( "Filtrando fuel_month: ", year_select, "\n"),
  "english" =  cat( "Filtering fuel_month: ", year_select, "\n"),
  "spanish" = cat( "Filtrando fuel_month: ", year_select, "\n"))

pmonth <- pmonth[Year ==  year_select]


# met ####
head(met)
met$date <- ISOdate(met$Year, met$Month, 1, 0,0,0)
switch (language,
  "portuguese" = cat( "Filtrando met: ", year_select, "\n"),
  "english" =  cat( "Filtering met: ", year_select, "\n"),
  "spanish" = cat( "Filtrando met: ", year_select, "\n"))

met <- met[Year == year_select]

rain <- rain[year(date) == year_select]

# fuel ####
switch (language,
  "portuguese" = cat( "Filtrando fuel: ", year_select, "\n"),
  "english" =  cat( "Filtering fuel: ", year_select, "\n"),
  "spanish" = cat( "Filtrando fuel: ", year_select, "\n"))

fuel <- fuel[Year == year_select]
fuel$kinitial <- 1

# saving RDS ####
saveRDS(s, "config/s.rds")
saveRDS(metadata, "config/metadata.rds")
saveRDS(mileage, "config/mileage.rds")
saveRDS(tfs, "config/tfs.rds")
saveRDS(veh, "config/fleet_age.rds")
saveRDS(fuel, "config/fuel.rds")
saveRDS(fuel_spec, "config/fuel_spec.rds")
saveRDS(met, "config/met.rds")
saveRDS(rain, "config/rain.rds")
saveRDS(pmonth, "config/pmonth.rds")
saveRDS(euro, "config/euro.rds")
saveRDS(tech, "config/tech.rds")
saveRDS(fuel_spec, "config/fuel_spec.rds")

saveRDS(im_ok, "config/im_ok.rds")
saveRDS(im_co, "config/im_co.rds")
saveRDS(im_hc, "config/im_hc.rds")
saveRDS(im_nox, "config/im_nox.rds")
saveRDS(im_pm, "config/im_pm.rds")

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

dir.create(path = "post/streets", showWarnings = FALSE)
dir.create(path = "post/grids", showWarnings = FALSE)

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
            aes(x = month(date),
                y = as.numeric(FUEL_M3),
                fill = fuel)) +
  geom_bar(stat = "identity") +
  labs(y = "m3") +
  facet_wrap(~ region) +
  theme_bw(base_size = 16) +
  theme(panel.spacing = unit(0,'lines'))
p

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
                                     "Year"),
                                 with = F],
                      id.vars = c("Year"),
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

    ggplot(vv[family == fam[i] &
                veh> 0],
           aes(x = Year,
               y = veh,
               colour = vehicles)) +
      geom_point() +
      geom_line() +
      theme_bw()-> p


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


# sulphur/enxofre ####
switch(language,
       "portuguese" = cat("Plotando enxofre (ppm) \n"),
       "english" = cat("Plotting sulfur (ppm) \n"),
       "spanish" = cat("Plotando azufre (ppm) \n")
)

for (i in seq_along(n_veh)) {
  df_x <- as.data.frame(s)[, n_veh[[i]]]

  png(
    paste0(
      "images/S_",
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
    ylab = "S (ppm)",
    main = names(n_veh)[i],
    type = "l",
    pch = NULL,
    lwd = 1,
    theme = theme,
    spl = 8
  )
  dev.off()
}

# standard ####
switch(language,
       "portuguese" = cat("Plotando Padrao de emissao \n"),
       "english" = cat("Plotting emissions standard \n"),
       "spanish" = cat("Plotando norma de emision \n")
)

peuro <- euro[, metadata$vehicles, with = F]

for(i in 1:ncol(peuro)){
  peuro[[i]] <- gsub("Euro ", "", peuro[[i]])
  peuro[[i]] <- suppressWarnings(as.roman(peuro[[i]]))
  peuro[[i]] <- as.numeric(peuro[[i]])
}

peuro[is.na(peuro)] <- 0
peuro$Year<- euro$Year
melt.data.table(peuro, measure.vars = metadata$vehicles, id.vars = "Year") -> dfeuro

ggplot(dfeuro,
aes(x = Year,
 y = variable,
 fill = as.factor(value))) +
geom_tile() +
  theme_bw(base_size = 14) -> p

png(paste0("images/standard.png"),
width = 3000,
height = 2500,
"px",
res = 300)
print(p)
dev.off()


# Plot Temperature ####
units(celsius(1))$numerator


  ggplot(met,
         aes(x = date,
             y = Temperature)) +
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


# Plot Rain ####

  ggplot(rain,
         aes(x = date,
             y = PN)) +
    geom_line() +
    labs(title = year_select) +
    facet_wrap(~ region) +
    theme_bw(base_size = 16) -> p


  png("images/Rain.png",
      width = 2000,
      height = 1500,
      "px",
      res = 300)
  print(p)
  dev.off()


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
           title = paste0("Argentina by state ", year_select),
           approach = 'Top-Down',
           traffic = "DNRBA",
           composition = "DNRBA",
           ef = "COPERT",
           cold_start = "COPERT",
           evaporative = "Running Losses, Diurnal and Hot Soak",
           standards = "EURO",
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

