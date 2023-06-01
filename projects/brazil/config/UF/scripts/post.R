

UF_select          <- basename(getwd())

g <- readRDS(paste0("../../../rds/g_", UF_select, ".rds"))

# df
df <- rbind(
  fread("emi/exhaust.csv"),
  fread("emi/ressus.csv"),
  fread("emi/evaporatives.csv")
)

df$emissions <- units::set_units(df$emissions, "g")
df$t <- units::set_units(df$emissions, "t")

# streets  ####
suppressWarnings(file.remove("post/emi_street.rds"))
switch(language,
       "portuguese" = cat("Distribuindo as emissões nas ruas\n"),
       "english" = cat("Distributing emissions on streets\n"),
       "spanish" = cat("Distribuyendo las emisiones en las calles\n")
)

dt <- df[pollutant != "NMHC",
         sum(emissions, na.rm = T),
         by = .(month, pollutant)
]

dt$fuel <- "all"
dt$type_emi <- "Exhaust"

dtev <- df[pollutant == "NMHC",
           sum(emissions, na.rm = T),
           by = .(month, pollutant, fuel, type_emi)
]
dtev
DT <- data.table(
  month = c(dt$month, dtev$month),
  pollutant = c(dt$pollutant, dtev$pollutant),
  fuel = c(dt$fuel, dtev$fuel),
  type_emi = c(dt$type_emi, dtev$type_emi),
  g = c(dt$V1, dtev$V1)
)
DT
# DT <- DT[month %in% months_subset]
DT$month <- as.character(ifelse(nchar(DT$month) < 2,
                                paste0("0", DT$month),
                                DT$month
))
DT$TY <- ifelse(DT$type_emi == "Exhaust", "EXH", "EVA")
DT$PFTM <- paste(DT$pollutant, DT$fuel, DT$TY, DT$month, sep = "_")

roads$length <- st_length(roads)
roads$lengthHDV <- ifelse(roads[[osm_name]] %in% c(
  "tertiary",
  "secondary",
  "primary"
), 0,
roads$length
)

for (i in seq_along(DT$PFTM)) {
  roads[[DT$PFTM[i]]] <- DT[PFTM == DT$PFTM[i]]$g * roads$length / sum(roads$length)
}

# By default distributing NO,NO2, NOx only one trunks and motorway
names_nox <- c(
  grep("NO_", DT$PFTM, value = T),
  grep("NO2_", DT$PFTM, value = T),
  grep("NOx_", DT$PFTM, value = T)
)

for (i in seq_along(names_nox)) {
  roads[[names_nox[i]]] <- DT[PFTM == names_nox[i]]$g * roads$lengthHDV / sum(roads$lengthHDV)
}

saveRDS(roads, "post/emi_street.rds")


# grids ####
suppressWarnings(file.remove("post/emi_grid.rds"))

x <- roads[, DT$PFTM]

switch(language,
       "portuguese" = cat("Cortando as ruas para grade\n"),
       "english" = cat("Cropping streets for grid extent\n"),
       "spanish" = cat("Cortando las calles para grilla\n")
)
#x <- st_crop(x, st_as_sfc(st_bbox(g)))

switch(language,
       "portuguese" = cat("Distribuindo as emissões na grade\n"),
       "english" = cat("Gridding emissions\n"),
       "spanish" = cat("Distribuyendo las emisiones en las grilla\n")
)

gx <- emis_grid(spobj = x, g = g)
gx$id <- g$id   #id from parent domain
saveRDS(gx, paste0("post/emi_grid.rds"))

# datatable ####
suppressWarnings(file.remove("post/emi_table.rds"))
switch(language,
       "portuguese" = cat("Salvando emissões\n"),
       "english" = cat("Saving emissions\n"),
       "spanish" = cat("Guardando emisiones\n")
)

saveRDS(df, "post/emi_table.rds")

dt0 <- df[, round(sum(t), 2), by = .(pollutant)]
print(dt0)


switch (language,
        "portuguese" = message("\n\nArquivos em:"),
        "english" = message("\n\nFiles in:"),
        "spanish" = message("\n\nArchivos en:"))

message("post/emi_street.rds\n",
        "post/emi_grid.rds\n",
        "post/emi_table.rds\n")

