

UF_select          <- basename(getwd())

g <- readRDS(paste0("../../../rds/g_", UF_select, ".rds"))

# df
df <- rbind(
  fread("emi/exhaust.csv"),
  fread("emi/wear.csv"),
  fread("emi/evaporatives.csv")
)
df$pollutant <- ifelse(df$pollutant == "PM2.5", "PM", df$pollutant)
df$emissions <- units::set_units(df$emissions, "g")
df$t <- units::set_units(df$emissions, "t")

# datatable ####
suppressWarnings(file.remove("post/emi_table.rds"))
switch(language,
       "portuguese" = cat("Salvando emissões\n"),
       "english" = cat("Saving emissions\n"),
       "spanish" = cat("Guardando emisiones\n")
)

saveRDS(df, "post/emi_table.rds")

fwrite(df, "post/emi_table.csv")

dt0 <- df[, round(sum(t), 2), by = .(pollutant)]
print(dt0)


switch (language,
        "portuguese" = message("\n\nArquivos em:"),
        "english" = message("\n\nFiles in:"),
        "spanish" = message("\n\nArchivos en:"))

message("post/emi_table.rds\n",
        "post/emi_table.csv\n")

