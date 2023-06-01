# data.table ####
di <- list.files()

rbindlist(lapply(seq_along(di), function(i){
  print(di[i])
  x <- readRDS(paste0(di[i], "/post/emi_table.rds"))
  x$region <- gsub("regions/", "", di[i])
  x
})) -> df

df$emissions <- units::set_units(df$emissions, "g")

df$t <-      units::set_units(df$emissions, "t")

suppressWarnings(file.remove("rds/emi_table.rds"))

switch (language,
        "portuguese" = cat("Salvando emissões em\n"),
        "english" = cat("Saving emissions at\n"),
        "spanish" = cat("Guardando emisiones en\n"))
cat("post/emi_table.rds\n")
saveRDS(df, "rds/emi_table.rds")

dt0 <- df[, round(sum(t), 2), by = .(pollutant)]
print(dt0)

# totals by region ####
dt0 <- df[, round(sum(t), 2), by = .(pollutant, region)]
emisr <- dcast.data.table(data = dt0, 
                 formula = region~pollutant)
setorderv(emisr, "region")

st <- read_state()
names(st)
names(emisr)[1] <- "abbrev_state"

dfx <- merge(st, emisr, by = "abbrev_state")
dfx
saveRDS(dfx, "../rds/emi_region.rds")
rm(list = ls())
gc()
