# grade
g <- st_transform(g, crs)

# df ####
dir.create("post/DF")
dt <- rbind(
  fread("emi/exhaust.csv"),
  fread("emi/cold.csv"),
  fread("emi/evaporatives.csv"),
  fread("emi/paved_roads.csv"),
  fread("emi/wear.csv")
)


dt$g <- units::set_units(dt$g, "g")
dt$t <- units::set_units(dt$g, "t")
fwrite(dt, "post/DF/emissions.csv.gz")
saveRDS(dt, "post/DF/emissions.rds")

# Agregando emissões por categoria ####
switch(
  language,
  "portuguese" = message("\nAgregando emissões por categoria\n"),
  "english" = message("\nAggregating emissions by category...\n"),
  "spanish" = message("\nAgregando emisiones por categoria...\n")
)


dt0 <- dt[, round(sum(t) * factor_emi, 2), by = .(pollutant)]
print(dt0)


# emissoes by veh
dt1 <- dt[, sum(t), by = .(pollutant, veh)]
df1 <- dcast.data.table(data = dt1, formula = pollutant ~ veh)
saveRDS(df1, "post/DF/emissions_by_veh.rds")
dir.create("csv")
data.table::fwrite(df1, "csv/emissions_by_veh.csv", row.names = FALSE)

# emissoes by fuel
dt2 <- dt[, sum(t), by = .(pollutant, fuel)]
df2 <- dcast.data.table(data = dt2, formula = pollutant ~ fuel)
saveRDS(df2, "post/DF/emissions_by_fuel.rds")
data.table::fwrite(df2, "csv/emissions_by_fuel.csv", row.names = FALSE)

# emissoes by age
dt3 <- dt[, sum(t), by = .(pollutant, age)]
df3 <- dcast.data.table(data = dt3, formula = pollutant ~ age)
saveRDS(df3, "post/DF/emissions_by_age.rds")
data.table::fwrite(df3, "csv/emissions_by_age.csv", row.names = FALSE)


# streets  ####
dir.create("post/streets")
switch(
  language,
  "portuguese" = message("\nAgregando emissões por rua...\n"),
  "english" = message("\nAgregating emissions by street...\n"),
  "spanish" = message("\nAgregando emisiones por calle...\n")
)

dtxx <- function(x, tfs, rmf = FALSE) {
  dt <- rbindlist(lapply(seq_along(x), function(i) {
    df <- fread(x[i])
    # print(x[i])
    # print(dim(df))
    if (rmf) {
      df$fuel <- NULL
    }
    df$id <- 1:nrow(df)
    df
  }))

  dt[,
    lapply(.SD, sum, na.rm = T),
    .SDcols = paste0("V", 1:nrow(tfs)),
    by = id
  ] -> xx
  Emissions(xx, time = "h") -> xx
  xx$id <- as.numeric(xx$id)
  xx
}

# CO
x <- list.files(
  path = "emi/",
  pattern = "CO.csv",
  recursive = T,
  full.names = T
)

dtxx(x, tfs) -> x_st

x_st <- st_sf(x_st, geometry = net$geom)

saveRDS(x_st, "post/streets/CO.rds")


# NOx
x <- list.files(
  path = "emi/",
  pattern = "NO.csv",
  recursive = T,
  full.names = T
)

dtxx(x, tfs) -> x_st

x_st <- st_sf(x_st, geometry = net$geom)

saveRDS(x_st, "post/streets/NOx.rds")

# NO
x <- list.files(
  path = "emi/",
  pattern = "NO.csv",
  recursive = T,
  full.names = T
)

dtxx(x, tfs) -> x_st

x_st <- st_sf(x_st, geometry = net$geom)

saveRDS(x_st, "post/streets/NO.rds")

# NO2
x <- list.files(
  path = "emi/",
  pattern = "NO2.csv",
  recursive = T,
  full.names = T
)

dtxx(x, tfs) -> x_st

x_st <- st_sf(x_st, geometry = net$geom)

saveRDS(x_st, "post/streets/NO2.rds")

# PM2.5
x <- list.files(
  path = "emi/",
  pattern = "PM.csv",
  recursive = T,
  full.names = T
)

dtxx(x, tfs, rmf = T) -> x_st

x_st <- st_sf(x_st, geometry = net$geom)

saveRDS(x_st, "post/streets/PM2.5.rds")

# PM10
x <- list.files(
  path = "emi/",
  pattern = "PM10.csv",
  recursive = T,
  full.names = T
)

dtxx(x, tfs, rmf = T) -> x_st

x_st <- st_sf(x_st, geometry = net$geom)

saveRDS(x_st, "post/streets/PM10.rds")


# NMHC
x <- list.files(path = "emi/", pattern = "NMHC", recursive = T, full.names = T)
dt <- rbindlist(lapply(seq_along(x), function(i) {
  df <- fread(x[i])
  df$id <- 1:nrow(df)
  df
}))

dt[,
  lapply(.SD, sum, na.rm = T),
  .SDcols = paste0("V", 1:nrow(tfs)),
  by = .(id, fuel)
] -> xx

for (i in 1:nrow(tfs)) {
  xx[[paste0("V", 1:nrow(tfs))[i]]] <- Emissions(
    xx[[paste0("V", 1:nrow(tfs))[i]]],
    time = "h"
  )
}

unique(xx$fuel)
ln <- split(xx, xx$fuel)
names(ln)
for (i in seq_along(ln)) {
  ln[[i]]$fuel <- NULL
  x_st <- ln[[i]]
  x_st <- st_sf(x_st, geometry = net$geom)
  saveRDS(x_st, paste0("post/streets/NMHC_EXHAUST_", names(ln)[i], ".rds"))
}

# EVAP
x <- list.files(path = "emi/", pattern = "EVAP", recursive = T, full.names = T)
dt <- rbindlist(lapply(seq_along(x), function(i) {
  df <- fread(x[i])
  df$id <- 1:nrow(df)
  df
}))

names(dt)

dt[,
  lapply(.SD, sum, na.rm = T),
  .SDcols = paste0("V", 1:nrow(tfs)),
  by = .(id, fuel)
] -> xx

for (i in 1:nrow(tfs)) {
  xx[[paste0("V", 1:nrow(tfs))[i]]] <- Emissions(
    xx[[paste0("V", 1:nrow(tfs))[i]]],
    time = "h"
  )
}

xx <- xx[!fuel %in% c("ELEC", "GLP", "HY")] # 0
unique(xx$fuel)
ln <- split(xx, xx$fuel)
names(ln)
for (i in seq_along(ln)) {
  ln[[i]]$fuel <- NULL
  x_st <- ln[[i]]
  x_st <- st_sf(x_st, geometry = net$geom)
  saveRDS(x_st, paste0("post/streets/NMHC_EVAP_", names(ln)[i], ".rds"))
}


# grids ####
dir.create("post/grids")
switch(
  language,
  "portuguese" = message("\nAgregando emissões por grade\n"),
  "english" = message("\nAgregating emissions by grid...\n"),
  "spanish" = message("\nAgregando emisiones por grilla...\n")
)

lf <- list.files(path = "post/streets", pattern = ".rds", full.names = TRUE)
na <- list.files(path = "post/streets", pattern = ".rds", full.names = F)
na <- gsub(pattern = ".rds", replacement = "", x = na)

for (i in seq_along(lf)) {
  x <- readRDS(lf[i])
  gx <- emis_grid(spobj = x, g = g, sr = crs)
  saveRDS(gx, paste0("post/grids/", na[i], ".rds"))
}

switch(
  language,
  "portuguese" = message("\n\nArquivos em: /post/*:"),
  "english" = message("\nFiles in: /post/*"),
  "spanish" = message("\nArchivos en: /post/*")
)


switch(
  language,
  "portuguese" = message("Limpando..."),
  "english" = message("Cleaning..."),
  "spanish" = message("Limpiando...")
)

suppressWarnings(
  rm(
    "df1",
    "df2",
    "df3",
    "dt",
    "dt0",
    "dt1",
    "dt2",
    "dt3",
    "factor_emi",
    "g",
    "gx",
    "i",
    "lf",
    "na",
    "net",
    "pol",
    "pols",
    "x",
    "crs"
  )
)
