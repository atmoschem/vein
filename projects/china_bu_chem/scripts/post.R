file.remove(list.files(path = "post/", recursive = T, full.names = T))

dt <- fread("emi/DF_EXHAUST.csv")

dt$g <- units::set_units(dt$g, "g")
dt$t <- units::set_units(dt$g, "t")

dt0 <- dt[, 
          round(sum(t) * factor_emi, 2), 
          by = .(pollutant)]
print(dt0)
# 
# dt0$model <- "VEIN 2019"
# dt00 <- dt0
# dt00$V1 <- units::set_units(4000*1000, "t")
# dt00$model <- "Ye Wu 2009"
# 
# 
# dt01 <- dt0
# dt01$V1 <- units::set_units(42.51*10000, "t")
# dt01$model <- "HTSVE 2013"
# 
# 
# dt02 <- dt0
# dt02$V1 <- units::set_units(469016.75, "t")
# dt02$model <- "MEIC 2013"
# 
# dt03 <- dt0
# dt03$V1 <- units::set_units(60*10000, "t")
# dt03$model <- "Wang (2008) - Shanghai"
# 
# dt04 <- dt0
# dt04$V1 <- units::set_units(300*1000, "t")
# dt04$model <- "Beijing S Zhang - 2021"


stop()

# streets  ####
switch(language,
        "portuguese" = message("\nAgregando emissões por rua...\n"),
        "english" = message("\nAgregating emissions by street...\n"),
        "spanish" = message("\nAgregando emisiones por calle...\n")
)

for (i in seq_along(pol)) {
        pols <- ifelse(pol[i] == "HC", "_HC", pol[i])
        x <- emis_merge(pol = pols, path = "emi", net = net, k = units::set_units(1, "1/h"))
        saveRDS(x, paste0(
                "post/streets/",
                ifelse(pols == "_HC", "HC", pols), ".rds"
        ))
}

switch(language,
       "portuguese" = message("\nIdentificando NMHC manualmente\n"),
       "english" = message("\nManually identifying NMHC\n"),
       "spanish" = message("\nIdentificando NMHC manualmenten")
)

#Special for future speciation
x <- emis_merge(pol = "_G", 
                what = "HC_STREETS.rds",
                path = "emi", 
                net = net, 
                k = units::set_units(1, "1/h")*0.95
                ) # assumming 95% of HC is NMHC
saveRDS(x, "post/streets/G_NMHC.rds")

x <- emis_merge(pol = "_D", 
                what = "HC_STREETS.rds",
                path = "emi", 
                net = net, 
                k = units::set_units(1, "1/h")*0.95
) # assumming 95% of HC is NMHC
saveRDS(x, "post/streets/D_NMHC.rds")

x <- emis_merge(pol = "_CNG", 
                what = "HC_STREETS.rds",
                path = "emi", 
                net = net, 
                k = units::set_units(1, "1/h")*0.95
) # assumming 95% of HC is NMHC
saveRDS(x, "post/streets/CNG_NMHC.rds")

x <- emis_merge(pol = "Evaporative", 
                what = "STREETS.rds",
                path = "emi", 
                net = net, 
                k = units::set_units(1, "1/h")
) # assumming 95% of HC is NMHC
saveRDS(x, "post/streets/EVAP_G_NMHC.rds")

switch(language,
       "portuguese" = message("\nIdentificando NO and NO2 manualmente\n"),
       "english" = message("\nManually identifying NO and NO2\n"),
       "spanish" = message("\nIdentificando NO and NO2 manualmenten")
)

x <- emis_merge(pol = "NOx", 
                what = "STREETS.rds",
                path = "emi", 
                net = net, 
                k = units::set_units(1, "1/h")*0.9
) # assumming 90% of NOx is NO
saveRDS(x, "post/streets/NO.rds")


x <- emis_merge(pol = "NOx", 
                what = "STREETS.rds",
                path = "emi", 
                net = net, 
                k = units::set_units(1, "1/h")*0.1
) # assumming 10% of NOx is NO2
saveRDS(x, "post/streets/NO2.rds")



# grids ####
switch(language,
        "portuguese" = message("\nAgregando emissões por grade\n"),
        "english" = message("\nAgregating emissions by grid...\n"),
        "spanish" = message("\nAgregando emisiones por grilla...\n")
)

lf <- list.files(path = "post/streets", pattern = ".rds", full.names = TRUE)
na <- list.files(path = "post/streets", pattern = ".rds", full.names = F)
na <- gsub(pattern = ".rds", replacement = "", x = na)

for (i in seq_along(lf)) {
        x <- readRDS(lf[i])
        gx <- emis_grid(spobj = x, g = g)
        saveRDS(gx, paste0("post/grids/", na[i], ".rds"))
}

# datatable ####
switch(language,
        "portuguese" = message("\nAgregando emissões em data.table\n"),
        "english" = message("\nAgregating emissions in data.table...\n"),
        "spanish" = message("\nAgregando emisiones en data.table...\n")
)

dt <- rbind(
  fread("emi/DF_EXHAUST.csv"),
  fread("emi/DF_EVAP.csv"),
  fread("emi/DF_WEAR.csv")
)
saveRDS(dt, "post/datatable/emissions.rds")

dt$g <- units::set_units(dt$g, "g")
dt$t <- units::set_units(dt$g, "t")

dt0 <- dt[, 
          round(sum(t) * factor_emi, 2), 
          by = .(pollutant)]
print(dt0)

dt0$model <- "VEIN 2019"
dt00 <- dt0
dt00$V1 <- units::set_units(4000*1000, "t")
dt00$model <- "Ye Wu 2009"


dt01 <- dt0
dt01$V1 <- units::set_units(42.51*10000, "t")
dt01$model <- "HTSVE 2013"


dt02 <- dt0
dt02$V1 <- units::set_units(469016.75, "t")
dt02$model <- "MEIC 2013"

dt03 <- dt0
dt03$V1 <- units::set_units(60*10000, "t")
dt03$model <- "Wang (2008) - Shanghai"

dt04 <- dt0
dt04$V1 <- units::set_units(300*1000, "t")
dt04$model <- "Beijing S Zhang - 2021"



# emissoes by veh
dt1 <- dt[, sum(t), by = .(pollutant, veh)]
df1 <- long_to_wide(df = dt1, column_with_new_names = "pollutant", column_with_data = "V1", column_fixed = "veh")
saveRDS(df1, "post/datatable/emissions_by_veh.rds")
data.table::fwrite(df1, "csv/emissions_by_veh.csv", row.names = FALSE)

# emissoes by fuel
dt2 <- dt[, sum(t), by = .(pollutant, fuel)]
df2 <- long_to_wide(df = dt2, column_with_new_names = "pollutant", column_with_data = "V1", column_fixed = "fuel")
saveRDS(df2, "post/datatable/emissions_by_fuel.rds")
data.table::fwrite(df2, "csv/emissions_by_fuel.csv", row.names = FALSE)

# emissoes by age
dt3 <- dt[, sum(t), by = .(pollutant, age)]
df3 <- long_to_wide(df = dt3, column_with_new_names = "pollutant", column_with_data = "V1", column_fixed = "age")
saveRDS(df3, "post/datatable/emissions_by_age.rds")
data.table::fwrite(df3, "csv/emissions_by_age.csv", row.names = FALSE)

switch(language,
        "portuguese" = message("\n\nArquivos em: /post/*:"),
        "english" = message("\nFiles in: /post/*"),
        "spanish" = message("\nArchivos en: /post/*")
)


switch(language,
        "portuguese" = message("Limpando..."),
        "english" = message("Cleaning..."),
        "spanish" = message("Limpiando...")
)

suppressWarnings(
        rm(
                "df1", "df2", "df3", "dt", "dt0", "dt1", "dt2", "dt3", "factor_emi",
                "g", "gx", "i", "lf", "na", "net", "pol", "pols", "x", "crs"
        )
)