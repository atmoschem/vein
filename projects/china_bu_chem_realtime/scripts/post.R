file.remove(list.files(path = "post/", recursive = T, full.names = T))

# DF ####
dt <- rbind(fread("emi/DF_EXHAUST.csv"),
            fread("emi/DF_EVAP.csv"),
            fread("emi/DF_WEAR.csv"))

dt$g <- units::set_units(dt$g, "g")
dt$t <- units::set_units(dt$g, "t")

dt0 <- dt[, 
          round(sum(g), 2), 
          by = .(pollutant)]
print(dt0)
fwrite(dt, "post/datatable/DF.csv")


# STREETS #
dt <- rbind(fread("emi/DF_EXHAUST_STREETS.csv"),
            fread("emi/DF_EVAP_STREETS.csv"),
            fread("emi/DF_WEAR_STREETS.csv"))
dt
dt$g <- units::set_units(dt$g, "g")
dt$t <- units::set_units(dt$g, "t")

dt0 <- dt[, 
          round(sum(g), 2), 
          by = .(id, pollutant)]


fwrite(dt, "post/datatable/STREETS.csv")

switch(language,
        "portuguese" = message("\n\nArquivos em: /post/*:"),
        "english" = message("\nFiles in: /post/*"),
        "spanish" = message("\nArchivos en: /post/*")
)

