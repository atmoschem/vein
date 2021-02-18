# grade
g <- st_transform(g, crs)

# streets  and grids####
switch(language,
        "portuguese" = message("\nLendo emi/STREETS.csv.gz...\n"),
        "english" = message("\nReading emi/STREETS.csv.gz...\n"),
        "spanish" = message("\nLeyendo emi/STREETS.csv.gz...\n")
)

st <- rbind(
        fread("emi/STREETS_EXHAUST.csv.gz"),
        fread("emi/STREETS_EVAPORATIVE.csv.gz"),
        fread("emi/STREETS_PAVED.csv.gz")
)
na <- paste0("V", 1:nrow(tfs))
geo <- st_geometry(net)


st[
        ,
        pols := paste0(pollutant, "_", gsub(pattern = " ", replacement = "", x = type_emi))
]


pols <- unique(st$pols)

switch(language,
        "portuguese" = message("\nAgregando emissões por grade\n"),
        "english" = message("\nAgregating emissions by grid...\n"),
        "spanish" = message("\nAgregando emisiones por grilla...\n")
)

for (i in seq_along(pols)) {
        print(pols[i])
        dfm <- st[pols == pols[i],
                lapply(.SD, sum, na.rm = TRUE),
                by = "id",
                .SDcols = na
        ]
        setDF(dfm)
        x <- st_sf(Emissions(dfm, time = "1/h"), geometry = geo)
        saveRDS(x, paste0("post/streets/", pols[i], ".rds"))

        gx <- emis_grid(spobj = x, g = g)
        saveRDS(gx, paste0("post/grids/", pols[i], ".rds"))
}


# Agregando emissões por categoria ####
switch(language,
        "portuguese" = message("\nAgregando emissões por categoria\n"),
        "english" = message("\nAggregating emissions by category...\n"),
        "spanish" = message("\nAgregando emisiones por categoria...\n")
)

dt <- rbind(
        fread("emi/DF_EXHAUST.csv.gz"),
        fread("emi/DF_EVAPORATIVE.csv.gz"),
        fread("emi/DF_PAVED.csv.gz")
)
dt$pollutant <- as.character(dt$pollutant)
dt$t <- units::set_units(units::set_units(dt$g, g), t)
saveRDS(dt, "post/datatable/emissions.rds")
unique(dt$type_emi)

dt0 <- dt[, round(sum(t) * factor_emi, 2), by = .(pollutant, type_emi)]
print(dt0)


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