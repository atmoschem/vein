file.remove("emi/FC.csv")
# Exhaust ####
for (i in seq_along(metadata$vehicles)) {
  cat(
    "\n", metadata$vehicles[i],
    rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
  )

  x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))

  for (j in seq_along(pol)) {
    cat(pol[j], " ")

    ef <- ef_cetesb(
      p = pol[j],
      veh = metadata$vehicles[i],
      year = year,
      agemax = ncol(x),
      verbose = verbose
    )

    array_x <- emis_hot_td(
      veh = x,
      lkm = mileage[[metadata$vehicles[i]]],
      ef = ef,
      fortran = TRUE,
      nt = check_nt() / 2,
      pro_month = pmonth[[metadata$vehicles[i]]],
      verbose = verbose,
      params = list(
        veh = metadata$vehicles[i],
        size = metadata$size[i],
        fuel = metadata$fuel[i],
        pollutant = pol[j],
        type_emi = "Exhaust",
        subtype_emi = "Exhaust",
        baseyear = year
      )
    )

    fwrite(array_x, "emi/FC.csv", append = TRUE)
  }
}

switch(language,
  "portuguese" = message("\nArquivos em:"),
  "english" = message("\nFiles in:"),
  "spanish" = message("\nArchivos en:")
)

cat(paste0(getwd(), "/emi/*\n"))

# data.table ####
dt <- fread("emi/FC.csv")

dt$pollutant <- as.character(dt$pollutant)
dt$g <- units::set_units(dt$emissions, "g")
dt$t <- units::set_units(dt$g, t)

dt0 <- dt[pollutant == "FC",
  round(sum(t), 2),
  by = .(fuel)
]
data.table::setkey(dt0, "fuel")

names(dt0)[2] <- "estimation_t"
dtf <- merge(dt0, fuel, by = "fuel")
dtf$density_tm3 <- units::set_units(dtf$density_tm3, "t/m^3")
dtf$consumption_lt <- units::set_units(dtf$consumption_lt, "l")
dtf$consumption_m3 <- units::set_units(dtf$consumption_lt, "m^3")
dtf$consumption_t <- dtf$consumption_m3 * dtf$density_tm3
dtf$estimation_consumption <- dtf$estimation_t / dtf$consumption_t
print(dtf[, c("fuel", "estimation_t", "consumption_t", "estimation_consumption")])

switch(language,
  "portuguese" = message("Limpando..."),
  "english" = message("Cleaning..."),
  "spanish" = message("Limpiando...")
)
suppressWarnings(rm(i, j, pol, dt, dt0, dtf, factor_emi, fuel))

ls()
invisible(gc())