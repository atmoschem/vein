year_selected <- as.numeric(substr(
  x = getwd(),
  start = nchar(getwd()) - 3,
  stop = nchar(getwd())
))

suppressWarnings(file.remove("emi/evaporatives.csv"))

# filtrando veiculos otto
meta_ev <- metadata[metadata$fuel != "D", ]
veh_ev <- meta_ev$vehicles

# temperature: one estimate per scenario. Scenarios are labelled in type_emi;
# set `evap_scenario` in main.R to keep only one (several scenarios in the same
# table must not be added together)
if (exists("evap_scenario") && !is.null(evap_scenario)) {
  meto <- meto[scenario %in% evap_scenario]
  if (nrow(meto) == 0) {
    stop("`evap_scenario` (", paste(evap_scenario, collapse = ", "),
         ") not found in met for year ", year_selected)
  }
}
t_sc <- split(meto, meto$scenario)
sc <- names(t_sc)

# regions in the row order of veh/*.rds (written by scripts/traffic.R)
reg <- readRDS(paste0("veh/", veh_ev[1], ".rds"))$region

dfreg <- data.table(region = reg, rows = as.character(seq_along(reg)))

switch(
  language,
  "portuguese" = cat("\nEmissões evaporativas\n"),
  "english" = cat("\nEvaporative emissions\n"),
  "spanish" = cat("\nEmisiones evaporativas\n")
)


# tt <- rr <- mm <- i <- eva <- 1
for (tt in seq_along(t_sc)) {
  # monthly temperature per region (repeated rows averaged). Rows are
  # month-major with the regions in the row order of veh/*.rds: this is the
  # layout emis_hot_td expects when nrow(ef) == 12 * nrow(veh)
  met <- t_sc[[tt]][
    region %in% reg,
    .(Temperature = mean(Temperature)),
    by = .(region, Month)
  ]
  met <- met[order(Month, match(region, reg))]
  if (nrow(met) != 12 * length(reg)) {
    stop(
      "met for scenario ", sc[tt], " must have 12 months for each of the ",
      length(reg), " regions"
    )
  }

  te <- met$Temperature
  bb <- (0 + 15) / 2
  cc <- (10 + 25) / 2
  dd <- (20 + 35) / 2

  met$tem <- ifelse(
    te <= bb,
    "0_15",
    ifelse(
      te > bb & te <= cc,
      "10_25",
      "20_35"
    )
  )

  # checks name
  type_emis <- c("Diurnal", "Running Losses", "Hot Soak")
  name_file_evap <- c("/DIURNAL_", "/RUNNING_LOSSES_", "/HOT_SOAK_")

  met$ef_d <- paste0("D_", met$tem)
  met$ef_rl <- paste0("R_", met$tem)
  met$ef_hs <- paste0("S_", met$tem)

  dd <- list(met$ef_d, met$ef_rl, met$ef_hs)

  names(dd) <- c("Diurnal", "Running Losses", "Hot Soak")

  for (eva in seq_along(dd)) {
    cat("\n", names(dd)[eva], "\n")

    for (i in seq_along(veh_ev)) {
      x <- readRDS(paste0("veh/", veh_ev[i], ".rds"))

      x[is.na(x)] <- 0

      if (!identical(x$region, reg)) {
        stop("veh/", veh_ev[i], ".rds rows are not in the region order of veh/", veh_ev[1], ".rds")
      }

      # national monthly profile, Jan..Dec
      dm <- as.numeric(pmonth[fuel == meta_ev$fuel[i], mean(as.numeric(m3)), keyby = m]$V1)

      rbindlist(lapply(seq_along(dd[[eva]]), function(efef) {
        xx <- ef_cetesb(
          p = dd[[eva]][efef],
          veh = veh_ev[i],
          year = year_selected,
          scale = scale,
          agemax = maxage,
          verbose = verbose
        )

        if (names(dd)[eva] == "Diurnal") {
          # diurnal: g/day* day/km= g/km

          xx <- xx / (mileage[[veh_ev[i]]] / 365) # mean daily mileage
        } else {
          xx <- xx * meta_ev$trips_day[i] / (mileage[[veh_ev[i]]] / 365) # quilometragem medio diario
        }
        #print(head(xx))
        as.data.frame(matrix(xx, nrow = 1))
      })) -> lef

      # print("EF")
      #print(head(lef))
      #	lef <- as.data.frame(lef)

      lef <- EmissionFactors(as.data.frame(lef))
      #print(head(lef))

      x <- as.data.frame(x)
      array_x <- emis_hot_td(
        veh = x[, 1:maxage],
        lkm = mileage[[veh_ev[i]]],
        ef = lef,
        pro_month = dm,
        verbose = verbose,
        params = list(
          veh = veh_ev[i],
          size = meta_ev$size[i],
          fuel = meta_ev$fuel[i],
          pollutant = "NMHC",
          type_emi = paste0("Evaporatives ", sc[tt]),
          subtype_emi = names(dd)[eva],
          baseyear = year_selected
        )
      )

      array_x <- merge(array_x, dfreg, by = "rows", all.x = T)

      fwrite(array_x, "emi/evaporatives.csv", append = TRUE)
    } # vehicle
  } # eva (3 diurnal, runnin hot
} # scenario temperature

cat("\n")


switch(
  language,
  "portuguese" = message("\n\nArquivos em: emi/evaporatives.csv:"),
  "english" = message("\n\nFiles in: emi/evaporatives.csv"),
  "spanish" = message("\n\nArchivos en: emi/evaporatives.csv")
)
