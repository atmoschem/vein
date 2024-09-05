
year   <- as.numeric(substr(x = getwd(),
                            start = nchar(getwd()) - 3,
                            stop = nchar(getwd()) ))

year_selected <- year

suppressWarnings(file.remove("emi/wear.csv"))

setDT(pmonth)

# Escapamento ####
switch(language,
       "portuguese" = cat("Estimando emissões Wear\n"),
       "english" = cat("Estimating emissions Wear\n"),
       "spanish" = cat("Estimando emisiones Wear\n")
)
metadata_original <- metadata

metadata$v_eea_old <- ifelse(metadata$v_eea_old %in% c("PC", "LCV", "Motorcycle"),
                             metadata$v_eea_old,
                             "HDV")

metadata$v_eea_old <- ifelse(metadata$v_eea_old %in% c("Motorcycle"),
                             "2W",
                             metadata$v_eea_old)
# monthly profile
metadata$fuel <- gsub("ELEC", "G", metadata$fuel)


wear <- c("tyre", "break", "road")

# wear ####

wear <- c("tyre", "break", "road")

reg <- unique(fuel$region)

for(k in seq_along(reg)) {

  cat("\n\n", reg[k],  "\n")

  for (i in seq_along(metadata$vehicles)) {

    cat(
      "\n", metadata$vehicles[i],
      rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
    )

    x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))

    x[is.na(x)] <- 0

    x <- x[region == reg[k], ]

    x$region <- NULL

    setDF(x)

    dm <- pmonth[region == reg[k] & #!
                   fuel == metadata$fuel[i]]$consumption_t

    for (j in seq_along(pol)) {

      cat(" ", pol[j], " ")

      for (m in seq_along(wear)) {

        ef <- ef_wear(wear= wear[m],
                      type = metadata$v_eea_old[i],
                      pol = pol[j],
                      speed = metadata$speed[i])

        ef <- rep(ef[[1]], ncol(x))

        array_x <- emis_hot_td(
          veh = x,
          lkm = mileage[[metadata$vehicles[i]]],
          ef = ef[1:maxage],
          fortran = TRUE,
          nt = check_nt()*0.9,
          pro_month = dm,
          verbose = verbose,
          params = list(
            veh = metadata$vehicles[i],
            size = metadata$size[i],
            fuel = metadata$fuel[i],
            pollutant = pol[j],
            type_emi = "Wear",
            subtype_emi = wear[m],
            baseyear = year
          )
        )
        array_x$region <- reg[k]

        fwrite(array_x, "emi/wear.csv", append = TRUE)
      }
    }
  }

}
switch(language,
       "portuguese" = message("\nEmissões em: /emi/wear.csv:"),
       "english" = message("\nEmissions in: /emi/wear.csv"),
       "spanish" = message("\nEmisiones en: /emi/wear.csv")
)
