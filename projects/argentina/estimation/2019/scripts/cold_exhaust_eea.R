

year   <- as.numeric(substr(x = getwd(),
                            start = nchar(getwd()) - 3,
                            stop = nchar(getwd()) ))

year_selected <- year

suppressWarnings(file.remove("emi/cold_exhaust.csv"))

setDT(pmonth)

# Escapamento ####
switch(language,
       "portuguese" = cat("Estimando emissões Cold Start\n"),
       "english" = cat("Estimating emissions Cold Start\n"),
       "spanish" = cat("Estimando emisiones Cold Start\n")
)

setDT(pmonth)

# fuel correction ####
euros <- c("PRE", "I", "II", "III", "IV", "V", "VI", "VIc")

fc <- as.list(fuel_spec$value)

names(fc) <- paste0(fuel_spec$fuel, "_", fuel_spec$parameter)

fuel_corr(
  euro = euros,
  g = c(e100 = fc$G_e100,
        aro = fc$G_aro,
        o2 = fc$G_o2,
        e150 = fc$G_e150,
        olefin = fc$G_olefin,
        s = fc$G_s),
  d = c(den = fc$D_den,
        pah = fc$D_pah,
        cn = fc$D_cn,
        t95 = fc$D_t95,
        s = fc$D_s)

) -> fcorr

names(fcorr)[3:4] <- c("veh", "pol")
cov1 <- fcorr[pol == "COV"]
cov1$pol <- "HC"
cov2 <- fcorr[pol == "COV"]
cov2$pol <- "NMHC"
no1 <- fcorr[pol == "NOx"]
no1$pol <- "NO"
no2 <- fcorr[pol == "NOx"]
no2$pol <- "NO2"

fcorr <- rbind(fcorr, cov1, cov2, no1, no2)

metadata$v_eea_old_fuel <- ifelse(metadata$v_eea_old %in% c("PC", "LCV", "Motorcycle"),
                                  paste0("LDV", metadata$fuel_eea_old),
                                  "HDV")

# Cold Start ####

metadata_cold <- metadata[metadata$fuel_eea_old %in% "G" &
                            metadata$v_eea_old %in% c("PC", "LCV"), ]

reg <- unique(fuel$region)

for(k in seq_along(reg)) {

  cat("\n\n", reg[k],  "\n")

  for (i in seq_along(metadata_cold$vehicles)) {

    cat("\n", metadata_cold$vehicles[i],
        rep("", max(nchar(metadata_cold$vehicles) + 1) - nchar(metadata_cold$vehicles[i]))
    )

    x <- readRDS(paste0("veh/", metadata_cold$vehicles[i], ".rds"))

    x[is.na(x)] <- 0

    x <- x[region == reg[k], ]

    x$region <- NULL

    setDF(x)

    # euro
    cate <- suppressWarnings(
      as.character(as.roman(gsub("Euro ", "",
                                 euro[[metadata_cold$vehicles[i]]]))))
    cate[is.na(cate)] <- "PRE"

    dm <- pmonth[region == reg[k] &
                   fuel == metadata_cold$fuel[i]]$consumption_t

    for (j in seq_along(pol)) {

      f_fcorr <- fcorr[veh == metadata_cold$v_eea_old_fuel[i] &
                         pol == pol[j]]

      # deterioration factor
      if(pol[j] %in% c("CO", "NO", "NO2", "HC", "NMHC")) {

        (efdet <- emis_det(po = ifelse(
          pol[j] %in% c("NO", "NOx", "NO2"),"NOx",
          ifelse(
            pol[j] %in% c("NMHC", "HC"),"HC",
            "CO"
          )),
          cc = "<=1400",
          eu = cate[1:ncol(x)],
          km = cumsum(mileage[[metadata$vehicles[i]]])))
      } else {
        efdet <- 1
      }

      cat(pol[j], " ")

      ltrip <- add_lkm(metadata_cold$km_cycle[i])

      ta <- met[region == unique(region)[k]]$Temperature #!

      a <- cold_mileage(ltrip = ltrip, ta = celsius(ta))

      ef <- ef_ldv_speed(v = metadata_cold$v_eea_old[i],
                         t = metadata_cold$t_eea_old[i],
                         cc = metadata_cold$cc_eea_old[i],
                         f = if(metadata_cold$fuel_eea_old[i] == "LPG" & pol[j] == "PM") "G" else  metadata_cold$fuel_eea_old[i],
                         p = pol[j],
                         eu = cate,
                         speed = Speed(metadata_cold$speed[i]),
                         fcorr = if(nrow(f_fcorr) == 0) rep(1, 8) else f_fcorr$value)[1:length(x)]
      ef <- ef*efdet

      if(metadata$fuel_eea_old[i] == "LPG" & pol[j] == "PM") ef <- ef*0


      kk <- 1

      poly <- ifelse(
        pol[j] %in% c("NO", "NO2"), "NOx",
        ifelse(
          pol[j] %in% c("NMHC",
                        "CH4"), "HC",
          ifelse(
            pol[j] %in% c("CO2"), "FC",
            pol[j]
          )))


      kk <- ifelse(
        pol[j] %in% c("NO", "NMHC"), 0.9,
        ifelse(
          pol[j] %in% c("NO2",
                        "CH4"), 0.1,
          ifelse(
            pol[j] == "CO2",
            44/(12.011+1.008*1.8+16*0),
            1
          )))


      efcold <- ef_ldv_cold(ta = matrix(ta, nrow = 1),
                            cc = ifelse(metadata_cold$cc_eea_old[i] == "<3.5",
                                        ">2000",
                                        metadata_cold$cc_eea_old[i]),
                            f = metadata_cold$fuel_eea_old[i],
                            p = poly,
                            eu = cate,
                            speed = Speed(metadata_cold$speed[i]),
                            fcorr = if(nrow(f_fcorr) == 0) rep(1, 8) else f_fcorr$value,
                            k = kk)

      nrow(x) ==  nrow(ef)

      ef$speed <- NULL

      array_x <- emis_cold_td(
        veh = x,
        lkm = mileage[[metadata_cold$vehicles[i]]],
        ef = ef[, 1:ncol(x)],
        efcold = efcold[, 1:ncol(x)],
        fortran = TRUE,
        beta = matrix(a, nrow = 1),
        nt = check_nt()/2,
        pro_month = dm,
        verbose = verbose,
        params = list(
          veh = metadata_cold$vehicles[i],
          size = metadata_cold$size[i],
          fuel = metadata_cold$fuel[i],
          pollutant = pol[j],
          type_emi = "Cold",
          subtype_emi = "Exhaust",
          baseyear = year,
          month = rep(1:12, each = ncol(x))
        )
      )

      fwrite(array_x, "emi/cold_exhaust.csv", append = TRUE)
    }
  }
}

switch(language,
       "portuguese" = message("\nEmissões em: /emi/exhaust.csv:"),
       "english" = message("\nEmissions in: /emi/exhaust.csv"),
       "spanish" = message("\nEmisiones en: /emi/exhaust.csv")
)


switch(language,
       "portuguese" = message("Limpando..."),
       "english" = message("Cleaning..."),
       "spanish" = message("Limpiando...")
)

suppressWarnings(
  rm(
    i, j, pol,
    n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC,
    ns, ln, p, df, dl, cores
  )
)

ls()


invisible(gc())
