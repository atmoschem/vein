

# OS
sep <- ifelse(Sys.info()[["sysname"]] == "Windows", "00%3A", ":")

wrfi <- paste0(dir_wrfinput, "/wrfinput_d0", domain)

# tempos
ti <- wrf_get(file = wrfi, name = "Times")

switch(language,
  "portuguese" = cat("Primer tempo de WRF:", as.character(ti)[1], "\n"),
  "english" = cat("First WRF time:", as.character(ti)[1], "\n"),
  "spanish" = cat("Primer tiempo de WRF:", as.character(ti)[1], "\n")
)

# ltemissions, first monday 00:00 before ti
timepos <- as.POSIXct(as.character(ti), format = "%Y-%m-%d_%H:%M:%S")
oneweek <- timepos - 3600 * 24 * 7

df_time <- data.frame(
  times = seq.POSIXt(from = oneweek, to = timepos, by = "hour")
)

df_time$wday <- strftime(df_time$times, "%u")
df_time$hour <- hour(df_time$times)
lt_emissions <- df_time[df_time$wday == 1 & df_time$hour == 0, ]$times[1]

switch(language,
  "portuguese" = cat(
    "Segunda-feira 00:00 anterior do primeiro tempo WRF:",
    as.character(lt_emissions), "\n"
  ),
  "english" = cat(
    "Monday 00:00, previous of first WRF time:",
    as.character(lt_emissions), "\n"
  ),
  "spanish" = cat(
    "Lunes 00:00 antes del primer tiempo WRF:",
    as.character(lt_emissions), "\n"
  )
)


ti <- as.POSIXct(ti)

dir_mech <- paste0("post/", mech)

# creating directory wrf/mech
dir_wrfchemi <- paste0(dir_wrfchemi, "/", mech)
dir.create(path = dir_wrfchemi)

# Grades
lf <- list.files(path = dir_mech, pattern = ".rds", full.names = TRUE)
na <- list.files(path = dir_mech, pattern = ".rds", full.names = F)
emis_option <- na <- gsub(".rds", "", na)


if (mech %in% c("MOZT1", "CBMZ")) {
  emis_option <- ifelse(emis_option == "E_ETOH", "E_C2H5OH", emis_option)
  na <- ifelse(na == "E_ETOH", "E_C2H5OH", na)
}

if (io_style_emissions == 1) {
  # emissions on 00z / 12z style, create the 12z ####
  wrfc <- eixport::wrf_create(
    wrfinput_dir = dir_wrfinput,
    wrfchemi_dir = dir_wrfchemi,
    domains = domain,
    io_style_emissions = 1,
    day_offset = 0,
    variables = emis_option,
    verbose = TRUE,
    n_aero = n_aero,
    return_fn = TRUE
  )

  wrfc <- eixport::wrf_create(
    wrfinput_dir = dir_wrfinput,
    wrfchemi_dir = dir_wrfchemi,
    domains = domain,
    io_style_emissions = 1,
    day_offset = 0.5,
    variables = emis_option,
    verbose = TRUE,
    n_aero = n_aero,
    return_fn = TRUE
  )

  for (i in 1:length(na)) {
    print(na[i])
    x <- readRDS(lf[i])

    xx <- emis_order(
      x = x,
      lt_emissions = lt_emissions,
      start_utc_time = ti,
      desired_length = wrf_times,
      tz_lt = Sys.timezone(),
      seconds = hours * 3600,
      verbose = TRUE
    )

    # 0-12
    gx <- GriddedEmissionsArray(
      x = xx[, 1:12],
      cols = cols, # if towdown, cols
      rows = rows, # if towdown, rows
      times = 12,
      rotate = "cols"
    )
    wrf_put(
      file = paste0(dir_wrfchemi, "/wrfchemi_00z_d0", domain),
      name = na[i],
      POL = gx
    )

    # 12-0
    gx <- GriddedEmissionsArray(
      x = xx[, 13:24],
      cols = cols, # if towdown, cols
      rows = rows, # if towdown, rows
      times = 12,
      rotate = "cols"
    )
    wrf_put(
      file = paste0(dir_wrfchemi, "/wrfchemi_12z_d0", domain),
      name = na[i],
      POL = gx
    )
  }
} else if (io_style_emissions == 2) {
  # emissions for all hours ####

  wrfc <- wrf_create(
    wrfinput_dir = dir_wrfinput,
    wrfchemi_dir = paste0("wrf/", mech),
    io_style_emissions = io_style_emissions,
    domains = domain,
    frames_per_auxinput5 = wrf_times,
    auxinput5_interval_m = 60,
    variables = emis_option,
    separator = "_",
    verbose = TRUE,
    return_fn = TRUE
  )


  for (i in 1:length(na)) {
    print(na[i])
    x <- readRDS(lf[i])

    xx <- emis_order(
      x = x,
      lt_emissions = lt_emissions,
      start_utc_time = ti,
      desired_length = wrf_times,
      tz_lt = Sys.timezone(),
      seconds = hours * 3600,
      verbose = TRUE
    )
    gx <- GriddedEmissionsArray(
      x = xx,
      cols = cols,
      rows = rows,
      times = wrf_times,
      rotate = rotate,
    )
    wrf_put(
      file = wrfc,
      name = na[i],
      POL = gx
    )
  }
} else {
  stop("io_style_emissions can have values 1 (0-12z files) or 2 (all hours file)")
}

message(paste0("WRFCHEMI in wrf/", wrfc))


png(
  filename = paste0("WRF_NO0.png"),
  width = 2100, height = 1500, units = "px", pointsize = 12,
  bg = "white", res = 300
)
a <- wrf_get(wrfc, "E_NO", as_raster = T)
a <- a[[1]]
a[] <- ifelse(a[] <= 0, NA, a[])
print(sp::spplot(a,
  col.regions = cpt(rev = T),
  main = paste(names(a), "mol/km²/h"),
  scales = list(Draw = T)
))
dev.off()

png(
  filename = paste0("WRF_NO12.png"),
  width = 2100, height = 1500, units = "px", pointsize = 12,
  bg = "white", res = 300
)
a <- wrf_get(wrfc, "E_NO", as_raster = T)
a <- a[[1]]
a[] <- ifelse(a[] <= 0, NA, a[])
print(sp::spplot(a,
  col.regions = cpt(rev = T),
  main = paste(names(a), "mol/km²/h"),
  scales = list(Draw = T)
))
dev.off()

png(
  filename = paste0("WRF_CO0.png"),
  width = 2100, height = 1500, units = "px", pointsize = 12,
  bg = "white", res = 300
)
a <- wrf_get(wrfc, "E_CO", as_raster = T)
a <- a[[1]]
a[] <- ifelse(a[] <= 0, NA, a[])
print(sp::spplot(a,
  col.regions = cpt(rev = T),
  main = paste(names(a), "mol/km²/h"),
  scales = list(Draw = T)
))
dev.off()

ls()

suppressWarnings(
  rm(
    "cols", "dfwrf", "domain", "dx", "emis_opt", "emis_option",
    "fCO", "fHC", "firsthour", "fNOx", "fPM10", "fPM2.5", "g_ch3oh",
    "g_eth", "g_hc3", "g_hc5", "g_hc8", "g_iso", "g_ket", "g_ol2", "g_oli",
    "g_olt", "g_tol", "g_xyl", "grids", "i", "lasthour", "mm_no2", "mm_x",
    "net", "no2_mol", "dir_wrfchemi", "dir_wrfinput", "polss", "rows", "ti",
    "voc", "vocB5EX", "vocE100EV", "vocE100EX", "vocE25EV", "vocE25EX", "wrf_times", "wrfi", "x", "xx"
  )
)