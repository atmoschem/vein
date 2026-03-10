dir.create("wrf/", showWarnings = FALSE)

# OS
sep <- ifelse(Sys.info()[["sysname"]] == "Windows", "00%3A", ":")

# tempos
ti <- wrf_get(file = wrfi, name = "Times")

switch(
  language,
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

switch(
  language,
  "portuguese" = cat(
    "Segunda-feira 00:00 anterior do primeiro tempo WRF:",
    as.character(lt_emissions),
    "\n"
  ),
  "english" = cat(
    "Monday 00:00, previous of first WRF time:",
    as.character(lt_emissions),
    "\n"
  ),
  "spanish" = cat(
    "Lunes 00:00 antes del primer tiempo WRF:",
    as.character(lt_emissions),
    "\n"
  )
)

# wrfc <- paste0("wrfchemi_d0", domain, "_", as.character(ti))
# if (Sys.info()[["sysname"]] == "Windows") wrfc <- gsub("00:", sep, wrfc)
#
ti <- as.POSIXct(ti)

domain <- substr(x = wrfi, nchar(wrfi), nchar(wrfi))

# Grades
lf <- list.files(
  path = "post/spec_grid",
  pattern = ".rds",
  full.names = TRUE
)

for (kk in seq_along(lf)) {
  dir.create(paste0("wrf/", sprintf("%02d", kk)))

  gx <- readRDS(lf[kk])[, -c("id", "geometry")]

  na <- names(gx)

  # emissions on 00z / 12z style, create the 12z
  eixport::wrf_create(
    wrfinput_dir = pasta_wrfinput,
    wrfchemi_dir = paste0("wrf/", sprintf("%02d", kk)),
    domains = domain,
    io_style_emissions = 1,
    day_offset = 0,
    variables = na,
    verbose = TRUE,
    n_aero = 15,
    return_fn = T
  ) -> a1

  eixport::wrf_create(
    wrfinput_dir = pasta_wrfinput,
    wrfchemi_dir = paste0("wrf/", sprintf("%02d", kk)),
    domains = domain,
    io_style_emissions = 1,
    day_offset = 0.5,
    variables = na,
    verbose = TRUE,
    n_aero = n_aero,
    return_fn = T
  ) -> a2

  for (i in 1:length(na)) {
    print(na[i])
    x <- readRDS(lf[kk])[, na[i], with = F]

    wrf_put(
      file = a1,
      name = na[i],
      POL = rep(x[[1]], 12)
    )

    wrf_put(
      file = a2,
      name = na[i],
      POL = rep(x[[1]], 12)
    )
  }
}

cat("\nWRFCHEMI in \n")
cat(paste(a1, a2, sep = "\n"))
