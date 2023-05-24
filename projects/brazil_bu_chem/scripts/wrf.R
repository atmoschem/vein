dir.create(paste0("wrf/", mech), showWarnings = FALSE)

# OS
sep <- ifelse(Sys.info()[["sysname"]] == "Windows", "00%3A", ":")

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

switch(
  language,
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

# wrfc <- paste0("wrfchemi_d0", domain, "_", as.character(ti))
# if (Sys.info()[["sysname"]] == "Windows") wrfc <- gsub("00:", sep, wrfc)
# 
ti <- as.POSIXct(ti)

# Grades
lf <- list.files(path = paste0("post/mech/", mech, "/grids"),
                 pattern = ".rds", 
                 full.names = TRUE)

na <- list.files(path = paste0("post/mech/", mech, "/grids"), 
                 pattern = ".rds", 
                 full.names = F)
na <- gsub(".rds", "", na)

# emissions on 00z / 12z style, create the 12z
eixport::wrf_create(
  wrfinput_dir = pasta_wrfinput,
  wrfchemi_dir = paste0(pasta_wrfchemi, "/", mech),
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
  wrfchemi_dir = paste0(pasta_wrfchemi, "/", mech),
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
    cols = rows, # if towdown, cols
    rows = cols, # if towdown, rows
    times = 12,
    rotate = "cols"
  )
  wrf_put(
    file = a1,
    name = na[i],
    POL = gx
  )
  
  # 12-0
  gx <- GriddedEmissionsArray(
    x = xx[, 13:24],
    cols = rows, # if towdown, cols
    rows = cols, # if towdown, rows
    times = 12,
    rotate = "cols"
  )
  wrf_put(
    file = a2,
    name = na[i],
    POL = gx
  )
}

cat("\nWRFCHEMI in \n")
cat(paste(a1, a2, sep = "\n"))


png(
  filename = paste0("images/WRF_NO0.png"),
  width = 2100, 
  height = 1500, 
  units = "px", 
  pointsize = 12,
  bg = "white", 
  res = 300
)
a <- wrf_get(paste0(pasta_wrfchemi, "/", 
                    mech,
                    "/wrfchemi_00z_d0", 
                    domain), 
             "E_NO", 
             as_raster = T)
a <- a[[1]]
a[] <- ifelse(a[] <= 0, NA, a[])
print(sp::spplot(a,
                 col.regions = cpt(rev = T),
                 main = names(a),
                 scales = list(Draw = T)
))
dev.off()

png(
  filename = paste0("images/WRF_NO12.png"),
  width = 2100, 
  height = 1500, 
  units = "px", 
  pointsize = 12,
  bg = "white", 
  res = 300
)
a <- wrf_get(paste0(pasta_wrfchemi, "/", 
                    mech,
                    "/wrfchemi_12z_d0", 
                    domain), "E_NO", 
             as_raster = T)
a <- a[[1]]
a[] <- ifelse(a[] <= 0, NA, a[])
print(sp::spplot(a,
                 col.regions = cpt(rev = T),
                 main = names(a),
                 scales = list(Draw = T)
))
dev.off()

png(
  filename = paste0("images/WRF_CO0.png"),
  width = 2100, 
  height = 1500, 
  units = "px", 
  pointsize = 12,
  bg = "white", 
  res = 300
)
a <- wrf_get(paste0(pasta_wrfchemi, "/", 
                    mech,
                    "/wrfchemi_00z_d0", 
                    domain), "E_CO", 
             as_raster = T)
a <- a[[1]]
a[] <- ifelse(a[] <= 0, NA, a[])
print(sp::spplot(a,
                 col.regions = cpt(rev = T),
                 main = names(a),
                 scales = list(Draw = T)
))
dev.off()
