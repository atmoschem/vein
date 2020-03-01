#OS
sep <- ifelse(Sys.info()[["sysname"]] == "Windows","00%3A", ":")

#tempos
ti <- wrf_get(file = wrfi,  name = "Times")
cat("Primer tempo de WRF:", as.character(ti)[1], "\n")

wrfc <- paste0("wrfchemi_d0", domain, "_", as.character(ti))
if(Sys.info()[["sysname"]] == "Windows") wrfc <- gsub("00:", sep, wrfc)

ti <- as.POSIXct(ti)

# emissions on 00z / 12z style, create the 12z
eixport::wrf_create(wrfinput_dir         = pasta_wrfinput,
                    wrfchemi_dir         = pasta_wrfchemi,
                    domains              = domain,
                    io_style_emissions   = 1,
                    day_offset           = 0, 
                    variables            = emis_option,
                    verbose              = TRUE)

eixport::wrf_create(wrfinput_dir         = pasta_wrfinput,
                    wrfchemi_dir         = pasta_wrfchemi,
                    domains              = domain,
                    io_style_emissions   = 1,
                    day_offset           = 0.5,
                    variables            = emis_option,
                    verbose              = TRUE)

# Emissiones for the whole period
wrf_create(wrfinput_dir         = pasta_wrfinput,
           wrfchemi_dir         = pasta_wrfchemi,
           domains              = domain,
           frames_per_auxinput5 = wrf_times,
           auxinput5_interval_m = 60,
           variables            = emis_option,
           verbose              = TRUE)


# Grades
lf <- paste0("post/grids/", pol, ".rds")

for(i in seq_along(pol)) {
  x <- readRDS(lf[i])
  
  xx <- emis_order2(x = x,
                    lt_emissions = lt_emissions,
                    start_utc_time = ti,
                    desired_length = wrf_times,
                    tz_lt = Sys.timezone(),
                    verbose = TRUE)
  
  # 0-12
  gx <- GriddedEmissionsArray(x = xx[, 1:12], 
                              cols = cols, 
                              rows = rows, 
                              times = 12)
 wrf_put(file = "wrf/wrfchemi_00z_d02",  
         name = paste0("E_",pol[i]),  
         POL =   gx*peso_molecular[i])
  
 # 12-0
 gx <- GriddedEmissionsArray(x = xx[, 13:24], 
                             cols = cols, 
                             rows = rows, 
                             times = 12)
 wrf_put(file = "wrf/wrfchemi_12z_d02",  
         name = paste0("E_",pol[i]),  
         POL =   gx*peso_molecular[i])
 
 
 # periodo
 gx <- GriddedEmissionsArray(x = xx, 
                             cols = cols, 
                             rows = rows, 
                             times = wrf_times)
 wrf_put(file = paste0("wrf/", wrfc),  
         name = paste0("E_",pol[i]),  
         POL =   gx*peso_molecular[i])
 
 
 }
png(filename =  paste0("images/WRF_E_NO_00z.png"),
    width = 2100, height = 1500, units = "px", pointsize = 12,
    bg = "white",  res = 300)
wrf_plot("wrf/wrfchemi_00z_d02", "E_NO", col = cpt(n = 15))
dev.off()

png(filename =  paste0("images/WRF_E_NO_12z.png"),
    width = 2100, height = 1500, units = "px", pointsize = 12,
    bg = "white",  res = 300)
wrf_plot("wrf/wrfchemi_12z_d02", "E_NO", col = cpt(n = 11))
dev.off()

png(filename =  paste0("images/WRF_E_CO_.png"),
    width = 2100, height = 1500, units = "px", pointsize = 12,
    bg = "white",  res = 300)
wrf_plot(paste0("wrf/", wrfc), "E_CO", col = cpt(n = 22))
dev.off()

ls()

suppressWarnings(
  rm("cols", "domain", "emis_opt", "emis_option", "gx", "i", "lf", "net",
   "pasta_wrfchemi", "pasta_wrfinput", "peso_molecular", "pol", "rows", 
   "ti", "wrfi", "x", "xx",
   "lt_emissions", "sep", "sL1", "sL2", "sL3", "sL4", "wrf_times")
)
gc()

