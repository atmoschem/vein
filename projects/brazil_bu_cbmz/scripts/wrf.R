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
                    verbose              = TRUE,
                    n_aero               = 15)

eixport::wrf_create(wrfinput_dir         = pasta_wrfinput,
                    wrfchemi_dir         = pasta_wrfchemi,
                    domains              = domain,
                    io_style_emissions   = 1,
                    day_offset           = 0.5,
                    variables            = emis_option,
                    verbose              = TRUE,
                    n_aero               = 15)

# Grades
lf <- list.files(path = "post/spec_grid", pattern = ".rds", full.names = TRUE)
na <- list.files(path = "post/spec_grid", pattern = ".rds", full.names = F)
na <- gsub("PM2.5", "PM25", na)[grep("E_", na)]
na <- gsub(".rds", "", na)
ars <- intersect(na, emis_option)

for(i in 1:length(na)){
  print(na[i])
  print(lf[i])
  x <- readRDS(lf[i])
  
  xx <- emis_order(x = x,
                   lt_emissions = lt_emissions,
                   start_utc_time = ti,
                   desired_length = wrf_times,
                   tz_lt = Sys.timezone(), 
                   seconds = hours*3600,
                   verbose = TRUE)
  
  # 0-12
  gx <- GriddedEmissionsArray(x = xx[, 1:12], 
                              cols = cols, 
                              rows = rows, 
                              times = 12)
  wrf_put(file = paste0(pasta_wrfchemi, "/wrfchemi_00z_d0",domain),  
          name = na[i],  
          POL =   gx)
  
  # 12-0
  gx <- GriddedEmissionsArray(x = xx[, 13:24], 
                              cols = cols, 
                              rows = rows, 
                              times = 12)
  wrf_put(file = paste0(pasta_wrfchemi, "/wrfchemi_12z_d0",domain),  
          name = na[i],  
          POL =   gx)
}

message(paste0("WRFCHEMI in wrf/", wrfc)) 


png(filename =  paste0("images/WRF_NO0.png"),
    width = 2100, height = 1500, units = "px", pointsize = 12,
    bg = "white",  res = 300)
a <- wrf_get(paste0(pasta_wrfchemi, "/wrfchemi_00z_d0",domain), "E_NO", as_raster = T)
a <- a[[1]]
a[] <- ifelse(a[] <=0, NA, a[])
sp::spplot(a, col.regions = cpt(rev = T), main = names(a), scales = list(Draw = T))
dev.off()

png(filename =  paste0("images/WRF_NO12.png"),
    width = 2100, height = 1500, units = "px", pointsize = 12,
    bg = "white",  res = 300)
a <- wrf_get(paste0(pasta_wrfchemi, "/wrfchemi_12z_d0",domain), "E_NO", as_raster = T)
a <- a[[1]]
a[] <- ifelse(a[] <=0, NA, a[])
sp::spplot(a, col.regions = cpt(rev = T), main = names(a), scales = list(Draw = T))
dev.off()

png(filename =  paste0("images/WRF_CO0.png"),
    width = 2100, height = 1500, units = "px", pointsize = 12,
    bg = "white",  res = 300)
a <- wrf_get(paste0(pasta_wrfchemi, "/wrfchemi_00z_d0",domain), "E_CO", as_raster = T)
a <- a[[1]]
a[] <- ifelse(a[] <=0, NA, a[])
sp::spplot(a, col.regions = cpt(rev = T), main = names(a), scales = list(Draw = T))
dev.off()

ls()

suppressWarnings(
  rm("cols", "dfwrf", "domain", "dx", "emis_opt", "emis_option", 
     "fCO", "fHC", "firsthour", "fNOx", "fPM10", "fPM2.5", "g_ch3oh", 
     "g_eth", "g_hc3", "g_hc5", "g_hc8", "g_iso", "g_ket", "g_ol2", "g_oli", 
     "g_olt", "g_tol", "g_xyl", "grids", "i", "lasthour", "mm_no2", "mm_x", 
     "net", "no2_mol", "pasta_wrfchemi", "pasta_wrfinput", "polss", "rows", "ti", 
     "voc", "vocB5EX", "vocE100EV", "vocE100EX", "vocE25EV", "vocE25EX", "wrf_times", "wrfi", "x", "xx")
)
