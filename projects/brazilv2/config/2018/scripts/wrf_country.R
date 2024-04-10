#OS
sep <- ifelse(Sys.info()[["sysname"]] == "Windows","00%3A", ":")

#tempos
ti <- wrf_get(file = wrfi,  name = "Times")
# ti <- as.POSIXct(as.character(ti), tz)

switch(language,
       "portuguese" = cat("Primer tempo de WRF:", as.character(ti)[1], "\n"),
       "english" = cat("First WRF time:", as.character(ti)[1], "\n"),
       "spanish" = cat("Primer tiempo de WRF:", as.character(ti)[1], "\n"))



# ltemissions, first monday 00:00 before ti
timepos <- as.POSIXct(as.character(ti), 
                      format = "%Y-%m-%d_%H:%M:%S", 
                      tz)
oneweek <- timepos - 3600*24*7

df_time <- data.frame(
  times = seq.POSIXt(from = oneweek, to = timepos, by = "hour")
)

df_time$wday <- strftime(df_time$times, "%u")
df_time$hour <- hour(df_time$times)
lt_emissions <- df_time[df_time$wday == 1 & 
                          df_time$hour == 0, ]$times[1]

switch(
  language,
  "portuguese" = cat("Segunda-feira 00:00 anterior do primeiro tempo WRF:", 
                     as.character(lt_emissions), "\n"),
  "english" = cat("Monday 00:00, previous of first WRF time:", 
                  as.character(lt_emissions), "\n"),
  "spanish" = cat("Lunes 00:00 antes del primer tiempo WRF:", 
                  as.character(lt_emissions), "\n"))


wrfc <- paste0("wrfchemi_d0", domain, "_", as.character(ti))

if(Sys.info()[["sysname"]] == "Windows") wrfc <- gsub("00:", sep, wrfc)

ti <- as.POSIXct(ti)

# Grades
lf <- paste0("post/spec_grid/",
             dom,
             "emis_grid_0",
             month,
             "_mol.rds")

x <- readRDS(lf)
names(x)

na <- names(x)[2:ncol(x)]
na[1:25] <- paste0("E_", na[1:25]) #! CRITICAL
names(x) <- c("id", na)

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



for(i in 1:length(na)){
  print(na[i])
  
  wrf_put(file = paste0(pasta_wrfchemi, 
                        "/",
                        mech,
                        "/wrfchemi_00z_d0",
                        domain),  
          name = na[i],  
          POL =   rep(matrix(x[[na[i]]], 
                             nrow = rows, 
                             ncol = cols)[, cols:1], 12)*ifelse(
                               na[i] == "E_CO", co,
                               ifelse(
                                 na[i] == "E_NO2", no2,
                                 ifelse(
                                   na[i] == "E_NO", no,
                                   ifelse(
                                     na[i] %in% c("E_SO4I",
                                                  "E_SO4J",
                                                  "E_NO3I",
                                                  "E_NO3J",
                                                  "E_PM25I",
                                                  "E_PM25J",
                                                  "E_ORGI",
                                                  "E_ORGJ",
                                                  "E_ECI",
                                                  "E_ECJ",
                                                  "E_PM_10",
                                                  "E_PM10" ), pm, 
                                     1))))
          )

  wrf_put(file = paste0(pasta_wrfchemi, 
                        "/",
                        mech,
                        "/wrfchemi_12z_d0",
                        domain),  
          name = na[i],  
          POL =   rep(matrix(x[[na[i]]], 
                             nrow = rows, 
                             ncol = cols)[, cols:1], 12)*ifelse(
                               na[i] == "E_CO", co,
                               ifelse(
                                 na[i] == "E_NO2", no2,
                                 ifelse(
                                   na[i] == "E_NO", no,
                                   ifelse(
                                     na[i] %in% c("E_SO4I",
                                                  "E_SO4J",
                                                  "E_NO3I",
                                                  "E_NO3J",
                                                  "E_PM25I",
                                                  "E_PM25J",
                                                  "E_ORGI",
                                                  "E_ORGJ",
                                                  "E_ECI",
                                                  "E_ECJ",
                                                  "E_PM_10",
                                                  "E_PM10" ), pm, 
                                     1))))
          )
  
  }



png(filename =  paste0("images/WRF_",
                       dom,
                       "NO.png"),
    width = 2100, 
    height = 1500, 
    units = "px", 
    pointsize = 12,
    bg = "white",  
    res = 300)

a <- wrf_get(paste0(pasta_wrfchemi, 
                    "/",
                    mech,
                    "/wrfchemi_12z_d0",
                    domain), "E_NO", as_raster = T)
a <- a[[1]]
a[] <- ifelse(a[] <=0, NA, a[])

newproj <- "+proj=longlat +datum=WGS84"

b <- raster::projectRaster(a, crs=newproj)
print(sp::spplot(b, 
                 col.regions = cpt(rev = T), 
                 main = paste0("E_NO 12:00 domain ", domain, " mol/km2/h"), 
                 scales = list(Draw = T)))

dev.off()

png(filename =  paste0("images/WRF_",
                       dom,
                       "CO.png"),
    width = 2100, 
    height = 1500, 
    units = "px", 
    pointsize = 12,
    bg = "white",  
    res = 300)

a <- wrf_get(paste0(pasta_wrfchemi, 
                    "/",
                    mech,
                    "/wrfchemi_12z_d0",
                    domain), "E_CO", as_raster = T)
a <- a[[1]]
a[] <- ifelse(a[] <=0, NA, a[])
newproj <- "+proj=longlat +datum=WGS84"

b <- raster::projectRaster(a, crs=newproj)
print(sp::spplot(b, 
                 col.regions = cpt(rev = T), 
                 main = paste0("E_CO 12:00 domain ", domain, " mol/km2/h"), 
                 scales = list(Draw = T)))

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
