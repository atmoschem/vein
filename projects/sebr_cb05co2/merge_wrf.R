library(sf)
library(geobr)
library(eixport)
library(raster)
library(ncdf4)
st <- read_state()
g <- st_as_sfc(st_bbox(wrf_grid("wrfinput_d01")))
plot(st[st$abbrev_state %in% c("MG", "SP", "RJ"), "abbrev_state"], axes = T, reset = F)
plot(g, add = T)
# prepare data
untar(tarfile = "MG.tar.gz", exdir = "MG")
untar(tarfile = "SP.tar.gz", exdir = "SP")
untar(tarfile = "RJ.tar.gz", exdir = "RJ")

setwd("MG")
source("main.R")

setwd("../RJ")
source("main.R")

setwd("../SP")
source("main.R")


setwd("..")
w11 <- "MG/wrf/wrfchemi_00z_d01"
w12 <- "MG/wrf/wrfchemi_12z_d01"

w21 <- "RJ/wrf/wrfchemi_00z_d01"
w22 <- "RJ/wrf/wrfchemi_12z_d01"

w31 <- "SP/wrf/wrfchemi_00z_d01"
w32 <- "SP/wrf/wrfchemi_12z_d01"


file.copy("SP/wrf/wrfchemi_00z_d01", "wrfchemi_00z_d01", overwrite = T)
file.copy("SP/wrf/wrfchemi_12z_d01", "wrfchemi_12z_d01", overwrite = T)
w71 <- "wrfchemi_00z_d01"
w72 <- "wrfchemi_12z_d01"

nc <- nc_open(w11)
nx <- names(nc$var)
nc11 <- nc_open(w11)
nc12 <- nc_open(w12)

nc21 <- nc_open(w21)
nc22 <- nc_open(w22)

nc31 <- nc_open(w31)
nc32 <- nc_open(w32)


nxx <- nx[4:length(nx)]
for (i in seq_along(nxx)) {
  print(nxx[i])
  wrf_put(
    file = w71,
    name = nxx[i],
    POL =
      ncvar_get(nc = nc11, varid = nxx[i]) +
        ncvar_get(nc = nc21, varid = nxx[i]) +
        ncvar_get(nc = nc31, varid = nxx[i])
  )

  wrf_put(
    file = w72,
    name = nxx[i],
    POL =
      ncvar_get(nc = nc12, varid = nxx[i]) +
        ncvar_get(nc = nc22, varid = nxx[i]) +
        ncvar_get(nc = nc32, varid = nxx[i])
  )
}

library(stars)
library(ggplot2)

st <- read_state()
w71 <- "wrfchemi_00z_d01"
w72 <- "wrfchemi_12z_d01"
r1 <- read_ncdf(w71, var = "E_CO2")
ggplot() +
  geom_stars(data = r1) +
  scale_fill_gradientn(colors = cptcity::cpt(pal = "neota_othr_dragon_foot")) +
  coord_equal() +
  theme_void()