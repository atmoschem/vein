# module load R && R
setwd("/glade/scratch/sibarra/emis_brazil/")
library(terra)
library(colorout)
library(data.table)
library(cptcity)
cams <- list.files("cams", 
                 full.name = T)

v1 <- list.files("nc_2018", 
                 full.name = T)

v2 <- list.files("nc_2019", 
                 full.name = T)
# mask ####
mask <- rast(v1[1])[[1]]
mask[] <- ifelse(mask[] > 0, NA, 1)
mask[] <- ifelse(is.na(mask[]), 0, 1 )

# time
dt <- data.table(
    time = seq.Date(as.Date("2000-01-01"), 
                    as.Date("2023-12-01"), 
                    by = "month"))

dt[, id := 1:.N]

# acetylene - voc9 ####
cx <- rast(cams[1], "tro")
cx[[dt[year(time) %in% 2018:2019]$id]] <- cx[[dt[year(time) %in% 2018:2019]$id]] * mask
#plot(cx[[dt[year(time) %in% 2018:2019]$id[1]]], col = cpt(rev = T))
#maps::map(add = T)

voc9 <- rast(list(rast("nc_2018/voc9.nc"),
                  rast("nc_2019/voc9.nc")))

#plot(voc9[[1]], col = cpt(rev = T))
#maps::map(add = T)


cx[[dt[year(time) %in% 2018:2019]$id]] <- cx[[dt[year(time) %in% 2018:2019]$id]] + voc9

terra::writeCDF(cx, 
                "voc9.nc", 
                overwrite = T)

