#library(sf)
#a <- geobr::read_state()
#a <- st_transform(a, 3857)
#saveRDS(a, "rds/ufs.rds")

# cp <- list.files("config", pattern = ".rds", full.names = T)
# 
# lapply(cp, file.copy, "config/UFBR/config")
# file.copy("config/inventory_all.xlsx", 
#           "config/UFBR/config")
# setwd("config/UFBR")
# source("config/clean.R")
# setwd("../../")

uf <- "config/2018.tar.gz"
#dir.create("estimation")
unlink("estimation", recursive = T)
dir.create("estimation")


years <- 2018

lapply(seq_along(years), function(i) {
    print(paste0("estimation/", years, "/")[i])

    untar(tarfile = uf, 
          exdir = paste0("estimation/", years[i], "/"))
    # file.remove(paste0("estimation/", years, "/", ufs[j], "/UFBR.tar.gz")[i])
})
