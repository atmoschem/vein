
source("config/r/01_fleet.R")
rm(list = ls())
gc()


source("config/r/02_fuel.R")
rm(list = ls())
gc()
source("config/r/03_mileage.R")
rm(list = ls())
gc()
source("config/r/04_met.R")
rm(list = ls())
gc()

# source("config/r/05_nroads.R")
# rm(list = ls())
# gc()

# estimation ####
# inventory template (tar.gz of a vein project) unpacked into estimation/<year>/
uf <- "config/argentina.tar.gz"
if (!file.exists(uf)) stop(uf, " not found: copy the inventory template there")
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
