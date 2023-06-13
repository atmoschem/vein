
source("config/01_fleet_brazil.R")
source("config/02_fuel.R")
source("config/03_mileage.R")
source("config/04_met.R")
source("config/05_pre_main.R")

x <- list.files(path="estimation", pattern = "main.R", recursive = T, full.names = T)
xx <- paste0("/home/sibarra/BR/estimation", 
             x[!grepl(pattern = ".Rproj", x)])
xx <- gsub("main.R", "", xx)
saveRDS(xx, "/p1-baal/sibarra/brazil/mains.rds")

for (i in 1:810) {
  
  di <- readRDS("/p1-baal/sibarra/brazil/mains.rds")
  setwd(di[i])
  print(di[i])

  sink(paste0(di[i], "/log.txt"))
  print(paste0(di[i], "/log.txt"))
  
  source(file = "main.R", echo = T)
  gc()
  sink()

}


