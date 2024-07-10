
options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(stars)
library(eixport)
sessionInfo()

system("rm wrf/CBMZ/*")

# WRF CHEM
co <- 1
o3 <- 1
no2 <- 0.9
no <- 1.5
pm <- 1


# type only grids
k <- 1
language <- "portuguese" # english spanish
for(kk in 1:2){
  mech <- "CBMZ"
  dir.create("wrf")
  dir.create(paste0("wrf/", mech))
  g1 <- readRDS("../../wrf/gd01.rds")
  dim(g1)
  g2 <- readRDS("../../wrf/gd02.rds")
  dim(g2)

  for( k in seq_along(mech)) {
    language       <- "portuguese" # english spanish
    # net            <- readRDS("network/net.rds")
    domain         <- kk
    cols           <- ifelse(domain == 2, 198, 179)
    rows           <- ifelse(domain == 2, 219, 179)
    n_aero         <- 15
    wrf_times      <- 24 # 
    pasta_wrfinput <- "../../wrf/"
    pasta_wrfchemi <- "wrf"
    # dir.create(paste0(pasta_wrfchemi, "/", mech[k]))
    dom            <- paste0("d0", domain,"_")
    wrfi           <- paste0("../../wrf/wrfinput_d0", domain)
    hours          <- 0
    tz             <- "America/Sao_Paulo"
    month          <- 7
    lf             <- paste0("post/spec_grid/", dom, "emis_grid_07_mol.rds")

    source("scripts/wrf_country.R", encoding = "UTF-8")
  }
}

file.copy(paste0("wrf/",mech,"/wrfchemi_00z_d01"),
          paste0("wrf/",mech,"/wrfchemi_00z_d01_vein")
)

file.copy(paste0("wrf/",mech,"/wrfchemi_12z_d01"),
          paste0("wrf/",mech,"/wrfchemi_12z_d01_vein")
)

file.copy(paste0("wrf/",mech,"/wrfchemi_00z_d02"),
          paste0("wrf/",mech,"/wrfchemi_00z_d02_vein")
)

file.copy(paste0("wrf/",mech,"/wrfchemi_12z_d02"),
          paste0("wrf/",mech,"/wrfchemi_12z_d02_vein")
)


