# split version of main.R: the pipeline is divided into
# scripts/NN_main_*.R and sourced here in order
message("\n== 01_main_config_fleet.R | fleet by age ==")
source("scripts/01_main_config_fleet.R", encoding = "UTF-8")
message("\n== 02_main_config_fuel.R | fuel consumption ==")
source("scripts/02_main_config_fuel.R", encoding = "UTF-8")
message("\n== 03_main_config_mileage.R | mileage ==")
source("scripts/03_main_config_mileage.R", encoding = "UTF-8")
message("\n== 04_main_config_met.R | meteorology and rain ==")
source("scripts/04_main_config_met.R", encoding = "UTF-8")
# 05_main_config_nroads.R | road counts per region: disabled, the block is
# commented out in main.R (the script reads absolute paths outside the project)
# message("\n== 05_main_config_nroads.R | road counts per region ==")
# source("scripts/05_main_config_nroads.R", encoding = "UTF-8")
message("\n== 06_main_estimation.R | unpack project into estimation/<year> ==")
source("scripts/06_main_estimation.R", encoding = "UTF-8")
