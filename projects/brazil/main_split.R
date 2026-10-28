# split version of main.R: the pipeline is divided into
# scripts/NN_main_*.R and sourced here in order
message("\n== 01_main_fleet.R | fleet by state and year (config/01_fleet_brazil.R) ==")
source("scripts/01_main_fleet.R", encoding = "UTF-8")
message("\n== 02_main_fuel_data.R | fuel sales by state (config/02_fuel.R) ==")
source("scripts/02_main_fuel_data.R", encoding = "UTF-8")
message("\n== 03_main_mileage.R | mileage curves (config/03_mileage.R) ==")
source("scripts/03_main_mileage.R", encoding = "UTF-8")
message("\n== 04_main_met.R | temperature scenarios (config/04_met.R) ==")
source("scripts/04_main_met.R", encoding = "UTF-8")
message("\n== 05_main_estimation_dirs.R | untar config/UFBR.tar.gz into estimation/YYYY (config/05_pre_main.R) ==")
source("scripts/05_main_estimation_dirs.R", encoding = "UTF-8")
# 06_main_run_years.R: this block is commented out in main.R, so it is not run here
# message("\n== 06_main_run_years.R | run every estimation/YYYY/main.R ==")
# source("scripts/06_main_run_years.R", encoding = "UTF-8")
