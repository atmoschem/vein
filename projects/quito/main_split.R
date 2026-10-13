###########################################################
###                                                    ####
###   ANNUAL EMISSIONS INVENTORY FOR QUITO, ECUADOR    ####
###                                                    ####
###########################################################
# split version of main.R: the pipeline is divided into
# scripts/NN_main_*.R and sourced here in order
message("\n== 01_main_config.R | configuration ==")
source("scripts/01_main_config.R", encoding = "UTF-8")
message("\n== 02_main_network.R | network ==")
source("scripts/02_main_network.R", encoding = "UTF-8")
message("\n== 03_main_traffic.R | traffic ==")
source("scripts/03_main_traffic.R", encoding = "UTF-8")
message("\n== 04_main_fuel.R | fuel consumption ==")
source("scripts/04_main_fuel.R", encoding = "UTF-8")
message("\n== 05_main_exhaust.R | hot exhaust + cold start ==")
source("scripts/05_main_exhaust.R", encoding = "UTF-8")
message("\n== 06_main_evaporatives.R | evaporatives ==")
source("scripts/06_main_evaporatives.R", encoding = "UTF-8")
message("\n== 07_main_paved_roads.R | paved roads ==")
source("scripts/07_main_paved_roads.R", encoding = "UTF-8")
message("\n== 08_main_wear.R | wear ==")
source("scripts/08_main_wear.R", encoding = "UTF-8")
message("\n== 09_main_post.R | post-estimation ==")
source("scripts/09_main_post.R", encoding = "UTF-8")
message("\n== 10_main_nc.R | NetCDF ==")
source("scripts/10_main_nc.R", encoding = "UTF-8")
message("\n== 11_main_plots.R | plots ==")
source("scripts/11_main_plots.R", encoding = "UTF-8")
message("\n== 12_main_mech.R | chemical mechanisms ==")
source("scripts/12_main_mech.R", encoding = "UTF-8")
message("\n== 13_main_wrf.R | WRF-Chem ==")
source("scripts/13_main_wrf.R", encoding = "UTF-8")
