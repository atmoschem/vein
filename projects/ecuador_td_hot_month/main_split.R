# split version of main.R: the pipeline is divided into
# scripts/NN_main_*.R and sourced here in order
message("\n== 01_main_config.R | configuration ==")
source("scripts/01_main_config.R", encoding = "UTF-8")
message("\n== 02_main_network.R | network ==")
source("scripts/02_main_network.R", encoding = "UTF-8")
message("\n== 03_main_traffic.R | monthly traffic ==")
source("scripts/03_main_traffic.R", encoding = "UTF-8")
message("\n== 04_main_fuel.R | monthly fuel consumption ==")
source("scripts/04_main_fuel.R", encoding = "UTF-8")
message("\n== 05_main_exhaust.R | hot exhaust ==")
source("scripts/05_main_exhaust.R", encoding = "UTF-8")
message("\n== 06_main_plots.R | plots ==")
source("scripts/06_main_plots.R", encoding = "UTF-8")
