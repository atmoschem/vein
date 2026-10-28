# split version of main.R: the pipeline is divided into
# scripts/NN_main_*.R and sourced here in order
message("\n== 01_main_config.R | configuration ==")
source("scripts/01_main_config.R", encoding = "UTF-8")
# 02_main_network.R: this block is commented out in main.R, so it is not run here
# message("\n== 02_main_network.R | network ==")
# source("scripts/02_main_network.R", encoding = "UTF-8")
message("\n== 03_main_traffic.R | traffic ==")
source("scripts/03_main_traffic.R", encoding = "UTF-8")
message("\n== 04_main_estimation.R | fuel calibration, exhaust, evaporatives ==")
source("scripts/04_main_estimation.R", encoding = "UTF-8")
message("\n== 05_main_wear.R | tyre, brake and road wear ==")
source("scripts/05_main_wear.R", encoding = "UTF-8")
message("\n== 06_main_post.R | post-estimation ==")
source("scripts/06_main_post.R", encoding = "UTF-8")
# 07_main_post_spatial.R: this block is commented out in main.R, so it is not run here
# message("\n== 07_main_post_spatial.R | post-estimation spatial ==")
# source("scripts/07_main_post_spatial.R", encoding = "UTF-8")
message("\n== 08_main_plots.R | plots ==")
source("scripts/08_main_plots.R", encoding = "UTF-8")
# 09_main_mech.R: this block is commented out in main.R, so it is not run here
# message("\n== 09_main_mech.R | chemical mechanisms (spatial) ==")
# source("scripts/09_main_mech.R", encoding = "UTF-8")
