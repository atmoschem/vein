options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# if domain 1 is available
# mechs <- c("CBMZ", "S99", "RADM2", "CB05")
# mechs <- c("CBMZ")
# for (z in seq_along(mechs)) {
#     print(mechs[z])
#     mech <- mechs[z]
#     language <- "english" # english spanish
#     wrf_times <- 673 # nrow(tfs)
#     dir_wrfinput <- "../wrfinputs/"
#     dir_wrfchemi <- "wrf"
#     domain <- 1
#     hours <- 0
#     dir_mech <- paste0("post/", mechs[z])
#     lf <- list.files(path = dir_mech, pattern = ".rds", full.names = TRUE)
#     na <- list.files(path = dir_mech, pattern = ".rds", full.names = F)
#     emis_option <- na <- gsub(".rds", "", na)
#
#
#     if (mech %in% c("MOZT1", "CBMZ")) {
#         emis_option <- ifelse(emis_option == "E_ETOH", "E_C2H5OH", emis_option)
#         na <- ifelse(na == "E_ETOH", "E_C2H5OH", na)
#     }
#
#     wrfc <- wrf_create(
#         wrfinput_dir = dir_wrfinput,
#         wrfchemi_dir = paste0("wrf/", mechs[z]),
#         io_style_emissions = io_style_emissions,
#         domains = domain,
#         frames_per_auxinput5 = wrf_times,
#         auxinput5_interval_m = 60,
#         variables = emis_option,
#         separator = "_",
#         verbose = TRUE,
#         return_fn = TRUE
#     )
# }
