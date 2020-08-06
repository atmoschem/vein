list.of.packages <- c("ggplot2", "readxl", "remotes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

remotes::install_github("atmoschem/vein")
remotes::install_github("atmoschem/eixport")
rm(list.of.packages, new.packages)