# apagando dados
a <- list.files(path = "config", pattern = ".rds", full.names = T)
file.remove(a)

unlink("csv", recursive = T)
unlink("emi", recursive = T)
unlink("images", recursive = T)
unlink("veh", recursive = T)
unlink("post", recursive = T)
# unlink("wrf/wrfchemi_00z_d02")
# unlink("wrf/wrfchemi_12z_d02")
# unlink(paste0("wrf/", wrfc))

system("tar -caf brazil_td.tar.gz .")
system("mv brazil_td.tar.gz ../")
file.remove(".Rhistory")
