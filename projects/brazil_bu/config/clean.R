# apagando dados
a <- list.files(path = "config", pattern = ".rds", full.names = T)
file.remove(a)

unlink("csv", recursive = T)
unlink("emi", recursive = T)
unlink("images", recursive = T)
unlink("veh", recursive = T)
unlink("post", recursive = T)
unlink("wrf/wrfchemi_00z_d02")
unlink("wrf/wrfchemi_12z_d02")
unlink(paste0("wrf/", wrfc))

system("tar -caf brazil_bu.tar.gz .")
system("cp brazil_bu.tar.gz brazil_bu_v2020-02-26.tar.gz")
system("mv brazil_bu.tar.gz ../")
system("mv brazil_bu_v2020-02-26.tar.gz ../")
file.remove(".Rhistory")
