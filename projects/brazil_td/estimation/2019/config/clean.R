# apagando dados
a <- list.files(path = "config", pattern = ".rds", full.names = T)
file.remove(a)

unlink("csv", recursive = T)
unlink("emi", recursive = T)
unlink("images", recursive = T)
unlink("veh", recursive = T)
unlink("post", recursive = T)
unlink("wrf", recursive = T)
# unlink("wrf/wrfchemi_12z_d02")
# unlink(paste0("wrf/", wrfc))

system(paste0("tar -caf ", basename(getwd()), ".tar.gz ."))
system(paste0("mv ", basename(getwd()), ".tar.gz ../"))
file.remove(".Rhistory")