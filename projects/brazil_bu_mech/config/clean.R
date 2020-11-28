# apagando dados
a <- list.files(path = "config", pattern = ".rds", full.names = T)
file.remove(a)

unlink("csv", recursive = T)
unlink("emi", recursive = T)
unlink("images", recursive = T)
unlink("notes", recursive = T)
unlink("post", recursive = T)
unlink("wrf/wrfc*")
unlink("veh", recursive = T)

system("tar -caf brazil_bu_mech.tar.gz .")
system("mv brazil_bu_mech.tar.gz ../")
file.remove(".Rhistory")
