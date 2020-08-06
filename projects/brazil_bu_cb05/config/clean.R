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

system("tar -caf brazil_bu_cb05.tar.gz .")
# system("cp brazil_bu.tar.gz brazil_bu_v2020-02-26.tar.gz")
system("mv brazil_bu_cb05.tar.gz ../")
# system("mv brazil_bu_v2020-02-26.tar.gz ../")
file.remove(".Rhistory")
