a <- list.files(path = "config", pattern = ".rds", full.names = T)
file.remove(a)

unlink("estimation", recursive = T)


system(paste0("tar -caf ", basename(getwd()), ".tar.gz ."))
system(paste0("mv ", basename(getwd()), ".tar.gz ../"))
# file.remove(".Rhistory")