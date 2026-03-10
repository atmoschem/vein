system(paste0("tar -caf ", basename(getwd()), ".tar.gz ."))
system(paste0("mv ", basename(getwd()), ".tar.gz ../"))
file.remove(".Rhistory")
