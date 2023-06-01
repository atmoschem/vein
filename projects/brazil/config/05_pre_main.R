#library(sf)
#a <- geobr::read_state()
#a <- st_transform(a, 3857)
#saveRDS(a, "rds/ufs.rds")
a  <- readRDS("rds/ufs.rds")

ufs <- a$abbrev_state
uf <- "config/UF.tar.gz"
#dir.create("estimation")
unlink("estimation", recursive = T)
dir.create("estimation")


lapply(1:2, function(i) {
  lapply(seq_along(1), function(j) {
    print(paste0("estimation/", 1970:2100, "/", ufs[j])[i])

    untar(tarfile = uf, 
          exdir = paste0("estimation/", 1970:2100, "/", 
                         ufs[j])[i])
    file.remove(paste0("estimation/", 1970:2100, "/", ufs[j], "/UF.tar.gz")[i])
  })
})
