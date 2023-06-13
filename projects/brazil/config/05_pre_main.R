#library(sf)
#a <- geobr::read_state()
#a <- st_transform(a, 3857)
#saveRDS(a, "rds/ufs.rds")
a  <- readRDS("rds/ufs.rds")

ufs <- a$abbrev_state |> as.character()
uf <- "config/UFBR.tar.gz"
#dir.create("estimation")
unlink("estimation", recursive = T)
dir.create("estimation")


years <- 1960:2100

lapply(seq_along(years), function(i) {
  lapply(seq_along(ufs), function(j) {
    print(paste0("estimation/", years, "/", ufs[j])[i])

    untar(tarfile = uf, 
          exdir = paste0("estimation/", years, "/", 
                         ufs[j])[i])
    file.remove(paste0("estimation/", years, "/", ufs[j], "/UFBR.tar.gz")[i])
  })
})
