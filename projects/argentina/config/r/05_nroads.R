library(data.table)
library(sf)
file.remove("config/csv/nroads.csv")

l <- list.files("/media/sergio/ext4/veinrun/brazil_roads/" ,
                pattern = "gpkg",
                full.names = T)

n <- list.files("/media/sergio/ext4/veinrun/brazil_roads/" ,
                pattern = "gpkg",
                full.names = F)

n <- gsub("_roads.gpkg", "", n)

for(i in seq_along(n)) {

  x <- as.data.table(st_read(l[i]))

  dn <- x[, .N, by = highway]

  dn$region <- n[i]

  fwrite(dn, "config/csv/nroads.csv", append = T)

}


br <- fread("config/csv/nroads.csv")

dbr <- br[, sum(N), by = highway]

dbr[, perc := dbr$V1/sum(dbr$V1)]

fwrite(dbr, "config/csv/nroads_country.csv")

