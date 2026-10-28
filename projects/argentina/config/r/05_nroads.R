library(data.table)
library(sf)
if (file.exists("config/csv/nroads.csv")) file.remove("config/csv/nroads.csv")

# folder with one OSM road file per region, named <region>_roads.gpkg
# (not included in the project)
dir_roads <- "network/roads/"

l <- list.files(dir_roads,
                pattern = "gpkg",
                full.names = T)

n <- list.files(dir_roads,
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

