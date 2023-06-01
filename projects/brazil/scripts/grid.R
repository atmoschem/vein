
library(geobr)
library(vein)
library(sf)
#br <- read_country()
br <- readRDS("rds/ufs.rds")
g <- make_grid(spobj = br, width = 0.03, crs = 3857)
br <- st_transform(br, 3857)
saveRDS(g, "rds/grid_003.rds", compress = "xz")

for(i in seq_along(br$abbrev_state)) {
  print(br$abbrev_state[i])
   gx <- st_crop(g, br[i, ])
   saveRDS(gx, paste0("rds/g_", br$abbrev_state[i], ".rds"))

}

