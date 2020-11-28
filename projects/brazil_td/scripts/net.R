# Edit
cat("\nNames: ", names(net), "\n")
net <- st_transform(net, crs)
saveRDS(net, "network/net.rds")

# plotting geometry
png("images/NET.png", 2000, 1500, "px", res = 300)
plot(st_geometry(net),
  axes = T
)
dev.off()

message(paste0("Files in ", getwd(), "/netWORK\n"))
cat("Cleaning... \n")

suppressWarnings(
  rm(i, region, net, veiculos, crs, osm, st)
)
invisible(gc())