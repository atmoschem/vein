year         <- as.numeric(substr(x = getwd(), start = nchar(getwd()) - 6, stop = nchar(getwd()) - 3))
UF_select    <- basename(getwd())
period       <- ifelse(year < 1990, "past", "present")
net          <- sf::st_read(paste0("../../../network/", period, "/", UF_select, "_roads.gpkg"))


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

message(paste0("Files in ", getwd(), "/network\n"))
cat("Cleaning... \n")

