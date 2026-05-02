# Edit
cat("\nNames: ", names(net), "\n")
net <- st_transform(net, crs)

# plotting geometry
png("images/NET.png", 2000, 1500, "px", res = 300)
plot(st_geometry(net),
     axes = T
)
dev.off()

categories

for (i in seq_along(categories)) {
  
  if(categories[i] %in% c("ffs", "ps")) {
    net[[categories[i]]] <- Speed(net[[categories[i]]])
  } else {
    net[[categories[i]]] <- Vehicles(net[[categories[i]]], time = "1/h")
  }

  png(
    filename = paste0("images/NET_", categories[i], ".png"),
    width = 2300, height = 1500, units = "px", pointsize = 12,
    bg = "white", res = 300
  )
  plot(net[categories[i]],
       axes = T,
       pal = cpt(colorRampPalette = T, rev = T)
  )
  dev.off()
}

net$lkm <- units::set_units(st_length(net), km)


saveRDS(net, "network/net.rds")

message(paste0("Files in ", getwd(), "/network\n"))
cat("Cleaning... \n")

suppressWarnings(
  rm(i, region, net, veiculos, crs, osm, st)
)
invisible(gc())
