library(sf)
# Edita para tuas necessidades
cat("\nNames: ", names(net), "\n")
net <- st_transform(net, crs)
# A composição veicular da CETESB tem 28 tipos de veiculos
# Em teoría, poderiamos ter 28 fluxos ou mais na rede net
for(i in seq_along(veiculos)) {
  png(filename =  paste0("images/NET_", veiculos[i],".png"),
      width = 2300, height = 1500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  plot(net[veiculos[i]], 
       axes = T, 
       pal = cpt(colorRampPalette = T, rev = T), 
       main = paste0(tit, ": ", veiculos[i]))
  dev.off()
}

net$lkm <- net$lkm*units::as_units("km")

saveRDS(net, 'network/net.rds')

message(paste0("Arquivos em ", getwd(), "/net\n"))
message(paste0("Figuras em ", getwd(), "/images\n"))
cat("Limpando... \n")

suppressWarnings(
  rm(i, tit, net, veiculos, crs)
  
)
gc()
