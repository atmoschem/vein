library(sf)

switch(language,
        "portuguese" = cat("\nNomes:\n ", names(net), "\n"),
        "english" = cat("\nNames:\n ", names(net), "\n"),
        "spanish" = cat("\nNombres:\n ", names(net), "\n")
)

# A composição veicular da CETESB tem 28 tipos de categories
# Em teoría, poderiamos ter 28 fluxos ou mais na rede net

# The number of traffic flow vehicles can be the same from vehicular composition
#

ls <- as.data.frame(do.call("cbind", lapply(1:nrow(tfs), function(i) {
  net$speed
} )))
names(ls) <-  paste0("S", 1:nrow(tfs))

df <- netspeed(ps = net$speed, ffs = net$speed*1.5, scheme = TRUE)

png(filename = paste0("images/NET_SPEED.png"),
    width = 2300, 
    height = 3000, 
    units = "px", 
    pointsize = 12,
    bg = "white", 
    res = 300
  )
  
  plot(df)
  
  dev.off()

saveRDS(df, "network/speed.rds")

switch(language,
        "portuguese" = message("\nArquivos em: /net:"),
        "english" = message("\nFiles in: /net"),
        "spanish" = message("\nArchivos en: /net")
)

switch(language,
        "portuguese" = message("Figuras em: /images:"),
        "english" = message("Figures in: /images"),
        "spanish" = message("Figuras en: /images")
)

switch(language,
        "portuguese" = message("Limpando..."),
        "english" = message("Cleaning..."),
        "spanish" = message("Limpiando...")
)

suppressWarnings(
        rm(i, tit, net, categories, crs)
)
invisible(gc())