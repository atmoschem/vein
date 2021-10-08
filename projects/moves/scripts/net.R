switch(language,
        "portuguese" = cat("\nNomes: ", names(net), "\n"),
        "english" = cat("\nNames: ", names(net), "\n"),
        "spanish" = cat("\nNombres: ", names(net), "\n")
)


net <- st_transform(net, crs)
intersect(categories, names(net))
net$sum_categories <- rowSums(st_set_geometry(net, NULL)[, categories], na.rm = T)

switch(language,
       "portuguese" = cat("Filtrando ruas sem fluxo: \n"),
       "english" = cat("Filtering empty roads\n"),
       "spanish" = cat("Filtrando calles sin volumenes\n")
)

net <- net[net$sum_categories > 0, ]
#
for (i in seq_along(categories)) {
        png(
                filename = paste0("images/NET_", categories[i], ".png"),
                width = 2300, height = 1500, units = "px", pointsize = 12,
                bg = "white", res = 300
        )
        plot(net[categories[i]],
                axes = T,
                pal = cpt(colorRampPalette = T, rev = T),
                main = paste0(tit, ": ", categories[i])
        )
        dev.off()
}
net$lkm <- sf::st_length(net)

net$lkm <- units::set_units(x = net$lkm, "miles")

saveRDS(net, "network/net.rds")

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
