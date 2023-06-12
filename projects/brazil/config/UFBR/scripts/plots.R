year <- as.numeric(substr(x = getwd(), start = nchar(getwd()) - 6, stop = nchar(getwd()) - 3))

tit <- paste0("Emissões veiculares em ", basename(getwd()), year)

switch (language,
        "portuguese" = cat("\nPlotando ruas\n"),
        "english" = cat("\nPlotting streets\n"),
        "spanish" = cat("\nPlotando calles\n"))

# category ####
dt <- readRDS("post/emi_table.rds")
dt0 <- dt[, round(sum(t), 2), 
          by = .(pollutant, type_emi)]


dt$veh <- as.character(dt$veh)
uv <- unique(dt$veh)
n_PC <- uv[grep(pattern = "PC", x = uv)]
n_LCV <- uv[grep(pattern = "LCV", x = uv)]
n_TRUCKS <- uv[grep(pattern = "TRUCKS", x = uv)]
n_BUS <- uv[grep(pattern = "BUS", x = uv)]
n_MC <- uv[grep(pattern = "MC", x = uv)]
dt$vehicles <-  fifelse(
  dt$veh %in% n_PC, "PC",
  fifelse(
    dt$veh %in% n_LCV, "LCV",
    fifelse(
      dt$veh %in% n_TRUCKS, "TRUCKS",
      fifelse(
        dt$veh %in% n_BUS, "BUS",
        fifelse(
          dt$veh %in%  n_MC,  "MC", "Fleet")))))

# totais x veicles ####
switch (language,
        "portuguese" = cat("\nPlotando categorias por total\n"),
        "english" = cat("\nPlotting categories by total\n"),
        "spanish" = cat("\nPlotando categorias por total\n"))
dt1 <- dt[pollutant %in% pol, 
          as.numeric(sum(t)), 
          by = .(pollutant, veh)]
dt1$veh <- factor(x = dt1$veh, 
                  levels = metadata$vehicles)

for(i in seq_along(pol)){
  p <- ggplot(dt1[pollutant == pol[i]], 
              aes(x = veh, y = V1, fill = V1)) + 
    geom_bar(stat = "identity", col = "black") + 
    labs(y = "t/ano", 
         title =  paste0(pol[i], ": ", tit)) +
    scale_fill_gradientn(pol[i], colours = cpt()) +
    scale_x_discrete(limits = rev(metadata$vehicles)) +
    theme_bw() +
    coord_flip()
  
  png(filename =  paste0("images/TOTAL_", pol[i], ".png"),
      width = 2100, height = 1500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
}

# totais x veicles2 ####
switch (language,
        "portuguese" = cat("\nPlotando categorias por type_emi\n"),
        "english" = cat("\nPlotting categories by type_emi\n"),
        "spanish" = cat("\nPlotando categorias por type_emi\n"))
dt1 <- dt[pollutant %in% pol, 
          as.numeric(sum(t)), 
          by = .(pollutant, veh, type_emi)]
dt1$veh <- factor(x = dt1$veh, 
                  levels = metadata$vehicles)
dt1$pol_te <- as.character(paste0(dt1$pollutant, "_", dt1$type_emi))
pole <- as.character(unique(dt1$pol_te))

for(i in seq_along(pole)){
  p <- ggplot(dt1[pol_te == pole[i]], 
              aes(x = veh, y = V1, fill = V1)) + 
    geom_bar(stat = "identity", col = "black") + 
    labs(y = "t/ano", 
         title =  paste0(pole[i], ": ", tit)) +
    scale_fill_gradientn(pole[i], colours = cpt()) +
    scale_x_discrete(limits = rev(metadata$vehicles)) +
    theme_bw() +
    coord_flip()
  
  png(filename =  paste0("images/TOTAL_",pole[i], ".png"),
      width = 2100, height = 1500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
}

# totais x veh ####
switch (language,
        "portuguese" = cat("\nPlotando categorias por total e tipo\n"),
        "english" = cat("\nPlotting categories by total and type\n"),
        "spanish" = cat("\nPlotando categorias por total y tipo\n"))

dt1 <- dt[pollutant %in% pol, 
          as.numeric(sum(t)), 
          by = .(pollutant, veh, type_emi)]

for(i in seq_along(pol)){
  p <- ggplot(dt1[pollutant == pol[i]], 
              aes(x = veh, y = V1, fill = type_emi)) + 
    geom_bar(stat = "identity", col = "black") + 
    labs(y = "t/ano", 
         title =  paste0(pol[i], ": ", tit)) +
    scale_x_discrete(limits = rev(metadata$vehicles)) +
    coord_flip()+
    theme_bw()
  
  png(filename =  paste0("images/TOTAL_TYPE_", pol[i], ".png"),
      width = 2100, height = 1500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
}


# totais x mes ####
switch (language,
        "portuguese" = cat("\nPlotando categorias por mes\n"),
        "english" = cat("\nPlotting categories by month\n"),
        "spanish" = cat("\nPlotando categorias por mes\n"))

dt1 <- dt[pollutant %in% pol, 
          as.numeric(sum(t)), 
          by = .(pollutant, vehicles, month)]

for(i in seq_along(pol)){
  p <- ggplot(dt1[pollutant == pol[i]], 
              aes(x = month, y = V1, fill = vehicles)) + 
    geom_bar(stat = "identity", col = "black") + 
    labs(y = "t/ano", 
         title =  paste0(pol[i], ": ", tit)) +
    theme_bw()
  
  png(filename =  paste0("images/MONTH_", pol[i], ".png"),
      width = 2100, height = 1500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
}

# totais x age ####
dt1 <- dt[pollutant %in% pol, 
          as.numeric(sum(t)), 
          by = .(pollutant, vehicles, age)]

for(i in seq_along(pol)){
  p <- ggplot(dt1[pollutant == pol[i]], 
              aes(x = age, y = V1, fill = vehicles)) + 
    geom_bar(stat = "identity", col = "black") + 
    labs(y = "t/ano", 
         title =  ifelse(pol[i] == "PM", 
                         paste0(pol[i], ": ", tit, " sem ressuspenssao "),
                         paste0(pol[i], ": ",tit))) +
    theme_bw()
  
  png(filename =  paste0("images/AGE_", pol[i], ".png"),
      width = 2100, height = 1500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
}

# totais x age ####
dt1 <- dt[pollutant %in% pol, 
          as.numeric(sum(t)), 
          by = .(pollutant, vehicles, month)]

for(i in seq_along(pol)){
  p <- ggplot(dt1[pollutant == pol[i]], 
              aes(x = month, y = V1, fill = vehicles)) + 
    geom_bar(stat = "identity", col = "black") + 
    labs(y = "t", title = paste(pol[i], tit)) +
    theme_bw()
  
  png(filename =  paste0("images/MONTH_", pol[i], ".png"),
      width = 2100, height = 1500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
}



# grid ####
switch (language,
        "portuguese" = cat("\nPlotando grade\n"),
        "english" = cat("\nPlotting grid\n"),
        "spanish" = cat("\nPlotando grilla\n"))

dt <- readRDS("post/emi_grid.rds")
names(dt)

png(filename =  paste0("images/GRID_", names(dt)[2], ".png"),
    width = 2100, height = 1500, units = "px", pointsize = 12,
    bg = "white",  res = 300)
plot(dt[names(dt)[2]], lty = 0, pal = cpt(colorRampPalette = T, rev = T), breaks = "sd")
dev.off()


switch (language,
        "portuguese" = message("\nFiguras em /images\n"),
        "english" = message("\nFigures in /image\n"),
        "spanish" = message("\nFiguras en /images\n"))


switch (language,
        "portuguese" = message("Limpando..."),
        "english" = message("Cleaning..."),
        "spanish" = message("Limpiando..."))


suppressWarnings(
  rm("bg", "breaks", "cn", "dt", "dt0", "dt1", "ef_cetesb2", "factor_emi", 
     "hh", "hours", "i", "j", "metadata", "n_BUS", "n_LCV", "n_MC", "n_PC", 
     "n_TRUCKS", "num_vein", "p", "pal", "pol", "pole", "tfs", "tit", "uv",
     "veh", "vein_version", "x", "year")
)


ls()
invisible(gc())
