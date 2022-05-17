# check hours
if(length(hours) > nrow(tfs)) {
  switch (language,
          "portuguese" = stop("\nEscolhe um número de horas menor ou igual que numero de horas de tfs\n"),
          "english" = stop("\nChoose a number of hours less than or equal to the number of hours of tfs\n"),
          "spanish" = stop("\nEscoje un numero de horas menor o igual que el numero de horas de tfs\n"))
}

hh <- paste0(ifelse(nchar(hours) < 2, paste0("0", hours), hours), "-00")

# streets
switch (language,
        "portuguese" = cat("\nPlotando ruas\n"),
        "english" = cat("\nPlotting streets\n"),
        "spanish" = cat("\nPlotando calles\n"))
for(i in seq_along(pol)) {
  for(j in seq_along(hours)) {
    x <- readRDS(paste0("post/", month, "/streets/", pol[i], ".rds"))
    cn <- names(x)[hours + 1]
    
    png(filename =  paste0("images/STREETS_", month, "_", pol[i],"_", hh[j], ".png"),
        width = 2100, height = 1500, units = "px", pointsize = 12,
        bg = "white",  res = 300)
    par(bg = 'white')
    plot(x[as.numeric(x[[cn[j] ]]) > 0, ][cn[j]], 
         axes = TRUE,
         bg = bg,
         main = paste0(pol[i], ": ", tit, " [g/h] ", hh[j], " LT"),
         pal = cptcity::cpt(colorRampPalette = TRUE, rev = TRUE, pal = pal), lwd = 2)
    dev.off()
    
  }
}

# grade
switch (language,
        "portuguese" = cat("\nPlotando grades\n"),
        "english" = cat("\nPlotting grids\n"),
        "spanish" = cat("\nPlotando grillas\n"))

for(i in seq_along(pol)) {
  for(j in seq_along(hours)) {
    x <- readRDS(paste0("post/", month, "/grids/", pol[i], ".rds"))
    cn <- names(x)[hours + 1]
    
    png(filename =  paste0("images/GRIDS_", month, "_", pol[i],"_", hh[j], ".png"),
        width = 2100, height = 1500, units = "px", pointsize = 12,
        bg = "white",  res = 300)
    par(bg = 'white')
    plot(x[as.numeric(x[[cn[j] ]]) > 0, ][cn[j]], 
         axes = TRUE,
         bg = bg,
         lty = 0.3,
         main = paste0(pol[i], ": ", tit, " [g/km^2/h] ", hh[j], " LT"),
         pal = cptcity::cpt(colorRampPalette = TRUE, rev = TRUE, pal = pal))
    dev.off()
    
  }
}


# categoria
dt <- readRDS(paste0("post/", month, "/datatable/emissions.rds"))
dt0 <- dt[, round(sum(t)*factor_emi, 2), by = .(pollutant, type_emi)]


dt$veh <- as.character(dt$veh)
uv <- unique(dt$veh)
n_PC <- uv[grep(pattern = "PC", x = uv)]
n_LCV <- uv[grep(pattern = "LCV", x = uv)]
n_TRUCKS <- uv[grep(pattern = "TRUCKS", x = uv)]
n_BUS <- uv[grep(pattern = "BUS", x = uv)]
n_MC <- uv[grep(pattern = "MC", x = uv)]
dt$vehicles <-  ifelse(
  dt$veh %in% n_PC, "PC",
  ifelse(
    dt$veh %in% n_LCV, "LCV",
    ifelse(
      dt$veh %in% n_TRUCKS, "TRUCKS",
      ifelse(
        dt$veh %in% n_BUS, "BUS",
        ifelse(
          dt$veh %in%  n_MC,  "MC", "Fleet")))))

# totais x veicles
switch (language,
        "portuguese" = cat("\nPlotando categorias por total\n"),
        "english" = cat("\nPlotting categories by total\n"),
        "spanish" = cat("\nPlotando categorias por total\n"))

dt1 <- dt[pollutant %in% pol, 
          as.numeric(sum(t))*factor_emi, 
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
  
  png(filename =  paste0("images/TOTAL_", month, "_",pol[i], ".png"),
      width = 2100, height = 1500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
}

# totais x veicles
switch (language,
        "portuguese" = cat("\nPlotando categorias por type_emi\n"),
        "english" = cat("\nPlotting categories by type_emi\n"),
        "spanish" = cat("\nPlotando categorias por type_emi\n"))

dt1 <- dt[pollutant %in% pol, 
          as.numeric(sum(t))*factor_emi, 
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
  
  png(filename =  paste0("images/TOTAL_",month, "_", pole[i], ".png"),
      width = 2100, height = 1500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
}

# totais x veh
switch (language,
        "portuguese" = cat("\nPlotando categorias por total e tipo\n"),
        "english" = cat("\nPlotting categories by total and type\n"),
        "spanish" = cat("\nPlotando categorias por total y tipo\n"))

dt1 <- dt[pollutant %in% pol, 
          as.numeric(sum(t))*factor_emi, 
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
  
  png(filename =  paste0("images/TOTAL_TYPE_", month, "_", pol[i], ".png"),
      width = 2100, height = 1500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
}


# totais x hora
switch (language,
        "portuguese" = cat("\nPlotando categorias por hora\n"),
        "english" = cat("\nPlotting categories by hour\n"),
        "spanish" = cat("\nPlotando categorias por hora\n"))

dt1 <- dt[pollutant %in% pol, 
          as.numeric(sum(t))*factor_emi, 
          by = .(pollutant, vehicles, hour)]

for(i in seq_along(pol)){
  p <- ggplot(dt1[pollutant == pol[i]], 
              aes(x = hour, y = V1, fill = vehicles)) + 
    geom_bar(stat = "identity", col = "black") + 
    labs(y = "t/ano", 
         title =  paste0(pol[i], ": ", tit)) +
    theme_bw()
  
  png(filename =  paste0("images/HOUR_", month, "_", pol[i], ".png"),
      width = 2100, height = 1500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
}

switch (language,
        "portuguese" = cat("\nPlotando categorias por hora e type_emi\n"),
        "english" = cat("\nPlotting categories by hour and type_emi\n"),
        "spanish" = cat("\nPlotando categorias por hora y type_emi\n"))

dt1 <- dt[pollutant %in% pol, 
          as.numeric(sum(t))*factor_emi, 
          by = .(pollutant, vehicles, hour, type_emi)]
dt1$pol_te <- as.character(paste0(dt1$pollutant, "_", dt1$type_emi))
pole <- as.character(unique(dt1$pol_te))

for(i in seq_along(pole)){
  p <- ggplot(dt1[pol_te == pole[i]], 
              aes(x = hour, y = V1, fill = vehicles)) + 
    geom_bar(stat = "identity", col = "black") + 
    labs(y = "t/ano", 
         title =  paste0(pol[i], ": ", tit)) +
    theme_bw()
  
  png(filename =  paste0("images/HOUR_", month, "_", pole[i], ".png"),
      width = 2100, height = 1500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
}

# totais x age
dt1 <- dt[pollutant %in% pol, 
          as.numeric(sum(t))*factor_emi, 
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
  
  png(filename =  paste0("images/AGE_", month, "_", pol[i], ".png"),
      width = 2100, height = 1500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
}

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
