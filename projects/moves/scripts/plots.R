met <- readRDS("config/met.rds")
names(met)
year
year <- met$Year[1]
month <- met$Month[1]
date <- as.Date(ISOdate(year, month, 1, 0, 0))

numberOfDays <- function(date) {
  m <- format(date, format = "%m")
  
  while (format(date, format = "%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format = "%d")))
}

factor_emi <- numberOfDays(date = date) # daily to month

# 
# check hours
if (length(hours) > nrow(tfs)) {
  switch(language,
    "portuguese" = stop("\nEscolhe um número de horas menor ou igual que numero de horas de tfs\n"),
    "english" = stop("\nChoose a number of hours less than or equal to the number of hours of tfs\n"),
    "spanish" = stop("\nEscoje un numero de horas menor o igual que el numero de horas de tfs\n")
  )
}

hh <- paste0(ifelse(nchar(hours) < 2, paste0("0", hours), hours), "00")

# streets ####
switch(language,
  "portuguese" = cat("\nPlotando ruas\n"),
  "english" = cat("\nPlotting streets\n"),
  "spanish" = cat("\nPlotando calles\n")
)

pol <- list.files(path = "post/streets/", full.names = T)

npol <- list.files(path = "post/streets/", full.names = F)

npol <- gsub(pattern = ".rds", replacement = "", x = npol)

for (i in seq_along(pol)) {
  
  for (j in seq_along(hours)) {
    
    x <- readRDS(pol[i])
    
    cn <- names(x)[hours + 1]

    png(
      filename = paste0("images/STREETS_", npol[i], "_", hh[j], ".png"),
      width = 2500, 
      height = 2500, 
      units = "px", 
      pointsize = 12,
      bg = "white", 
      res = 300
    )
    par(bg = "white")
    plot(x[as.numeric(x[[cn[j]]]) > 0, ][cn[j]],
      axes = TRUE,
      bg = bg,
      main = paste0(npol[i], ": ", tit, " [g/h] ", hh[j], " LT"),
      pal = cptcity::cpt(colorRampPalette = TRUE, rev = FALSE, pal = pal), lwd = 2
    )
    dev.off()
  }
}

# grade ####
switch(language,
  "portuguese" = cat("\nPlotando grades\n"),
  "english" = cat("\nPlotting grids\n"),
  "spanish" = cat("\nPlotando grillas\n")
)


pol <- list.files(path = "post/grids/", full.names = T)

npol <- list.files(path = "post/grids/", full.names = F)

npol <- gsub(pattern = ".rds", replacement = "", x = npol)

for (i in seq_along(pol)) {
  
  for (j in seq_along(hours)) {
    
    x <- readRDS(pol[i])
    
    cn <- names(x)[hours + 1]

    png(
      filename = paste0("images/GRIDS_", npol[i], "_", hh[j], ".png"),
      width = 2500,
      height = 2500, 
      units = "px", 
      pointsize = 12,
      bg = "white", 
      res = 300
    )
    par(bg = "white")
    plot(x[as.numeric(x[[cn[j]]]) > 0, ][cn[j]],
      axes = TRUE,
      bg = bg,
      lty = 0.3,
      main = paste0(npol[i], ": ", tit, " [g/km^2/h] ", hh[j], " LT"),
      pal = cptcity::cpt(colorRampPalette = TRUE, rev = TRUE, pal = pal)
    )
    dev.off()
  }
}


# categoria ####
dt <- readRDS("post/datatable/emissions.rds")
dt0 <- dt[, round(sum(t) * factor_emi, 2), by = .(pollutant, process)]

dt$veh <- as.character(dt$veh)

# totais x veicles
switch(language,
  "portuguese" = cat("\nPlotando categorias por total\n"),
  "english" = cat("\nPlotting categories by total\n"),
  "spanish" = cat("\nPlotando categorias por total\n")
)

dt1 <- dt[,
  as.numeric(sum(g)),
  by = .(pollutant, veh)
]

dt1$veh <- factor(
  x = dt1$veh,
  levels = metadata$vehicles
)

pol <- unique(dt1$pollutant)

for (i in seq_along(pol)) {
  
  p <- ggplot(
    dt1[pollutant == pol[i]],
    aes(x = veh, y = V1, fill = V1)
  ) +
    geom_bar(stat = "identity", col = "black") +
    labs(
      y = paste0("g/", nrow(tfs),"h"),
      title = paste0(pol[i], ": ", tit)
    ) +
    scale_fill_gradientn(pol[i], colours = cpt()) +
    scale_x_discrete(limits = rev(metadata$vehicles)) +
    theme_bw() +
    coord_flip()

  png(
    filename = paste0("images/TOTAL_", gsub("\\.","_",gsub(" ", "", pol[i])), ".png"),
    width = 2500, 
    height = 2500, 
    units = "px", 
    pointsize = 12,
    bg = "white", 
    res = 300
  )
  print(p)
  dev.off()
}

# totals by vehicles
switch(language,
  "portuguese" = cat("\nPlotando categorias por type_emi\n"),
  "english" = cat("\nPlotting categories by type_emi\n"),
  "spanish" = cat("\nPlotando categorias por type_emi\n")
)

dt1 <- dt[,
  as.numeric(sum(g)),
  by = .(pollutant, veh, process)
]

dt1$veh <- factor(
  x = dt1$veh,
  levels = metadata$vehicles
)

dt1$pol_te <- as.character(paste0(dt1$pollutant, " - ", dt1$process))

pol <- as.character(unique(dt1$pollutant))

for (i in seq_along(pol)) {
  
  p <- ggplot(
    dt1[pollutant == pol[i]],
    aes(x = veh, 
        y = V1, 
        fill = process)
  ) +
    geom_bar(stat = "identity", col = "black") +
    labs(
      y = paste0("g/", nrow(tfs),"h"),
      title = paste0(pol[i], ": ", tit)
    ) +
    scale_x_discrete(limits = rev(metadata$vehicles)) +
    theme_bw() +
    coord_flip()

  png(
    filename = paste0("images/TOTAL_", gsub("\\.","_",gsub(" ", "", pol[i])), "_FILL.png"),
    width = 2500, 
    height = 2500, 
    units = "px",
    pointsize = 12,
    bg = "white", 
    res = 300
  )
  print(p)
  dev.off()
}

# totals by hour
switch(language,
  "portuguese" = cat("\nPlotando categorias por hora\n"),
  "english" = cat("\nPlotting categories by hour\n"),
  "spanish" = cat("\nPlotando categorias por hora\n")
)

dt1 <- dt[,
  as.numeric(sum(g)),
  by = .(pollutant, family, hour)
]

for (i in seq_along(pol)) {
  
  p <- ggplot(
    dt1[pollutant == pol[i]],
    aes(x = hour, 
        y = V1, 
        fill = family)
  ) +
    geom_bar(stat = "identity", col = "black") +
    labs(
      y = paste0("g/", nrow(tfs),"h"),
      title = paste0(pol[i], ": ", tit)
    ) +
    theme_bw()

  png(
    filename = paste0("images/HOUR_", gsub("\\.","_",gsub(" ", "", pol[i])), "_FILL.png"),
    width = 2500, 
    height = 2500, 
    units = "px", 
    pointsize = 12,
    bg = "white", 
    res = 300
  )
  print(p)
  dev.off()
}


# totais x age
dt1 <- dt[,
  as.numeric(sum(g)),
  by = .(pollutant, family, age)
]

for (i in seq_along(pol)) {
  p <- ggplot(
    dt1[pollutant == pol[i]],
    aes(x = age, 
        y = V1, 
        fill = family)
  ) +
    geom_bar(stat = "identity", col = "black") +
    labs(
      y = paste0("g/", nrow(tfs),"h"),
      title = paste0(pol[i], ": ", tit)
    ) +
    theme_bw()

  png(
    filename = paste0("images/AGE_", gsub("\\.","_",gsub(" ", "", pol[i])), "_FILL.png"),
    width = 2500, 
    height = 2500, 
    units = "px", 
    pointsize = 12,
    bg = "white", 
    res = 300
  )
  print(p)
  dev.off()
}

switch(language,
  "portuguese" = message("\nFiguras em /images\n"),
  "english" = message("\nFigures in /image\n"),
  "spanish" = message("\nFiguras en /images\n")
)


switch(language,
  "portuguese" = message("Limpando..."),
  "english" = message("Cleaning..."),
  "spanish" = message("Limpiando...")
)


suppressWarnings(
  rm(
    "bg", "breaks", "cn", "dt", "dt0", "dt1", "ef_cetesb2", "factor_emi",
    "hh", "hours", "i", "j", "metadata", "n_BUS", "n_LCV", "n_MC", "n_PC",
    "n_TRUCKS", "num_vein", "p", "pal", "pol", "pole", "tfs", "tit", "uv",
    "veh", "vein_version", "x", "year"
  )
)


ls()
invisible(gc())