

tit <-  provincia

# category ####
dt <- rbind(fread("emi/exhaust.csv"),
            fread("emi/evaporatives.csv"),
            fread("emi/wear.csv"))

dt$pollutant <- fifelse(dt$pollutant == "PM", "PM2.5", dt$pollutant)

dt$g <- units::set_units(dt$emissions, g)
dt$t <- units::set_units(dt$g, t)
dt0 <- dt[, round(sum(t, na.rm = T), 2),
          by = .(pollutant)
]


dt$veh <- as.character(dt$veh)
uv <- unique(dt$veh)

n_PC <- uv[grep(pattern = "PC", x = uv)]
n_TAXI <- uv[grep(pattern = "TAXI", x = uv)]
n_LCV <- uv[grep(pattern = "LCV", x = uv)]
n_TRUCKS <- uv[grep(pattern = "TRUCKS", x = uv)]
n_BUS <- uv[grep(pattern = "BUS", x = uv)]
n_MC <- uv[grep(pattern = "MC", x = uv)]

unique(dt$vehicles)

dt$vehicles <- fifelse(
  dt$veh %in% n_PC, "PC",
  fifelse(
    dt$veh %in% n_LCV, "LCV",
    fifelse(
      dt$veh %in% n_TRUCKS, "TRUCKS",
      fifelse(
        dt$veh %in% n_BUS, "BUS",
        fifelse(
          dt$veh %in% n_MC, "MC", 
          fifelse(
            dt$veh %in% n_TAXI, "TAXI", 
            "Fleet"
        ))
      )
    )
  )
)

# totais x veicles
switch(language,
       "portuguese" = cat("\nPlotando categorias por total\n"),
       "english" = cat("\nPlotting categories by total\n"),
       "spanish" = cat("\nPlotando categorias por total\n")
)
dt1 <- dt[,
          as.numeric(sum(t)),
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
      y = "t/year",
      title = paste0(pol[i], ": ", tit)
    ) +
    scale_fill_gradientn(pol[i], colours = cpt()) +
    scale_x_discrete(limits = rev(metadata$vehicles)) +
    theme_bw() +
    coord_flip() +
    theme(legend.title = element_blank())
  
  png(
    filename = paste0("images/TOTAL_", pol[i], ".png"),
    width = 2000, height = 2500, units = "px", pointsize = 12,
    bg = "white", res = 300
  )
  print(p)
  dev.off()
}

# totais x veicles
switch(language,
       "portuguese" = cat("\nPlotando categorias por type_emi\n"),
       "english" = cat("\nPlotting categories by type_emi\n"),
       "spanish" = cat("\nPlotando categorias por type_emi\n")
)
dt1 <- dt[,
          as.numeric(sum(t)),
          by = .(pollutant, veh, type_emi)
]
dt1$veh <- factor(
  x = dt1$veh,
  levels = metadata$vehicles
)

pol <- as.character(unique(dt1$pollutant))

for (i in seq_along(pol)) {
  p <- ggplot(
    dt1[pollutant == pol[i]],
    aes(x = veh, y = V1, fill = type_emi)
  ) +
    geom_bar(stat = "identity", col = "black") +
    labs(
      y = "t/year",
      title = paste0(pol[i], ": ", tit)
    ) +
    # scale_fill_gradientn(pole[i], colours = cpt()) +
    scale_x_discrete(limits = rev(metadata$vehicles)) +
    theme_bw() +
    coord_flip()    +
    theme(legend.title = element_blank())
  
  png(
    filename = paste0("images/TOTAL_TYPE_", pol[i], ".png"),
    width = 2100, height = 2500, units = "px", pointsize = 12,
    bg = "white", res = 300
  )
  print(p)
  dev.off()
}


# totais x mes
switch(language,
       "portuguese" = cat("\nPlotando categorias por mes\n"),
       "english" = cat("\nPlotting categories by month\n"),
       "spanish" = cat("\nPlotando categorias por mes\n")
)

dt1 <- dt[,
          as.numeric(sum(t, na.rm = T)),
          by = .(pollutant, month)
]
pol <- as.character(unique(dt1$pollutant))

p <- ggplot(
  dt1,
  aes(x = month, 
      y = V1,
      fill = pollutant)
) +
  geom_bar(stat = "identity", 
           col = "black") +
  labs(
    y = "t/month",
    title = paste0(tit)
  ) +
  facet_wrap(~pollutant,  scales = "free_y")+
  # scale_fill_brewer(type = "qual")+
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  theme_bw() +
  theme(legend.title = element_blank())


png(
  filename = paste0("images/TOTAL_MONTH.png"),
  width = 2500, height = 2000, units = "px", pointsize = 12,
  bg = "white", res = 300
)
print(p)
dev.off()




# totais x age
dt1 <- dt[,
          as.numeric(sum(t)),
          by = .(pollutant, vehicles, age)
]

pol <- as.character(unique(dt1$pollutant))

for (i in seq_along(pol)) {
  p <- ggplot(
    dt1[pollutant == pol[i]],
    aes(x = age, y = V1, fill = vehicles)
  ) +
    geom_bar(stat = "identity", col = "black") +
    labs(
      y = "t/year",
      title = ifelse(pol[i] == "PM",
                     paste0(pol[i], ": ", tit, " sem ressuspenssao "),
                     paste0(pol[i], ": ", tit)
      )
    ) +
    theme_bw() +
    theme(legend.title = element_blank())
  
  
  png(
    filename = paste0("images/AGE_", pol[i], ".png"),
    width = 2100, height = 2500, units = "px", pointsize = 12,
    bg = "white", res = 300
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