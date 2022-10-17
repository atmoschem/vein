

# checar nomes
for (i in seq_along(metadata$vehicles)) {
  if (identical(names(veh)[i + 1], metadata$vehicles[i]) == FALSE) {
    switch(language,
           "portuguese" = cat(
             "Nomes incompativeis:\n",
             "metadata:", metadata$vehicles[i], "\n",
             "`veh`:", names(veh)[i + 1], "\n"
           ),
           "english" = cat(
             "Incomptaible names:\n",
             "metadata:", metadata$vehicles[i], "\n",
             "`veh`:", names(veh)[i + 1], "\n"
           ),
           "spanish" = cat(
             "Nombres incomptatibles:\n",
             "metadata:", metadata$vehicles[i], "\n",
             "`veh`:", names(veh)[i + 1], "\n"
           )
    )
    
    stop()
  }
}


# apagando arquivos
switch(language,
       "portuguese" = message("Apagando veh/*.rds\n"),
       "english" = message("Deleting veh/*.rds\n"),
       "spanish" = message("Borrando veh/*.rds\n")
)

arquivos <- list.files(path = "veh", 
                       pattern = ".rds", 
                       full.names = TRUE)
file.remove(arquivos)

# fleet age
veh[is.na(veh)] <- 0



# Vehicle matrices
switch(language,
       "portuguese" = cat("Gerando matrices de veiculos sem sucateamento\n"),
       "english" = cat("Generating vehile matrices without scrapping\n"),
       "spanish" = cat("Generando matrices de vehiculos descontando chatarreamento\n")
)

# constants
kf <- ifelse(metadata$fuel == "G", k_G,
               ifelse(
                 metadata$fuel == "D", k_D,
                 ifelse(
                   metadata$fuel == "CNG", k_CNG,
                   1)))

lx <- list()

for (i in seq_along(metadata$vehicles)) {
  
  xage <- age(x = veh[[metadata$vehicles[i]]], 
              type = metadata$survival[i], 
              b = metadata$survival_param_b[i], 
              a = metadata$survival_param_a[i])
  
  
  x <- my_age(
    x = net[[metadata$vehicles[i]]],
    y = xage, 
    k = kf[i], 
    agemax = length(xage))
  
  saveRDS(x, paste0("veh/", metadata$vehicles[i], ".rds"))
  
  lx[[i]] <- x
}


# plots ####
switch(language,
       "portuguese" = cat("Plotando composição veicular\n"),
       "english" = cat("Plotting vehicular composition\n"),
       "spanish" = cat("Plotando composición vehicular\n")
)


sx <- lapply(lx, colSums, na.rm = T)

df <- data.frame(x = unlist(sx),
                 vehicles = rep(metadata$vehicles, 
                                each = length(lx[[1]])),
                 family = rep(metadata$family, 
                              each = length(lx[[1]])),
                 subfamily = rep(metadata$subfamily, 
                                 each = length(lx[[1]])),
                 fuel = rep(metadata$fuel, 
                            each = length(lx[[1]])))

df$age <- 1:length(lx[[1]])


df$subfamily <- factor(x = df$subfamily, 
                       levels = unique(metadata$subfamily))

ggplot(df, 
       aes(x = age, 
           y = x, 
           fill = fuel)) +
  geom_bar(stat = "identity") +
  facet_wrap(~subfamily,
             scales = "free_y") +
  labs(y= "veh")+
  theme_bw() +
  theme(text = element_text(colour = "black",
                            size = 18)) -> p


png("images/FLEET.png", 3500, 2000, "px", res = 300)
print(p)
dev.off()

data.table::setDT(df)

df[, 
   sum(x, na.rm = TRUE),
   by = .(vehicles)] -> dt

dt$per <- dt$V1 / sum(dt$V1) * 100

dt$vehicles <- factor(dt$vehicles, levels = metadata$vehicles)

p <- ggplot(dt, 
            aes(x = vehicles, 
                y = per, 
                fill = per)) +
  geom_bar(stat = "identity", col = "black") +
  labs(y = "[%]", title = "Vehicular composition") +
  ggplot2::scale_fill_gradientn("[%]", colours = cptcity::cpt(rev = T)) +
  theme_bw() +
  coord_flip() +
  scale_y_sqrt() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p

png(
  filename = paste0("images/FLEET_PERCENTAGE.png"),
  width = 2500, height = 2500, units = "px", pointsize = 12,
  bg = "white", res = 300
)
print(p)
dev.off()

switch(language,
       "portuguese" = message("\nArquivos em: /veh:"),
       "english" = message("\nFiles in: /veh"),
       "spanish" = message("\nArchivos en: /veh")
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


invisible(gc())