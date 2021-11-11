

# n_veh

n_MC <- metadata$vehicles[grep(pattern = "MC", x = metadata$vehicles)]
n_PC <- metadata$vehicles[grep(pattern = "PC", x = metadata$vehicles)]
n_PT <- metadata$vehicles[grep(pattern = "PT", x = metadata$vehicles)]
n_LCT <- metadata$vehicles[grep(pattern = "LCT", x = metadata$vehicles)]
n_BUS_INTERCITY <- metadata$vehicles[grep(pattern = "BUS_INTERCITY", x = metadata$vehicles)]
n_BUS_TRANSIT <- metadata$vehicles[grep(pattern = "BUS_TRANSIT", x = metadata$vehicles)]
n_BUS_SCHOOL <- metadata$vehicles[grep(pattern = "BUS_SCHOOL", x = metadata$vehicles)]
n_TRUCKS_REFUSE <- metadata$vehicles[grep(pattern = "TRUCKS_REFUSE", x = metadata$vehicles)]
n_TRUCKS_SU_SH <- metadata$vehicles[grep(pattern = "TRUCKS_SU_SH", x = metadata$vehicles)]
n_TRUCKS_SU_LH <- metadata$vehicles[grep(pattern = "TRUCKS_SU_LH", x = metadata$vehicles)]
n_TRUCKS_MH <- metadata$vehicles[grep(pattern = "TRUCKS_MH", x = metadata$vehicles)]
n_TRUCKS_CU_SH <- metadata$vehicles[grep(pattern = "TRUCKS_CU_SH", x = metadata$vehicles)]
n_TRUCKS_CU_LH <- metadata$vehicles[grep(pattern = "TRUCKS_CU_LH", x = metadata$vehicles)]

n_veh <- list(
  MC = n_MC,
  PC = n_PC,
  PT = n_PT,
  LCT = n_LCT,
  BUS_INTERCITY = n_BUS_INTERCITY,
  BUS_TRANSIT= n_BUS_TRANSIT,
  BUS_SCHOOL = n_BUS_SCHOOL,
  TRUCKS_REFUSE = n_TRUCKS_REFUSE,
  TRUCKS_SU_SH = n_TRUCKS_SU_SH,
  TRUCKS_SU_LH = n_TRUCKS_SU_LH,
  TRUCKS_MH = n_TRUCKS_MH,
  TRUCKS_CU_SH = n_TRUCKS_CU_SH,
  TRUCKS_CU_LH = n_TRUCKS_CU_LH
)

for(i in seq_along(metadata$vehicles)) {
  if(identical(names(veh)[i + 1],metadata$vehicles[i]) == FALSE) {
    
    switch (language,
            "portuguese" = cat( "Nomes incompativeis:\n",
                                "metadata:",metadata$vehicles[i], "\n",
                                "`veh`:",names(veh)[i + 1], "\n"),
            "english" = cat( "Incomptaible names:\n",
                             "metadata:",metadata$vehicles[i], "\n",
                             "`veh`:",names(veh)[i + 1], "\n"),
            "spanish" = cat( "Nombres incomptatibles:\n",
                             "metadata:",metadata$vehicles[i], "\n",
                             "`veh`:",names(veh)[i + 1], "\n"))
    
    
    stop()
  }
}


# apagando arquivos
switch (language,
        "portuguese" = message("Apagando veh/*.rds\n"),
        "english" = message("Deleting veh/*.rds\n"),
        "spanish" = message("Borrando veh/*.rds\n"))

arquivos <- list.files(path = "veh", pattern = ".rds", full.names = TRUE)
file.remove(arquivos)

# fleet age
veh[is.na(veh)] <- 0

veh <- remove_units(veh)

# plotting
switch (language,
        "portuguese" = cat("Plotando fluxos\n"),
        "english" = cat("Plotting traffic flows\n"),
        "spanish" = cat("Plotando flujos\n"))

# list of vehicles to colplot
lveh <- list()


# MC ####
fl <- function(x) {
  lapply(seq_along(x), function(i) {
    if(length(x) == 1) {
      1
    } else {
      df <- veh[, x]
      sum(df[i]) / sum(df, na.rm = T)
    }
  })-> lx
  names(lx) <- x
  lx
}
fl(n_MC) -> l_mc

lapply(seq_along(l_mc), function(i) {
  cat(names(l_mc)[i], " is ", n_MC[[i]], " of MC\n")
}) -> lx
cat("sum: ", sum(unlist(l_mc)), "\n\n")

# fuel
metadata[metadata$vein_name == "MC", ]$fuel
kf <- c(k_G)   #gasoline

for (i in seq_along(n_MC)) {
  x <- my_age(
    x = net[[categories[1]]],
    y = veh[[n_MC[i]]],
    name = n_MC[i],
    k = l_mc[[i]] * kf[i],
    verbose = verbose
  )
  saveRDS(x, paste0("veh/", n_MC[i], ".rds"))
}
lMC <- data.frame(MC_G = colSums(x))

# PC ####
fl(n_PC) -> l_pc

lapply(seq_along(l_pc), function(i) {
  cat(names(l_pc)[i], " is ", l_pc[[i]], " of PC\n")
}) -> lx
cat("sum: ", sum(unlist(l_pc)), "\n\n")


# fuel
metadata[metadata$vein_name == "PC", ]$fuel
kf <- c(k_G,   #diesel
        1,     #eletric
        k_E85, #E85
        k_D)   #gasoline

# colplot
lPC <- list()

for (i in seq_along(n_PC)) {
  x <- my_age(
    x = net[[categories[2]]],
    y = veh[[n_PC[i]]],
    name = n_PC[i],
    k = l_pc[[i]] * kf[i],
    verbose = verbose
  )
  saveRDS(x, paste0("veh/", n_PC[i], ".rds"))
  lPC[[i]] <- colSums(x)
}

# PT ####
fl(n_PT) -> l_pt

lapply(seq_along(l_pt), function(i) {
  cat(names(l_pt)[i], " is ", l_pt[[i]], " of PT\n")
}) -> lx
cat("sum: ", sum(unlist(l_pt)), "\n\n")

# fuel
metadata[metadata$family == "PT", ]$fuel
kf <- c(k_G,   #diesel
        1,     #eletric
        k_E85, #E85
        k_D)   #gasoline

# colplot
lPT <- list()

for (i in seq_along(n_PT)) {
  x <- my_age(
    x = net[[categories[3]]],
    y = veh[[n_PT[i]]],
    name = n_PT[i],
    k = l_pt[[i]] * kf[i],
    verbose = verbose
  )
  saveRDS(x, paste0("veh/", n_PT[i], ".rds"))
  lPT[[i]] <- colSums(x)
}

# LCT ####
fl(n_LCT) -> l_lct

lapply(seq_along(l_lct), function(i) {
  cat(names(l_lct)[i], " is ", l_lct[[i]], " of LCT\n")
}) -> lx
cat("sum: ", sum(unlist(l_lct)), "\n\n")

# fuel
metadata[metadata$vein_name == "LCT", ]$fuel
kf <- c(k_G,   #diesel
        1,     #eletric
        k_E85, #E85
        k_D)   #gasoline

# colplot
lLCT <- list()

for (i in seq_along(n_LCT)) {
  x <- my_age(
    x = net[[categories[4]]],
    y = veh[[n_LCT[i]]],
    name = n_LCT[i],
    k = l_lct[[i]] * kf[i],
    verbose = verbose
  )
  saveRDS(x, paste0("veh/", n_LCT[i], ".rds"))
  lLCT[[i]] <- colSums(x)
}

# BUS_INTERCITY ####
fl(n_BUS_INTERCITY) -> l_bus_intercity

lapply(seq_along(l_bus_intercity), function(i) {
  cat(names(l_bus_intercity)[i], " is ", l_bus_intercity[[i]], " of BUS_INTERCITY\n")
}) -> lx
cat("sum: ", sum(unlist(l_bus_intercity)), "\n\n")

# fuel
metadata[metadata$vein_name == "BUS_INTERCITY", ]$fuel
kf <- c(k_G,
        k_D,
        k_CNG)

# colplot
lBI <- list()

for (i in seq_along(n_BUS_INTERCITY)) {
  x <- my_age(
    x = net[[categories[5]]],
    y = veh[[n_BUS_INTERCITY[i]]],
    name = n_BUS_INTERCITY[i],
    k = l_bus_intercity[[i]] * kf[i],
    verbose = verbose
  )
  saveRDS(x, paste0("veh/", n_BUS_INTERCITY[i], ".rds"))
  lBI[[i]] <- colSums(x)
}


# BUS_TRANSIT ####
fl(n_BUS_TRANSIT) -> l_bus_transit

lapply(seq_along(l_bus_transit), function(i) {
  cat(names(l_bus_transit)[i], " is ", l_bus_transit[[i]], " of BUS_TRANSIT\n")
}) -> lx
cat("sum: ", sum(unlist(l_bus_transit)), "\n\n")

# fuel
metadata[metadata$vein_name == "BUS_TRANSIT", ]$fuel
kf <- c(k_G,
        k_D,
        k_CNG)

# colplot
lBT <- list()

for (i in seq_along(n_BUS_TRANSIT)) {
  x <- my_age(
    x = net[[categories[6]]],
    y = veh[[n_BUS_TRANSIT[i]]],
    name = n_BUS_TRANSIT[i],
    k = l_bus_transit[[i]] * kf[i],
    verbose = verbose
  )
  saveRDS(x, paste0("veh/", n_BUS_TRANSIT[i], ".rds"))
  lBT[[i]] <- colSums(x)
}


# BUS_SCHOOL ####
fl(n_BUS_SCHOOL) -> l_bus_school

lapply(seq_along(l_bus_school), function(i) {
  cat(names(l_bus_school)[i], " is ", l_bus_school[[i]], " of BUS_SCHOOL\n")
}) -> lx
cat("sum: ", sum(unlist(l_bus_school)), "\n\n")

# fuel
metadata[metadata$vein_name == "BUS_SCHOOL", ]$fuel
kf <- c(k_G,
        k_D,
        k_CNG)

# colplot
lBS <- list()

for (i in seq_along(n_BUS_SCHOOL)) {
  x <- my_age(
    x = net[[categories[7]]],
    y = veh[[n_BUS_SCHOOL[i]]],
    name = n_BUS_SCHOOL[i],
    k = l_bus_school[[i]] * kf[i],
    verbose = verbose
  )
  saveRDS(x, paste0("veh/", n_BUS_SCHOOL[i], ".rds"))
  lBS[[i]] <- colSums(x)
}


# TRUCKS_REFUSE ####
fl(n_TRUCKS_REFUSE) -> l_trucks_refuse

lapply(seq_along(l_trucks_refuse), function(i) {
  cat(names(l_trucks_refuse)[i], " is ", l_trucks_refuse[[i]], " of TRUCKS_REFUSE\n")
}) -> lx
cat("sum: ", sum(unlist(l_trucks_refuse)), "\n\n")

# fuel
metadata[metadata$vein_name == "TRUCKS_REFUSE", ]$fuel
kf <- c(k_G,
        k_D,
        k_CNG)

# colplot
lTR <- list()

for (i in seq_along(n_TRUCKS_REFUSE)) {
  x <- my_age(
    x = net[[categories[8]]],
    y = veh[[n_TRUCKS_REFUSE[i]]],
    name = n_TRUCKS_REFUSE[i],
    k = l_trucks_refuse[[i]] * kf[i],
    verbose = verbose
  )
  saveRDS(x, paste0("veh/", n_TRUCKS_REFUSE[i], ".rds"))
  lTR[[i]] <- colSums(x)
}


# TRUCKS_SU_SH ####
fl(n_TRUCKS_SU_SH) -> l_trucks_su_sh

lapply(seq_along(l_trucks_su_sh), function(i) {
  cat(names(l_trucks_su_sh)[i], " is ", l_trucks_su_sh[[i]], " of TRUCKS_SU_SH\n")
}) -> lx
cat("sum: ", sum(unlist(l_trucks_su_sh)), "\n\n")

# fuel
metadata[metadata$vein_name == "TRUCKS_SU_SH", ]$fuel
kf <- c(k_G,
        k_D,
        k_CNG)

# colplot
lTSS <- list()

for (i in seq_along(n_TRUCKS_SU_SH)) {
  x <- my_age(
    x = net[[categories[9]]],
    y = veh[[n_TRUCKS_SU_SH[i]]],
    name = n_TRUCKS_SU_SH[i],
    k = l_trucks_su_sh[[i]] * kf[i],
    verbose = verbose
  )
  saveRDS(x, paste0("veh/", n_TRUCKS_SU_SH[i], ".rds"))
  lTSS[[i]] <- colSums(x)
}

# TRUCKS_SU_LH ####
fl(n_TRUCKS_SU_LH) -> l_trucks_su_lh

lapply(seq_along(l_trucks_su_lh), function(i) {
  cat(names(l_trucks_su_lh)[i], " is ", l_trucks_su_lh[[i]], " of TRUCKS_SU_LH\n")
}) -> lx
cat("sum: ", sum(unlist(l_trucks_su_lh)), "\n\n")

# fuel
metadata[metadata$vein_name == "TRUCKS_SU_LH", ]$fuel
kf <- c(k_G,
        k_D,
        k_CNG)

# colplot
lTSL <- list()

for (i in seq_along(n_TRUCKS_SU_LH)) {
  x <- my_age(
    x = net[[categories[10]]],
    y = veh[[n_TRUCKS_SU_LH[i]]],
    name = n_TRUCKS_SU_LH[i],
    k = l_trucks_su_lh[[i]] * kf[i],
    verbose = verbose
  )
  saveRDS(x, paste0("veh/", n_TRUCKS_SU_LH[i], ".rds"))
  lTSL[[i]] <- colSums(x)
}


# TRUCKS_MH ####
fl(n_TRUCKS_MH) -> l_trucks_mh

lapply(seq_along(l_trucks_mh), function(i) {
  cat(names(l_trucks_mh)[i], " is ", l_trucks_mh[[i]], " of TRUCKS_MH\n")
}) -> lx
cat("sum: ", sum(unlist(l_trucks_mh)), "\n\n")

# fuel
metadata[metadata$vein_name == "TRUCKS_MH", ]$fuel
kf <- c(k_G,
        k_D,
        k_CNG)

# colplot
lMH <- list()

for (i in seq_along(n_TRUCKS_MH)) {
  x <- my_age(
    x = net[[categories[11]]],
    y = veh[[n_TRUCKS_MH[i]]],
    name = n_TRUCKS_MH[i],
    k = l_trucks_mh[[i]] * kf[i],
    verbose = verbose
  )
  saveRDS(x, paste0("veh/", n_TRUCKS_MH[i], ".rds"))
  lMH[[i]] <- colSums(x)
}


# TRUCKS_CU_SH ####
fl(n_TRUCKS_CU_SH) -> l_trucks_cu_sh

lapply(seq_along(l_trucks_cu_sh), function(i) {
  cat(names(l_trucks_cu_sh)[i], " is ", l_trucks_cu_sh[[i]], " of TRUCKS_CU_SH\n")
}) -> lx
cat("sum: ", sum(unlist(l_trucks_cu_sh)), "\n\n")

# fuel
metadata[metadata$vein_name == "TRUCKS_CU_SH", ]$fuel
kf <- c(k_G,
        k_D,
        k_CNG)

# colplot
lTCS <- list()

for (i in seq_along(n_TRUCKS_CU_SH)) {
  x <- my_age(
    x = net[[categories[12]]],
    y = veh[[n_TRUCKS_CU_SH[i]]],
    name = n_TRUCKS_CU_SH[i],
    k = l_trucks_cu_sh[[i]] * kf[i],
    verbose = verbose
  )
  saveRDS(x, paste0("veh/", n_TRUCKS_CU_SH[i], ".rds"))
  lTCS[[i]] <- colSums(x)
}


# TRUCKS_CU_LH ####
fl(n_TRUCKS_CU_LH) -> l_trucks_cu_lh

lapply(seq_along(l_trucks_cu_lh), function(i) {
  cat(names(l_trucks_cu_lh)[i], " is ", l_trucks_cu_lh[[i]], " of TRUCKS_CU_LH\n")
}) -> lx
cat("sum: ", sum(unlist(l_trucks_cu_lh)), "\n\n")

# fuel
metadata[metadata$vein_name == "TRUCKS_CU_LH", ]$fuel
kf <- c(k_D)

# colplot
lTCL <- list()

for (i in seq_along(n_TRUCKS_CU_LH)) {
  x <- my_age(
    x = net[[categories[13]]],
    y = veh[[n_TRUCKS_CU_LH[i]]],
    name = n_TRUCKS_CU_LH[i],
    k = l_trucks_cu_lh[[i]] * kf[i],
    verbose = verbose
  )
  saveRDS(x, paste0("veh/", n_TRUCKS_CU_LH[i], ".rds"))
  lTCL[[i]] <- colSums(x)
}


# colplot ####

DF <- cbind(lMC,
            do.call("cbind", lPC),
            do.call("cbind", lPT),
            do.call("cbind", lLCT),
            do.call("cbind", lBI),
            do.call("cbind", lBT),
            do.call("cbind", lBS),
            do.call("cbind", lTR),
            do.call("cbind", lTSS),
            do.call("cbind", lTSL),
            do.call("cbind", lMH),
            do.call("cbind", lTCS),
            do.call("cbind", lTCL))

names(DF) <- metadata$vehicles

for(i in seq_along(n_veh)) {
  png(paste0("images/FLOW_", n_veh[[i]], ".png"), 2000, 1500, "px", res = 300)
  
  colplot(
    df = DF, 
    cols = n_veh[[i]],
    xlab = "Age",
    ylab = "veh/h",
    main = paste0("FLOW ", n_veh[[i]]),
    type = "l",
    pch = NULL,
    theme = theme,
    spl = 9
  )
  dev.off()
}



# plots ####
switch(language,
       "portuguese" = cat("Plotando composição veicular\n"),
       "english" = cat("Plotting vehicular composition\n"),
       "spanish" = cat("Plotando composición vehicular\n")
)


f <- list.files(path = "veh", pattern = ".rds", full.names = T)
na <- list.files(path = "veh", pattern = ".rds")
na <- gsub(pattern = ".rds", replacement = "", x = na)
ff <- unlist(lapply(seq_along(f), function(i) {
  sum(readRDS(f[i]))
}))
df <- data.frame(
  per = ff,
  Categoria = na
)
df <- df[df$Categoria != "fleet_age", ]
df$per <- df$per / sum(df$per) * 100
df$Categoria <- factor(df$Categoria, levels = metadata$vehicles)
p <- ggplot(df, aes(x = Categoria, y = per, fill = per)) +
  geom_bar(stat = "identity", col = "black") +
  labs(y = "[%]", title = "Vehicular composition") +
  ggplot2::scale_fill_gradientn("[%]", colours = cptcity::cpt(rev = T)) +
  theme_bw() +
  coord_flip() +
  scale_x_discrete(limits = rev(metadata$vehicles)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

png(
  filename = paste0("images/TRAFFIC.png"),
  width = 2500, height = 2500, units = "px", pointsize = 12,
  bg = "white", res = 300
)
print(p)
dev.off()

# speed ####
if(speed) {
  

  rbindlist(lapply(seq_along(metadata$vehicles), function(i) {
    temp_fact(q = rowSums(readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))), 
              pro = tfs[[metadata$vehicles[i]]]) -> tf
    tf$id <- 1:nrow(tf)
    tf
  })) -> lx
  
  lx[, 
     lapply(.SD, sum, na.rm = T),
     .SDcols = paste0("V", 1:length(tfs[[1]])),
     by = id] -> ft
  
  ft$id <- NULL
  
  switch(language,
         "portuguese" = cat("Projetando velocidade\n"),
         "english" = cat("Projecting speed\n"),
         "spanish" = cat("Proyectando velocidad\n")
  )
  
  df <- netspeed(
    q = ft, 
    ps = net$CONGSPD, 
    ffs = net$FFSPEED, 
    cap = net$CAPE, 
    lkm = net$lkm, 
    alpha = 1, 
    dist = "miles"
  )
  saveRDS(df, "network/speed.rds")
  
  dfs <- netspeed(
    q = ft, 
    ps = net[[ps]], 
    ffs = net[[ffs]], 
    cap = net[[capacity]], 
    lkm = net$lkm, 
    alpha = 1, 
    dist = "miles",
    net = net
  )
  
  dfs <- st_transform(dfs, 4326)
  saveRDS(dfs, "network/speed_net.rds")
  
  
  sp_mov <- moves_speed(df)
  saveRDS(sp_mov, "network/speed_bin.rds")
  

  # plots ####
  switch(language,
         "portuguese" = cat("Plotando velocidade\n"),
         "english" = cat("Plotting speed\n"),
         "spanish" = cat("Plotando velocidad\n")
  )
  
  png(
    filename = paste0("images/SPEED.png"),
    width = 2500, height = 3500, units = "px", pointsize = 12,
    bg = "white", res = 300
  )
  plot(df, mai2 =c(2.2, 0.82, 0.82, 0.42))
  dev.off()
  
  png(
    filename = paste0("images/SPEED_HOUR.png"),
    width = 2500, height = 3000, units = "px", pointsize = 12,
    bg = "white", res = 300
  )
  plot(dfs, 
       max.plot = 25,
       axes = T,
       pal = cpt(colorRampPalette = T, rev = T),
       key.pos = 4)
  
  dev.off()
  
  
  png(
    filename = paste0("images/SPEEDBIN.png"),
    width = 2500, height = 3500, units = "px", pointsize = 12,
    bg = "white", res = 300
  )
  plot(Speed(remove_units(sp_mov), dist = units::unitless), pal = "mpl_viridis")
  dev.off()
  
  
}
  
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
  
  
  