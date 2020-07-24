# checar nomes
for(i in seq_along(metadata$vehicles)) {
  if(identical(names(veh)[i + 1],metadata$vehicles[i]) == FALSE) {
    
    switch (language,
            "portuguese" = cat( "Nomes incompativeis:\n",
                                "metadata:",metadata$vehicles[i], "\n",
                                "`veh`:",names(veh)[i + 1], "\n"),
            "english" = cat( "Incomptaible names:\n",
                             "metadata:",metadata$vehicles[i], "\n",
                             "`veh`:",names(veh)[i + 1], "\n"),
            "chinese" = cat( "不兼容的名称:\n",
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
        "chinese" = message("删除中 veh/*.rds\n"),
        "spanish" = message("Borrando veh/*.rds\n"))

arquivos <- list.files(path = "veh", pattern = ".rds", full.names = TRUE)
file.remove(arquivos)

# fleet age
veh[is.na(veh)] <- 0

# plotting
switch (language,
        "portuguese" = cat("Plotando fluxos\n"),
        "english" = cat("Plotting traffic flows\n"),
        "chinese" = cat("绘制交通流\n"),
        "spanish" = cat("Plotando flujos\n"))

# identicar nomes de grupos
nveh <- names(veh)
n_PC <- nveh[grep(pattern = "PC", x = nveh)]
n_LCV <- nveh[grep(pattern = "LCV", x = nveh)]
n_TRUCKS <- nveh[grep(pattern = "TRUCKS", x = nveh)]
n_BUS <- nveh[grep(pattern = "BUS", x = nveh)]
n_MC <- nveh[grep(pattern = "MC", x = nveh)]

# PC ####
#
# calcular proporção dentro de PC 
#

kPC_G <- sum(veh$PC_G)/sum(veh[, n_PC])
kPC_E <- sum(veh$PC_E)/sum(veh[, n_PC])
kPC_FG <- sum(veh$PC_FG)/sum(veh[, n_PC])
kPC_FE <- sum(veh$PC_FE)/sum(veh[, n_PC])
kPC <- c(kPC_G, kPC_E, kPC_FG, kPC_FE)     # ordem de fleet_age
l_PC <- list()

cat(paste0(
  "PC_G ", round(kPC_G, 3), " \n",
  "PC_E  ", round(kPC_E, 3), " \n",
  "PC_FG ", round(kPC_FG, 3), " \n",
  "PC_FE ", round(kPC_FE, 3), " \n",
  "∑: ", kPC_G + kPC_E + kPC_FG +  kPC_FE, "\n\n"))

# combustivel
kf <- c(k_G, k_E, k_G, k_E)
  
for(i in seq_along(n_PC)) {
  x <- my_age(x = net[[categories[1]]], 
              y = veh[[n_PC[i]]], 
              name = n_PC[i],
              k = kPC[i]*kf[i], 
              verbose = verbose)
   saveRDS(x, paste0("veh/", n_PC[i], ".rds"))
  l_PC[[i]] <- colSums(x)
}
df <- as.data.frame(do.call("cbind", l_PC))
names(df) <- n_PC

png("images/FLOW_PC.png", 2000, 1500, "px",res = 300)
colplot(df = df,
        xlab = "Age",
        ylab = "veh/h",
        main = "FLOW PC",
        type = "l",
        pch = NULL,
        theme = theme)
dev.off()

# LCV ####
#
# calcular proporção dentro de LCV
#

kLCV_G <- sum(veh$LCV_G)/sum(veh[, n_LCV])
kLCV_E <- sum(veh$LCV_E)/sum(veh[, n_LCV])
kLCV_FG <- sum(veh$LCV_FG)/sum(veh[, n_LCV])
kLCV_FE <- sum(veh$LCV_FE)/sum(veh[, n_LCV])
kLCV_D <- sum(veh$LCV_D)/sum(veh[, n_LCV])
kLCV <- c(kLCV_G, kLCV_E, kLCV_FG, kLCV_FE, kLCV_D) # ordem de fleet_age
l_LCV <- list()

cat(paste0(
  "LCV_G ", round(kLCV_G, 3), " \n",
  "LCV_E ", round(kLCV_E, 3), " \n",
  "LCV_FG ", round(kLCV_FG, 3), " \n",
  "LCV_FE ", round(kLCV_FE, 3), " \n",
  "LCV_D ", round(kLCV_D, 3), " \n",
  "∑: ", kLCV_G + kLCV_FG +  kLCV_FE +  kLCV_E + kLCV_D, "\n\n"))

# combustivel
kf <- c(k_G, k_E, k_G, k_E, k_D)

for(i in seq_along(n_LCV)) {
  x <- my_age(x = net[[categories[2]]], 
              y = veh[[n_LCV[i]]], 
              name = n_LCV[i],
              k = kLCV[i]*kf[i], 
              verbose = verbose)
  saveRDS(x, paste0("veh/", n_LCV[i], ".rds"))
  l_LCV[[i]] <- colSums(x)
}
df <- as.data.frame(do.call("cbind", l_LCV))
names(df) <- n_LCV

png("images/FLOW_LCV.png", 2000, 1500, "px",res = 300)
colplot(df = df,
        xlab = "Age",
        ylab = "veh/h",
        main = "FLOW LCV",
        type = "l",
        pch = NULL,
        theme = theme)
dev.off()

# TRUCKS ####
#
# calcular proporção dentro de TRUCKS
#

kTRUCKS_SL_D <- sum(veh$TRUCKS_SL_D)/sum(veh[, n_TRUCKS])
kTRUCKS_L_D <- sum(veh$TRUCKS_L_D)/sum(veh[, n_TRUCKS])
kTRUCKS_M_D <- sum(veh$TRUCKS_M_D)/sum(veh[, n_TRUCKS])
kTRUCKS_SH_D <- sum(veh$TRUCKS_SH_D)/sum(veh[, n_TRUCKS])
kTRUCKS_H_D <- sum(veh$TRUCKS_H_D)/sum(veh[, n_TRUCKS])
kTRUCKS <- c(kTRUCKS_SL_D, kTRUCKS_L_D, kTRUCKS_M_D, kTRUCKS_SH_D, kTRUCKS_H_D)
l_TRUCKS <- list()

cat(paste0(
  "TRUCKS_G ", round(kTRUCKS_SL_D, 3), " \n",
  "TRUCKS_FG ", round(kTRUCKS_L_D, 3), " \n",
  "TRUCKS_FE ", round(kTRUCKS_M_D, 3), " \n",
  "TRUCKS_E ", round(kTRUCKS_SH_D, 3), " \n",
  "TRUCKS_D ", round(kTRUCKS_H_D, 3), " \n\n",
  "∑: ", kTRUCKS_SL_D + kTRUCKS_L_D +  kTRUCKS_M_D +  kTRUCKS_SH_D + kTRUCKS_H_D, "\n"))

# combustivel
kf <- rep(k_D, 5)

for(i in seq_along(n_TRUCKS)) {
  x <- my_age(x = net[[categories[3]]], 
              y = veh[[n_TRUCKS[i]]], 
              name = n_TRUCKS[i],
              k = kTRUCKS[i]*kf[i], 
              verbose = verbose)
  saveRDS(x, paste0("veh/", n_TRUCKS[i], ".rds"))
  l_TRUCKS[[i]] <- colSums(x)
}
df <- as.data.frame(do.call("cbind", l_TRUCKS))
names(df) <- n_TRUCKS

png("images/FLOW_TRUCKS.png", 2000, 1500, "px",res = 300)
colplot(df = df,
        xlab = "Age",
        ylab = "veh/h",
        main = "FLOW TRUCKS",
        type = "l",
        pch = NULL,
        theme = theme,
        spl = 9)
dev.off()


# BUS ####
#
# calcular proporção dentro de BUS
#

kBUS_URBAN_D <- sum(veh$BUS_URBAN_D)/sum(veh[, n_BUS])
kBUS_MICRO_D <- sum(veh$BUS_MICRO_D)/sum(veh[, n_BUS])
kBUS_COACH_D <- sum(veh$BUS_COACH_D)/sum(veh[, n_BUS])
kBUS <- c(kBUS_URBAN_D, kBUS_MICRO_D, kBUS_COACH_D) # ordem de fleet_age
l_BUS <- list()

cat(paste0(
  "BUS_URBAN_D ", round(kBUS_URBAN_D, 3), " \n",
  "BUS_MICRO_D ", round(kBUS_MICRO_D, 3), " \n",
  "BUS_URBAN_D ", round(kBUS_COACH_D, 3), " \n",
  "∑: ", kBUS_URBAN_D + kBUS_MICRO_D +  kBUS_COACH_D, "\n\n"))

# combustivel
kf <- rep(k_D, 3)

for(i in seq_along(n_BUS)) {
  x <- my_age(x = net[[categories[4]]], 
              y = veh[[n_BUS[i]]], 
              name = n_BUS[i],
              k = kBUS[i]*kf[i], 
              verbose = verbose)
  saveRDS(x, paste0("veh/", n_BUS[i], ".rds"))
  l_BUS[[i]] <- colSums(x)
}
df <- as.data.frame(do.call("cbind", l_BUS))
names(df) <- n_BUS

png("images/FLOW_BUS.png", 2000, 1500, "px",res = 300)
colplot(df = df,
        xlab = "Age",
        ylab = "veh/h",
        main = "FLOW BUS",
        type = "l",
        pch = NULL,
        theme = theme,
        spl = 9)
dev.off()


# MC ####
#
# calcular proporção dentro de BUS
#

kMC_150_G <- sum(veh$MC_150_G)/sum(veh[, n_MC])
kMC_150_500_G <- sum(veh$MC_150_500_G)/sum(veh[, n_MC])
kMC_500_G <- sum(veh$MC_500_G)/sum(veh[, n_MC])
kMC_150_FG <- sum(veh$MC_150_FG)/sum(veh[, n_MC])
kMC_150_500_FG <- sum(veh$MC_150_500_FG)/sum(veh[, n_MC])
kMC_500_FG <- sum(veh$MC_500_FG)/sum(veh[, n_MC])
kMC_150_FE <- sum(veh$MC_150_FE)/sum(veh[, n_MC])
kMC_150_500_FE <- sum(veh$MC_150_500_FE)/sum(veh[, n_MC])
kMC_500_FE <- sum(veh$MC_500_FE)/sum(veh[, n_MC])
kMC <- c(kMC_150_G, kMC_150_500_G, kMC_500_G,
         kMC_150_FG, kMC_150_500_FG, kMC_500_FG,
         kMC_150_FE, kMC_150_500_FE, kMC_500_FE) # ordem de fleet_age
l_MC <- list()

cat(paste0(
  "MC_150_G ", round(kMC_150_G, 3), " \n",
  "MC_150_500_G ", round(kMC_150_500_G, 3), " \n",
  "MC_500_G ", round(kMC_500_G, 3), " \n",
  "MC_150_FG  ", round(kMC_150_FG, 3), " \n",
  "MC_150_500_FG  ", round(kMC_150_500_FG, 3), " \n",
  "MC_500_FG  ", round(kMC_500_FG, 3), " \n",
  "MC_150_FE  ", round(kMC_150_FE, 3), " \n",
  "MC_150_500_FE  ", round(kMC_150_500_FE, 3), " MC\n",
  "MC_500_FE  ", round(kMC_500_FE, 3), " MC\n",
  "∑: ", 
  kMC_150_G + kMC_150_500_G +  kMC_500_G +
    kMC_150_FG + kMC_150_500_FG + kMC_500_FG +
    kMC_150_FE + kMC_150_500_FE + kMC_500_FE, "\n\n"))

# combustivel
kf <- c(rep(k_G, 6), rep(k_E, 3))

for(i in seq_along(n_MC)) {
  x <- my_age(x = net[[categories[5]]], 
              y = veh[[n_MC[i]]], 
              name = n_MC[i],
              k = kMC[i]*kf[i], 
              verbose = verbose)
  saveRDS(x, paste0("veh/", n_MC[i], ".rds"))
  l_MC[[i]] <- colSums(x)
}
df <- as.data.frame(do.call("cbind", l_MC))
names(df) <- n_MC

png("images/FLOW_MC.png", 2000, 1500, "px",res = 300)
colplot(df = df,
        xlab = "Age",
        ylab = "veh/h",
        main = "FLOW BUS",
        type = "l",
        pch = NULL,
        theme = theme,
        spl = 9)
dev.off()

# plots ####
switch (language,
        "portuguese" = cat("Plotando composição veicular\n"),
        "english" = cat("Plotting vehicular composition\n"),
        "chinese" = cat("绘制车辆组成\n"),
        "spanish" = cat("Plotando composición vehicular\n"))


f <- list.files(path = "veh", pattern = ".rds", full.names = T)
na <- list.files(path = "veh", pattern = ".rds")
na <- gsub(pattern = ".rds", replacement = "", x = na)
ff <- unlist(lapply(seq_along(f), function(i){
  sum(readRDS(f[i]))
}))
df <- data.frame(per = ff,
                 Categoria = na)
df <- df[df$Categoria != "fleet_age", ]
df$per <- df$per/sum(df$per)*100
df$Categoria <- factor(df$Categoria, levels = metadata$vehicles)
p <- ggplot(df, aes(x = Categoria, y = per, fill = per)) +
  geom_bar(stat = "identity", col = "black")+
  labs(y = "[%]", title = "Vehicular composition") +
  ggplot2::scale_fill_gradientn("[%]",colours = cptcity::cpt(rev = T)) + 
  theme_bw() +
  coord_flip() +
  scale_x_discrete(limits = rev(metadata$vehicles)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

png(filename =  paste0("images/TRAFFIC.png"),
    width = 2500, height = 2500, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(p)
dev.off()

switch (language,
        "portuguese" = message("\nArquivos em: /veh:"),
        "english" = message("\nFiles in: /veh"),
        "chinese" = message("\n文件位于: /veh"),
        "spanish" = message("\nArchivos en: /veh"))

switch (language,
        "portuguese" = message("Figuras em: /images:"),
        "english" = message("Figures in: /images"),
        "chinese" = message("中的数字: /images"),
        "spanish" = message("Figuras en: /images"))

switch (language,
        "portuguese" = message("Limpando..."),
        "english" = message("Cleaning..."),
        "chinese" = message("清洁用品..."),
        "spanish" = message("Limpiando..."))


suppressWarnings(
  rm(kPC, kPC_G, kPC_E, kPC_FG, kPC_FE,
   kLCV, kLCV_G, kLCV_E, kLCV_FG, kLCV_FE, kLCV_D,
   kTRUCKS, kTRUCKS_SL_D, kTRUCKS_L_D, kTRUCKS_M_D, kTRUCKS_SH_D, kTRUCKS_H_D,
   kBUS, kBUS_URBAN_D, kBUS_MICRO_D, kBUS_COACH_D,
   kMC,kMC_150_G, kMC_150_500_G, kMC_500_G,
            kMC_150_FG, kMC_150_500_FG, kMC_500_FG,
            kMC_150_FE, kMC_150_500_FE, kMC_500_FE,
   l_PC, l_LCV, l_TRUCKS, l_BUS, l_MC,
   i, arquivos, cores, df, f, ff, 
   n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC,
   na, nveh, p, tit, tit2, categories, verbose, x, kf,
   k_G, k_D, k_E,
   metadata, net, veh, year,
   theme)
)   
gc()
