# checar nomes
for(i in seq_along(metadata$vehicles)) {
  if(identical(names(veh)[i + 1],metadata$vehicles[i]) == FALSE) {
    cat( "Metadata$Vehicles é: ", metadata$vehicles[i], "mais veh é: ", names(veh)[i + 1], "\n")
    stop()
  }
}


# apagando arquivos
cat("Apagando arquivos '.rds' de `veh`\n")
arquivos <- list.files(path = "veh", pattern = ".rds", full.names = TRUE)
file.remove(arquivos)

# fleet age
veh[is.na(veh)] <- 0

cat("Plotando perfis `veh`\n")
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
  "PC_G  é ", round(kPC_G, 3), " de PC\n",
  "PC_E  é ", round(kPC_E, 3), " de PC\n",
  "PC_FG é ", round(kPC_FG, 3), " de PC\n",
  "PC_FE é ", round(kPC_FE, 3), " de PC\n",
  "sum: ", kPC_G + kPC_E + kPC_FG +  kPC_FE, "\n\n"))

# combustivel
kf <- c(k_G, k_E, k_G, k_E)
  
for(i in seq_along(n_PC)) {
  x <- my_age(x = net[[veiculos[1]]], 
              y = veh[[n_PC[i]]], 
              name = n_PC[i],
              k = kPC[i]*kf[i], 
              verbose = verbose)
   saveRDS(x, paste0("veh/", n_PC[i], ".rds"))
  l_PC[[i]] <- colSums(x)
}

png("images/FLEET_PC.png", 2000, 1500, "px",res = 300)
plot(l_PC[[1]], xlab = "Age", main = tit,
     ylim = c(0, max(unlist(l_PC))), ylab = "[veh]",
     type = "b", pch = 16, col = cores[1]) 
axis(side = 3,at = c(0, ncol(x)), labels = c(year, year - ncol(x) + 1))
for(i in 2:length(n_PC)) {
  points(l_PC[[i]], type = "b", pch = 16, col = cores[i]) 
}
legend(x = "topright", col = cores[1:length(n_PC)], n_PC, pch = 16)
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
  "LCV_G  é ", round(kLCV_G, 3), " de LCV\n",
  "LCV_E  é ", round(kLCV_E, 3), " de LCV\n",
  "LCV_FG é ", round(kLCV_FG, 3), " de LCV\n",
  "LCV_FE é ", round(kLCV_FE, 3), " de LCV\n",
  "LCV_D  é ", round(kLCV_D, 3), " de LCV\n\n",
  "sum: ", kLCV_G + kLCV_FG +  kLCV_FE +  kLCV_E + kLCV_D, "\n"))

# combustivel
kf <- c(k_G, k_E, k_G, k_E, k_D)

for(i in seq_along(n_LCV)) {
  x <- my_age(x = net[[veiculos[2]]], 
              y = veh[[n_LCV[i]]], 
              name = n_LCV[i],
              k = kLCV[i]*kf[i], 
              verbose = verbose)
  saveRDS(x, paste0("veh/", n_LCV[i], ".rds"))
  l_LCV[[i]] <- colSums(x)
}

png("images/FLEET_LCV.png", 2000, 1500, "px",res = 300)
plot(l_LCV[[1]], xlab = "Age", main = tit,
     ylim = c(0, max(unlist(l_LCV))), ylab = "[veh]",
     type = "b", pch = 16, col = cores[1]) 
axis(side = 3,at = c(0, ncol(x)), labels = c(year, year - ncol(x) + 1))
for(i in 2:length(n_LCV)) {
  points(l_LCV[[i]], type = "b", pch = 16, col = cores[i]) 
}
legend(x = "topright", col = cores[1:length(n_LCV)], n_LCV, pch = 16)
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
  "TRUCKS_G  é ", round(kTRUCKS_SL_D, 3), " de TRUCKS\n",
  "TRUCKS_FG é ", round(kTRUCKS_L_D, 3), " de TRUCKS\n",
  "TRUCKS_FE é ", round(kTRUCKS_M_D, 3), " de TRUCKS\n",
  "TRUCKS_E  é ", round(kTRUCKS_SH_D, 3), " de TRUCKS\n",
  "TRUCKS_D  é ", round(kTRUCKS_H_D, 3), " de TRUCKS\n\n",
  "sum: ", kTRUCKS_SL_D + kTRUCKS_L_D +  kTRUCKS_M_D +  kTRUCKS_SH_D + kTRUCKS_H_D, "\n"))

# combustivel
kf <- rep(k_D, 5)

for(i in seq_along(n_TRUCKS)) {
  x <- my_age(x = net[[veiculos[3]]], 
              y = veh[[n_TRUCKS[i]]], 
              name = n_TRUCKS[i],
              k = kTRUCKS[i]*kf[i], 
              verbose = verbose)
  saveRDS(x, paste0("veh/", n_TRUCKS[i], ".rds"))
  l_TRUCKS[[i]] <- colSums(x)
}

png("images/FLEET_TRUCKS.png", 2000, 1500, "px",res = 300)
plot(l_TRUCKS[[1]], xlab = "Age", main = tit,
     ylim = c(0, max(unlist(l_TRUCKS))), ylab = "[veh]",
     type = "b", pch = 16, col = cores[1]) 
axis(side = 3,at = c(0, ncol(x)), labels = c(year, year - ncol(x) + 1))
for(i in 2:length(n_TRUCKS)) {
  points(l_TRUCKS[[i]], type = "b", pch = 16, col = cores[i]) 
}
legend(x = "topright", col = cores[1:length(n_TRUCKS)], n_TRUCKS, pch = 16)
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
  "BUS_URBAN_D  é ", round(kBUS_URBAN_D, 3), " de BUS\n",
  "BUS_MICRO_D  é ", round(kBUS_MICRO_D, 3), " de BUS\n",
  "BUS_URBAN_D  é ", round(kBUS_COACH_D, 3), " de BUS\n",
  "sum: ", kBUS_URBAN_D + kBUS_MICRO_D +  kBUS_COACH_D, "\n\n"))

# combustivel
kf <- rep(k_D, 3)

for(i in seq_along(n_BUS)) {
  x <- my_age(x = net[[veiculos[4]]], 
              y = veh[[n_BUS[i]]], 
              name = n_BUS[i],
              k = kBUS[i]*kf[i], 
              verbose = verbose)
  saveRDS(x, paste0("veh/", n_BUS[i], ".rds"))
  l_BUS[[i]] <- colSums(x)
}

png("images/FLEET_BUS.png", 2000, 1500, "px",res = 300)
plot(l_BUS[[1]], xlab = "Age", main = tit,
     ylim = c(0, max(unlist(l_BUS))), ylab = "[veh]",
     type = "b", pch = 16, col = cores[1]) 
axis(side = 3,at = c(0, ncol(x)), labels = c(year, year - ncol(x) + 1))
for(i in 2:length(n_BUS)) {
  points(l_BUS[[i]], type = "b", pch = 16, col = cores[i]) 
}
legend(x = "topright", col = cores[1:length(n_BUS)], n_BUS, pch = 16)
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
  "MC_150_G  é ", round(kMC_150_G, 3), " de MC\n",
  "MC_150_500_G  é ", round(kMC_150_500_G, 3), " de MC\n",
  "MC_500_G  é ", round(kMC_500_G, 3), " de MC\n",
  "MC_150_FG  é ", round(kMC_150_FG, 3), " de MC\n",
  "MC_150_500_FG  é ", round(kMC_150_500_FG, 3), " de MC\n",
  "MC_500_FG  é ", round(kMC_500_FG, 3), " de MC\n",
  "MC_150_FE  é ", round(kMC_150_FE, 3), " de MC\n",
  "MC_150_500_FE  é ", round(kMC_150_500_FE, 3), " de MC\n",
  "MC_500_FE  é ", round(kMC_500_FE, 3), " de MC\n",
  "sum: ", 
  kMC_150_G + kMC_150_500_G +  kMC_500_G +
    kMC_150_FG + kMC_150_500_FG + kMC_500_FG +
    kMC_150_FE + kMC_150_500_FE + kMC_500_FE, "\n\n"))

# combustivel
kf <- c(rep(k_G, 6), rep(k_E, 3))

for(i in seq_along(n_MC)) {
  x <- my_age(x = net[[veiculos[5]]], 
              y = veh[[n_MC[i]]], 
              name = n_MC[i],
              k = kMC[i]*kf[i], 
              verbose = verbose)
  saveRDS(x, paste0("veh/", n_MC[i], ".rds"))
  l_MC[[i]] <- colSums(x)
}

png("images/FLEET_MC.png", 2000, 1500, "px",res = 300)
plot(l_MC[[1]], xlab = "Age", main = tit,
     ylim = c(0, max(unlist(l_MC))), ylab = "[veh]",
     type = "b", pch = 16, col = cores[1]) 
axis(side = 3,at = c(0, ncol(x)), labels = c(year, year - ncol(x) + 1))
for(i in 2:length(n_MC)) {
  points(l_MC[[i]], type = "b", pch = 16, col = cores[i]) 
}
legend(x = "topright", col = cores[1:length(n_MC)], n_MC, pch = 16)
dev.off()

# plots ####
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
  labs(y = "[%]", title = tit2) +
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

cat(paste0("Arquivos em ", getwd(), "/veh\n"))
cat(paste0("Figuras em ", getwd(), "/images\n"))
cat("Limpando... \n")
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
   na, nveh, p, tit, tit2, veiculos, verbose, x, kf,
   k_G, k_D, k_E,
   metadata, net, veh, year)
   
gc()
