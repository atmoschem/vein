# check names
for(i in seq_along(metadata$vehicles)) {
  if(identical(names(veh)[i + 1], metadata$vehicles[i]) == FALSE) {
    cat( "Metadata$Vehicles is: ", metadata$vehicles[i], "but `veh`` is: ", names(veh)[i + 1], "\n")
    stop()
  }
}


# deleting files
cat("Deleting files '.rds' in `veh`\n")
arquivos <- list.files(path = "veh", 
                       pattern = ".rds", 
                       full.names = TRUE)
file.remove(arquivos)

# fleet age
veh[is.na(veh)] <- 0

cat("Plotting fleet `veh`\n")
# identify names in groups
nveh <- names(veh)
n_PC <- nveh[grep(pattern = "PC", x = nveh)]
n_LCV <- nveh[grep(pattern = "LCV", x = nveh)]
n_TRUCKS <- nveh[grep(pattern = "TRUCKS", x = nveh)]
n_BUS <- nveh[grep(pattern = "BUS", x = nveh)]
n_MC <- nveh[grep(pattern = "MC", x = nveh)]

# PC ####
#
# calculate proportion in PC 
#

kPC_G <- sum(veh$PC_G)/sum(veh[, n_PC])
kPC_E <- sum(veh$PC_E)/sum(veh[, n_PC])
kPC_FG <- sum(veh$PC_FG)/sum(veh[, n_PC])
kPC_FE <- sum(veh$PC_FE)/sum(veh[, n_PC])
kPC <- c(kPC_G, kPC_E, kPC_FG, kPC_FE)
l_PC <- list()

cat(paste0(
  "PC_G  is ", round(kPC_G, 3), " of PC\n",
  "PC_E  is ", round(kPC_E, 3), " of PC\n",
  "PC_FG is ", round(kPC_FG, 3), " of PC\n",
  "PC_FE is ", round(kPC_FE, 3), " of PC\n",
  "sum: ", kPC_G + kPC_E + kPC_FG +  kPC_FE, "\n\n"))

# fuel
kf <- c(k_G, k_E, k_G, k_E)
  
for(i in seq_along(n_PC)) {
  x <- veh[[n_PC[i]]]*kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_PC[i], ".rds"))
  l_PC[[i]] <- unlist(x)
}

png("images/FLEET_PC.png", 2000, 1500, "px",res = 300)
plot(l_PC[[1]], 
     xlab = "Age", 
     main = "Passenger Cars by age of use [veh/year]",
     ylim = c(0, max(unlist(l_PC))), 
     ylab = "[veh]",
     type = "b", 
     pch = 16, 
     col = cores[1]) 
mtext(side = 3,
     at = c(0, length(x)), 
     text = c(year, year - length(x) + 1))
for(i in 2:length(n_PC)) {
  points(l_PC[[i]], 
         type = "b", 
         pch = 16 - i, 
         col = cores[i]) 
}
legend(x = "topright", 
       col = cores[1:length(n_PC)], 
       legend = n_PC, 
       pch = c(16, 16 - 2:length(n_PC)))
dev.off()

# LCV ####
#
# calculate proportion in LCV
#

kLCV_G <- sum(veh$LCV_G)/sum(veh[, n_LCV])
kLCV_E <- sum(veh$LCV_E)/sum(veh[, n_LCV])
kLCV_FG <- sum(veh$LCV_FG)/sum(veh[, n_LCV])
kLCV_FE <- sum(veh$LCV_FE)/sum(veh[, n_LCV])
kLCV_D <- sum(veh$LCV_D)/sum(veh[, n_LCV])
kLCV <- c(kLCV_G, kLCV_E, kLCV_FG, kLCV_FE, kLCV_D) 
l_LCV <- list()

cat(paste0(
  "LCV_G  é ", round(kLCV_G, 3), " de LCV\n",
  "LCV_E  é ", round(kLCV_E, 3), " de LCV\n",
  "LCV_FG é ", round(kLCV_FG, 3), " de LCV\n",
  "LCV_FE é ", round(kLCV_FE, 3), " de LCV\n",
  "LCV_D  é ", round(kLCV_D, 3), " de LCV\n\n",
  "sum: ", kLCV_G + kLCV_FG +  kLCV_FE +  kLCV_E + kLCV_D, "\n"))

# fuel
kf <- c(k_G, k_E, k_G, k_E, k_D)

for(i in seq_along(n_LCV)) {
  x <- veh[[n_LCV[i]]]*kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_LCV[i], ".rds"))
  l_LCV[[i]] <- unlist(x)
}

png("images/FLEET_LCV.png", 2000, 1500, "px",res = 300)
plot(l_LCV[[1]], 
     xlab = "Age", 
     main = "Light Commercial Vehicles by age of use [veh/year]",
     ylim = c(0, max(unlist(l_LCV))), 
     ylab = "[veh]",
     type = "b", 
     pch = 16, 
     col = cores[1]) 
mtext(side = 3,
      at = c(0, length(x)), 
      text = c(year, year - length(x) + 1))
for(i in 2:length(n_LCV)) {
  points(l_LCV[[i]], 
         type = "b", 
         pch = 16 - i, 
         col = cores[i]) 
}
legend(x = "topright", 
       col = cores[1:length(n_LCV)], 
       legend = n_LCV, 
       pch = c(16, 16 - 2:length(n_LCV)))
dev.off()

# TRUCKS ####
#
# calculate proportion in TRUCKS
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

# fuel
kf <- rep(k_D, 5)

for(i in seq_along(n_TRUCKS)) {
  x <- veh[[n_TRUCKS[i]]]*kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_TRUCKS[i], ".rds"))
  l_TRUCKS[[i]] <- unlist(x)
}

png("images/FLEET_TRUCKS.png", 2000, 1500, "px",res = 300)
plot(l_TRUCKS[[1]], 
     xlab = "Age", 
     main = "Trucks by age of use [veh/year]",
     ylim = c(0, max(unlist(l_TRUCKS))), 
     ylab = "[veh]",
     type = "b", 
     pch = 16, 
     col = cores[1]) 
mtext(side = 3,
      at = c(0, length(x)), 
      text = c(year, year - length(x) + 1))
for(i in 2:length(n_TRUCKS)) {
  points(l_TRUCKS[[i]], 
         type = "b", 
         pch = 16 - i, 
         col = cores[i]) 
}
legend(x = "topright", 
       col = cores[1:length(n_TRUCKS)], 
       legend = n_TRUCKS, 
       pch = c(16, 16 - 2:length(n_TRUCKS)))
dev.off()


# BUS ####
#
# calculate proportion in BUS
#

kBUS_URBAN_D <- sum(veh$BUS_URBAN_D)/sum(veh[, n_BUS])
kBUS_MICRO_D <- sum(veh$BUS_MICRO_D)/sum(veh[, n_BUS])
kBUS_COACH_D <- sum(veh$BUS_COACH_D)/sum(veh[, n_BUS])
kBUS <- c(kBUS_URBAN_D, kBUS_MICRO_D, kBUS_COACH_D)
l_BUS <- list()

cat(paste0(
  "BUS_URBAN_D  é ", round(kBUS_URBAN_D, 3), " de BUS\n",
  "BUS_MICRO_D  é ", round(kBUS_MICRO_D, 3), " de BUS\n",
  "BUS_URBAN_D  é ", round(kBUS_COACH_D, 3), " de BUS\n",
  "sum: ", kBUS_URBAN_D + kBUS_MICRO_D +  kBUS_COACH_D, "\n\n"))

# fuel
kf <- rep(k_D, 3)

for(i in seq_along(n_BUS)) {
  x <- veh[[n_BUS[i]]]*kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_BUS[i], ".rds"))
  l_BUS[[i]] <- unlist(x)
}

png("images/FLEET_BUS.png", 2000, 1500, "px",res = 300)
plot(l_BUS[[1]], 
     xlab = "Age", 
     main = "Buses by age of use [veh/h]",
     ylim = c(0, max(unlist(l_BUS))), 
     ylab = "[veh]",
     type = "b", 
     pch = 16, 
     col = cores[1]) 
mtext(side = 3,
      at = c(0, length(x)), 
      text = c(year, year - length(x) + 1))
for(i in 2:length(n_BUS)) {
  points(l_BUS[[i]], 
         type = "b", 
         pch = 16 - i, 
         col = cores[i]) 
}
legend(x = "topright", 
       col = cores[1:length(n_BUS)], 
       legend = n_BUS, 
       pch = c(16, 16 - 2:length(n_BUS)))
dev.off()


# MC ####
#
# calculate proportion in MC
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
         kMC_150_FE, kMC_150_500_FE, kMC_500_FE)
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

# fuel
kf <- c(rep(k_G, 6), rep(k_E, 3))

for(i in seq_along(n_MC)) {
  x <- veh[[n_MC[i]]]*kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_MC[i], ".rds"))
  l_MC[[i]] <- unlist(x)
}

png("images/FLEET_MC.png", 2000, 1500, "px",res = 300)
plot(l_MC[[1]], 
     xlab = "Age", 
     main = "Motorcycles by age of use [veh/h]",
     ylim = c(0, max(unlist(l_MC))), 
     ylab = "[veh]",
     type = "b", 
     pch = 16, 
     col = cores[1]) 
mtext(side = 3,
      at = c(0, length(x)), 
      text = c(year, year - length(x) + 1))
for(i in 2:length(n_MC)) {
  points(l_MC[[i]], 
         type = "b", 
         pch = 16 - i, 
         col = cores[i]) 
}
legend(x = "topright", 
       col = cores[1:length(n_MC)], 
       legend = n_MC, 
       pch = c(16, 16 - 2:length(n_MC)))
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
  labs(y = "[%]", 
       title = "Vehicular composition [%]") +
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

message("Figures in images\n")
message("Files in veh\n")
message(paste0("veh/", metadata$vehicles, ".rds\n"))

cat("Cleaning... \n")
suppressWarnings(rm(kPC, kPC_G, kPC_E, kPC_FG, kPC_FE,
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
   metadata, net, veh, year))
invisible(gc())
