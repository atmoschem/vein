# apagando dados
a <- list.files(path = "config", pattern = ".rds", full.names = T)
file.remove(a)

# configuracao
metadata <- as.data.frame(metadata)
mileage <- as.data.frame(mileage)
tfs <- as.data.frame(tfs)
veh <- as.data.frame(veh)
fuel <- as.data.frame(fuel)

# checkar metadata$vehicles
cat( "Metadata$Vehicles é:\n")
print(metadata$vehicles)

# checar nomes mileage
if(!length(intersect(metadata$vehicles, names(mileage))) == length(metadata$vehicles)) {
  stop("Precisa adicionar coluna ",   setdiff(metadata$vehicles, names(mileage)), " em mileage")
}

# checar nomes tfs
if(!length(intersect(metadata$vehicles, names(tfs))) == length(metadata$vehicles)) {
  stop("Precisa adicionar coluna ",   setdiff(metadata$vehicles, names(tfs)), " em tfs")
}

# checar nomes veh
if(!length(intersect(metadata$vehicles, names(veh))) == length(metadata$vehicles)) {
  stop("Precisa adicionar coluna ",   setdiff(metadata$vehicles, names(veh)), " em veh")
}

message("Arquivos em ", getwd(), "/config/*\n")
saveRDS(metadata, "config/metadata.rds")
saveRDS(mileage, "config/mileage.rds")
saveRDS(tfs, "config/tfs.rds")
saveRDS(veh, "config/fleet_age.rds")
saveRDS(fuel, "config/fuel.rds")

# pastas
choice <- utils::menu(c("Sim", "Não"), title="Apagar pastas csv, emi, images, post e veh??")
if(choice == 1){
  message("Apagando pastas emi, images e post")
  unlink("csv", recursive = T)
  unlink("emi", recursive = T)
  unlink("images", recursive = T)
  unlink("post", recursive = T)
  unlink("veh", recursive = T)
} else {
  message("Sem apagar!")
}

dir.create(path = "csv")
dir.create(path = "emi")
dir.create(path = "images")
dir.create(path = "post")
dir.create(path = "post/datatable")
dir.create(path = "post/streets")
dir.create(path = "post/grids")
dir.create(path = "veh")


for(i in seq_along(metadata$vehicles)) {
  dir.create(path = paste0("emi/", metadata$vehicles[i]))
}


pa <- list.dirs(path = "emi", full.names = T, recursive = T)
po <- list.dirs("post", full.names = T, recursive = T)

message("Pastas creadas:\n")
cat("csv\n")
cat("images\n")
cat(paste0(po,"\n"))
cat(paste0(pa,"\n"))
cat("veh\n")


cat("Plotando perfis `tfs`\n")

# identicar nomes de grupos
n_PC <- metadata$vehicles[grep(pattern = "PC", x = metadata$vehicles)]
n_LCV <- metadata$vehicles[grep(pattern = "LCV", x = metadata$vehicles)]
n_TRUCKS <- metadata$vehicles[grep(pattern = "TRUCKS", x = metadata$vehicles)]
n_BUS <- metadata$vehicles[grep(pattern = "BUS", x = metadata$vehicles)]
n_MC <- metadata$vehicles[grep(pattern = "MC", x = metadata$vehicles)]

# PC ####
df_x <- tfs[, n_PC]
png("images/TFS_PC.png", 2000, 1500, "px",res = 300)
plot(df_x[[1]], 
     xlab = "Hour",
     ylab = "tf",
     main = "TFS PC",
     ylim = c(0, max(df_x)), 
     type = "b", pch = 16, col = cores[1]) 
for(i in 2:length(n_PC)) {
  points(df_x[[i]], type = "b", pch = 16, col = cores[i]) 
}
legend(x = "bottomright", col = cores[1:length(n_PC)], n_PC, pch = 16)
dev.off()

# LCV ####
df_x <- tfs[, n_LCV]
png("images/TFS_LCV.png", 2000, 1500, "px",res = 300)
plot(df_x[[1]], 
     xlab = "Hour", 
     ylab = "tf",
     main = "TFS LCV",
     ylim = c(0, max(df_x)), 
     type = "b", pch = 16, col = cores[1]) 
for(i in 2:length(n_LCV)) {
  points(df_x[[i]], type = "b", pch = 16, col = cores[i]) 
}
legend(x = "bottomright", col = cores[1:length(n_LCV)], n_LCV, pch = 16)
dev.off()

# TRUCKS ####
df_x <- tfs[, n_TRUCKS]
png("images/TFS_TRUCKS.png", 2000, 1500, "px",res = 300)
plot(df_x[[1]], 
     xlab = "Hour", 
     ylab = "tf",
     main = "TFS TRUCKS",
     ylim = c(0, max(df_x)), 
     type = "b", pch = 16, col = cores[1]) 
for(i in 2:length(n_TRUCKS)) {
  points(df_x[[i]], type = "b", pch = 16, col = cores[i]) 
}
legend(x = "bottomright", col = cores[1:length(n_TRUCKS)], n_TRUCKS, pch = 16)
dev.off()

# BUS ####
df_x <- tfs[, n_BUS]
png("images/TFS_BUS.png", 2000, 1500, "px",res = 300)
plot(df_x[[1]], 
     xlab = "Hour", 
     ylab = "tf",
     main = "TFS BUS",
     ylim = c(0, max(df_x)), 
     type = "b", pch = 16, col = cores[1]) 
for(i in 2:length(n_BUS)) {
  points(df_x[[i]], type = "b", pch = 16, col = cores[i]) 
}
legend(x = "bottomright", col = cores[1:length(n_BUS)], n_BUS, pch = 16)
dev.off()

# MC ####
df_x <- tfs[, n_MC]
png("images/TFS_MC.png", 2000, 1500, "px",res = 300)
plot(df_x[[1]], 
     xlab = "Hour", 
     ylab = "tf",
     main = "TFS MC",
     ylim = c(0, max(df_x)), 
     type = "b", pch = 16, col = cores[1]) 
for(i in 2:length(n_MC)) {
  points(df_x[[i]], type = "b", pch = 16, col = cores[i]) 
}
legend(x = "bottomright", col = cores[1:length(n_MC)], n_MC, pch = 16)
dev.off()

suppressWarnings(
  rm(i, choice, pa, metadata, po, tfs, veh, mileage, fuel,
   n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC, df_x, ef, cores, vkm, ef, a, rota)
)