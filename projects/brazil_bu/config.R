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

#checar Year
if(!"Year" %in% names(veh)) stop("Não estou enxergando a coluna 'Year' em 'veh'")
if(!"Year" %in% names(mileage)) stop("Não estou enxergando a coluna 'Year' em 'mileage'")
# if(!"Year" %in% names(ef)) stop("Não estou enxergando a coluna 'Year' em 'ef'")

# checar ano base
if(veh$Year[1] != year) stop(paste0("The base year is ", year,
                                    " but the first year in 'veh' is ", veh$Year[1]))
if(mileage$Year[1] != year) stop(paste0("The base year is ", year,
                                        " but the first year in 'mileage' is ", mileage$Year[1]))
# if(ef$Year[1] != year) stop(paste0("The base year is ", year,
#                                    " but the first year in 'ef' is ", ef$Year[1]))

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

# identicar nomes de grupos ####
n_PC <- metadata$vehicles[grep(pattern = "PC", x = metadata$vehicles)]
n_LCV <- metadata$vehicles[grep(pattern = "LCV", x = metadata$vehicles)]
n_TRUCKS <- metadata$vehicles[grep(pattern = "TRUCKS", x = metadata$vehicles)]
n_BUS <- metadata$vehicles[grep(pattern = "BUS", x = metadata$vehicles)]
n_MC <- metadata$vehicles[grep(pattern = "MC", x = metadata$vehicles)]

cat("Plotando frota \n")

# PC ####
df_x <- veh[, n_PC]
png("images/FLEET_PC.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_PC,
        xlab = "Age",
        ylab = "veh/h",
        main = "PC",
        type = "l",
        pch = NULL,
        theme = theme)
dev.off()

# LCV ####
df_x <- veh[, n_LCV]
png("images/FLEET_LCV.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_LCV,
        xlab = "Age",
        ylab = "veh/h",
        main = "LCV",
        type = "l",
        pch = NULL,
        theme = theme)
dev.off()

# TRUCKS ####
df_x <- veh[, n_TRUCKS]
png("images/FLEET_TRUCKS.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_TRUCKS,
        xlab = "Age",
        ylab = "veh/h",
        main = "TRUCKS",
        type = "l",
        pch = NULL,
        spl = 8,
        theme = theme)
dev.off()

# BUS ####
df_x <- veh[, n_BUS]
png("images/FLEET_BUS.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_BUS,
        xlab = "Age",
        ylab = "veh/h",
        main = "BUS",
        type = "l",
        pch = NULL,
        spl = 8,
        theme = theme)
dev.off()

# MC ####
df_x <- veh[, n_MC]
png("images/FLEET_MC.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_MC,
        xlab = "Age",
        ylab = "veh/h",
        main = "Motorcycles",
        type = "l",
        pch = NULL,
        spl = 9,
        theme = theme)
dev.off()


cat("Plotando perfis `tfs`\n")


# PC ####
df_x <- tfs[, n_PC]
png("images/TFS_PC.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_PC,
        xlab = "Hour",
        ylab = "",
        main = "TFS PC",
        type = "l",
        pch = NULL,
        theme = theme)
dev.off()

# LCV ####
df_x <- tfs[, n_LCV]
png("images/TFS_LCV.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_LCV,
        xlab = "Hour",
        ylab = "",
        main = "TFS LCV",
        type = "l",
        pch = NULL,
        theme = theme)
dev.off()

# TRUCKS ####
df_x <- tfs[, n_TRUCKS]
png("images/TFS_TRUCKS.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_TRUCKS,
        xlab = "Hour",
        ylab = "",
        main = "TFS TRUCKS",
        type = "l",
        pch = NULL,
        spl = 8,
        theme = theme)
dev.off()

# BUS ####
df_x <- tfs[, n_BUS]
png("images/TFS_BUS.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_BUS,
        xlab = "Hour",
        ylab = "",
        main = "TFS BUS",
        type = "l",
        pch = NULL,
        spl = 8,
        theme = theme)
dev.off()

# MC ####
df_x <- tfs[, n_MC]
png("images/TFS_MC.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_MC,
        xlab = "Hour",
        ylab = "",
        main = "TFS MC",
        type = "l",
        pch = NULL,
        spl = 9,
        theme = theme)
dev.off()

cat("Plotando quilometragem \n")
# PC
df_x <- mileage[, n_PC]
png("images/MILEAGE_PC.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_PC,
        xlab = "Age of use",
        ylab = "Mileage [km/year]",
        main = "Mileage PC",
        type = "l",
        pch = NULL,
        theme = theme)
dev.off()

# LCV
df_x <- mileage[, n_LCV]
png("images/MILEAGE_LCV.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_LCV,
        xlab = "Age of use",
        ylab = "Mileage [km/year]",
        main = "Mileage LCV",
        type = "l",
        pch = NULL,
        theme = theme)
dev.off()

# TRUCKS
df_x <- mileage[, n_TRUCKS]
png("images/MILEAGE_TRUCKS.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_TRUCKS,
        xlab = "Age of use",
        ylab = "Mileage [km/year]",
        main = "Mileage TRUCKS",
        type = "l",
        pch = NULL,
        spl = 8,
        theme = theme)
dev.off()

# BUS
df_x <- mileage[, n_BUS]
png("images/MILEAGE_BUS.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_BUS,
        xlab = "Age of use",
        ylab = "Mileage [km/year]",
        main = "Mileage BUS",
        type = "l",
        pch = NULL,
        theme = theme,
        spl = 8)
dev.off()

# MC
df_x <- mileage[, n_MC]
png("images/MILEAGE_MC.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_MC,
        xlab = "Age of use",
        ylab = "Mileage [km/year]",
        main = "Mileage MC",
        type = "l",
        pch = NULL,
        spl = 9,
        theme = theme)
dev.off()

# saveRDS

message("Files in ", getwd(), "/config/*\n",
        "config/metadata.rds\n",
        "config/mileage.rds\n",
        "config/tfs.rds\n",
        "config/fleet_age.rds\n",
        "config/fuel.rds\n")


suppressWarnings(
  rm(i, choice, pa, metadata, po, tfs, veh, mileage, fuel, theme,
     n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC, df_x, ef, cores, vkm, ef, a, rota)
)
