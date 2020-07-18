# deleting data
a <- list.files(path = "config", pattern = ".rds", full.names = T)
file.remove(a)

# configuration
metadata <- as.data.frame(metadata)
mileage <- as.data.frame(mileage)
tfs <- as.data.frame(tfs)
veh <- as.data.frame(veh)
fuel <- as.data.frame(fuel)
pmonth <- as.data.frame(pmonth)
met <- as.data.frame(met)[1:12, ] # to avoid possible blank lines

# check metadata$vehicles
cat("Your vehicles are:\n")
print(metadata$vehicles)

# check names mileage
if(!length(intersect(metadata$vehicles, names(mileage))) == length(metadata$vehicles)) {
  stop("You need to add column ",   setdiff(metadata$vehicles, names(mileage)), " in mileage")
}

# check names tfs
if(!length(intersect(metadata$vehicles, names(tfs))) == length(metadata$vehicles)) {
  stop("You need to add column ",   setdiff(metadata$vehicles, names(tfs)), " in tfs")
}

# check names veh
if(!length(intersect(metadata$vehicles, names(veh))) == length(metadata$vehicles)) {
  stop("You need to add column ",   setdiff(metadata$vehicles, names(veh)), " in veh")
}

# check names pmonth
if(!length(intersect(metadata$vehicles, names(pmonth))) == length(metadata$vehicles)) {
  stop("You need to add column ",   setdiff(metadata$vehicles, names(pmonth)), " in veh")
}

# check Year
if(!"Year" %in% names(veh)) stop("I'm not seeing column 'Year' in 'veh'")
if(!"Year" %in% names(mileage)) stop("I'm not seeing column 'Year' in 'mileage'")

# check base year
if(veh$Year[1] != year) stop(paste0("The base year is ", year, 
                                    " but the first year in 'veh' is ", veh$Year[1]))
if(mileage$Year[1] != year) stop(paste0("The base year is ", year, 
                                        " but the first year in 'mileage' is ", mileage$Year[1]))

# add lkm into mileage
cat("Adding `km`` unitinto mileage:\n")
mileage[metadata$vehicles] <- add_lkm(mileage[metadata$vehicles])

# normalizing monthyl profiles
cat("Calculating percentage in monthly profiles:\n")
for(i in seq_along(metadata$vehicles)){
  pmonth[[metadata$vehicles[i]]] <- 100*pmonth[[metadata$vehicles[i]]]/sum(pmonth[[metadata$vehicles[i]]])
}

# saveRDS
message("Files in ", getwd(), "/config/*\n")
saveRDS(metadata, "config/metadata.rds")
saveRDS(mileage, "config/mileage.rds")
saveRDS(tfs, "config/tfs.rds")
saveRDS(veh, "config/fleet_age.rds")
saveRDS(fuel, "config/fuel.rds")
saveRDS(pmonth, "config/pmonth.rds")
saveRDS(met, "config/met.rds")

# pastas
choice <- utils::menu(c("Yes", "No"), title="Delete directories 'csv', 'emi', 'images', 'post' and 'veh'?")
if(choice == 1){
  message("Deleting directories 'csv', 'emi', 'images', 'post' and 'veh'")
  unlink("csv", recursive = T)
  unlink("emi", recursive = T)
  unlink("images", recursive = T)
  unlink("post", recursive = T)
  unlink("veh", recursive = T)
} else {
  message("I did not delete any directory!")
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

message("New directories created:\n")
cat("csv\n")
cat("images\n")
cat(paste0(po,"\n"))
cat(paste0(pa,"\n"))
cat("veh\n")

# Plotting temporal factors ####
cat("Plotting temporal factors `tfs`\n")

# check group names
n_PC <- metadata$vehicles[grep(pattern = "PC", x = metadata$vehicles)]
n_LCV <- metadata$vehicles[grep(pattern = "LCV", x = metadata$vehicles)]
n_TRUCKS <- metadata$vehicles[grep(pattern = "TRUCKS", x = metadata$vehicles)]
n_BUS <- metadata$vehicles[grep(pattern = "BUS", x = metadata$vehicles)]
n_MC <- metadata$vehicles[grep(pattern = "MC", x = metadata$vehicles)]

# PC
df_x <- tfs[, n_PC]
png("images/TFS_PC.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_PC,
        xlab = "Hour",
        ylab = "",
        main = "TFS PC",
        theme = theme) 
dev.off()

# LCV
df_x <- tfs[, n_LCV]
png("images/TFS_LCV.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_LCV,
        xlab = "Hour",
        ylab = "",
        main = "TFS LCV",
        theme = theme) 
dev.off()

# TRUCKS
df_x <- tfs[, n_TRUCKS]
png("images/TFS_TRUCKS.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_TRUCKS,
        xlab = "Hour",
        ylab = "",
        main = "TFS TRUCKS",
        theme = theme) 
dev.off()

# BUS
df_x <- tfs[, n_BUS]
png("images/TFS_BUS.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_BUS,
        xlab = "Hour",
        ylab = "",
        main = "TFS BUS",
        theme = theme) 
dev.off()

# MC
df_x <- tfs[, n_MC]
png("images/TFS_MC.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_MC,
        xlab = "Hour",
        ylab = "",
        main = "TFS MC",
        theme = theme) 
dev.off()

# plotting mileage ####
cat("Plotting mileage \n")

# PC
df_x <- mileage[, n_PC]
png("images/MILEAGE_PC.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_PC,
        xlab = "Age of use", 
        ylab = "Mileage [km/year]",
        main = "Mileage PC",
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
        theme = theme)
dev.off()

# MC
df_x <- mileage[, n_MC]
png("images/MILEAGE_MC.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_MC,
        xlab = "Age of use", 
        ylab = "Mileage [km/year]",
        main = "Mileage MC",
        spl = 8,
        theme = theme)
dev.off()

# Plotting monthly profiles ####
cat("Plotting monthly profiles \n")

# PC
df_x <- pmonth[, n_PC]
png("images/PMONTH_PC.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_PC,
        xlab = "Months", 
        ylab = "Monthly profile [%]",
        main = "Monthly profile PC",
        spl = 5,
        theme = theme)
dev.off()

# LCV
df_x <- pmonth[, n_LCV]
png("images/PMONTH_LCV.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_LCV,
        xlab = "Months", 
        ylab = "Monthly profile [%]",
        main = "Monthly profile LCV",
        theme = theme)
dev.off()

# TRUCKS
df_x <- pmonth[, n_TRUCKS]
png("images/PMONTH_TRUCKS.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_TRUCKS,
        xlab = "Months", 
        ylab = "Monthly profile [%]",
        main = "Monthly profile TRUCKS",
        spl = 8,
        theme = theme)
dev.off()

# BUS
df_x <- pmonth[, n_BUS]
png("images/PMONTH_BUS.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_BUS,
        xlab = "Months", 
        ylab = "Monthly profile [%]",
        main = "Monthly profile BUS",
        spl = 8,
        theme = theme)
dev.off()

# MC
df_x <- pmonth[, n_MC]
png("images/PMONTH_MC.png", 2000, 1500, "px",res = 300)
colplot(df = df_x,
        cols = n_MC,
        xlab = "Months", 
        ylab = "Monthly profile [%]",
        main = "Monthly profile MC",
        spl = 9,
        theme = theme)
dev.off()

# saveRDS
message("Files in ", getwd(), "/config/*\n",
        "config/metadata.rds\n",
        "config/mileage.rds\n",
        "config/tfs.rds\n",
        "config/fleet_age.rds\n",
        "config/fuel.rds\n",
        "config/pmonth.rds\n",
        "config/met.rds\n")

suppressWarnings(
  rm(i, choice, pa, metadata, po, tfs, veh, mileage, fuel,
     n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC, df_x, ef, cores, vkm, ef, a, path,
     pmonth, met, theme)
)
