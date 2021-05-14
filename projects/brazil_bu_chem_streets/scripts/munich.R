#################################################
# 3 approaches
# 1 approach 
# generates files describing geometry and also,
# spatial features CO.rds, NO.rds, NO2.rds and VOC.rds with columns for hours
# 
# 2 approach
# generates txt files of each hour, similar to EL.traf.2014100800
# headers:
# i idbrin typo xa ya xb yb CH4 NMHC CO NOx PA FC SO2 CO2 BE EC OM NO2 
# TSP PM10 PM25 PM1 IP BkF BbF BghiP Fl BaP Py BjF BaA Chr Phe Ant DahA BeP As Cd Cr Cu Hg Ni Pb Se Zn Al Ti Fe Ba Si
#
# 3 approach
# generates binary files, each file with all hours, one file for pollutant
# also files with geometry.

unlink("munich", recursive = T)
dir.create("munich", showWarnings = FALSE)

sink("munich/README")
cat("I`m not an expert on MUNICH and I have provided emissions for others\n")
cat("The method that worked consisted in generating files with hourly emissions for CO, NO, NO2 and VOC\n")
cat("I have read the documentation of MUNICH, but it is not clear to me\n")
cat("Ideally, the outputs of VEIN should create direct inputs for MUNICH\n")
cat("In the meantime, the user must contact the developers Kim and Mario for the pre-processors\n")
cat("-----------------------------------------------------------------------------\n")
cat("1_approach means the approach generating the files for CO, NO, NO2 and VOC\n")
cat("2_approach generates files for each hour\n")
cat("-----------------------------------------------------------------------------\n")
cat("I hope that we can create a better interface\n")
cat(".-Sergio\n")
sink()



dir.create("munich/1_approach", showWarnings = FALSE)
dir.create("munich/2_approach", showWarnings = FALSE)
# dir.create("munich/3_approach", showWarnings = FALSE)

vars <- paste0("V", 1:nrow(tfs))

# 1 approach ####
voc1 <- readRDS("post/streets/E_EVAP_01.rds") # evaporatives
voc2 <- readRDS("post/streets/G_EVAP_01.rds") # evaporatives
voc3 <- readRDS("post/streets/D_NMHC.rds")    # exhaust
voc4 <- readRDS("post/streets/G_NMHC.rds")    # exhaust
voc5 <- readRDS("post/streets/E_NMHC.rds")    # exhaust
voc <- voc1

for(i in seq_along(vars)) voc[[vars[i]]] <- voc1[[vars[i]]]+voc2[[vars[i]]]+voc3[[vars[i]]]+
  voc4[[vars[i]]]+voc5[[vars[i]]]

lpol <- list(
  E_CO = readRDS("post/streets/CO.rds"),
  E_NO = readRDS("post/streets/NO.rds"),
  E_NO2 = readRDS("post/streets/NO2.rds"),
  E_VOC= voc
)
lmunich_pol <- list()

for(j in seq_along(lpol)) {
  
  x <- lpol[[j]]
  
  voc <- st_explode(x)
  
  lkm <- units::set_units(st_length(voc), km)
  
  for(i in seq_along(vars)) voc[[vars[i]]] <- units::set_units(voc[[vars[i]]], g/h)
  
  # changing units for ug, required in munich
  for(i in seq_along(vars)) voc[[vars[i]]] <- units::set_units(voc[[vars[i]]], ug/h)/lkm
  # result in units ug/h/km
  
  lmunich_pol[[j]] <- to_munich(sdf = voc[vars])
  
  saveRDS(lmunich_pol[[j]], paste0("munich/1_approach/RDS_", names(lpol)[j], ".rds"))  
  
  write.table(
    x = lmunich_pol[[j]]$Emissions,
    file = paste0("munich/1_approach/Emissions_ug_h_km_", names(lpol)[j], "_hours.txt"),
    row.names = FALSE,
    sep = " ",
    quote = FALSE
  )
  
  # later, manually edit, change width and height
  write.table(
    x = lmunich_pol[[j]]$Street,
    file = "munich/1_approach/streets.txt", 
    row.names = FALSE,
    sep = " ",
    quote = FALSE
  )
  cat(names(lpol)[j], "\n")
}

names(lmunich_pol) <- names(lpol)

# 2 approach ####
# generates txt files of each hour, similar to EL.traf.2014100800
# headers:
# i idbrin typo xa ya xb yb CH4 NMHC CO NOx NO NO2 SO2 CO2 
# PA FC SO2 CO2 BE EC OM NO2 
# TSP PM10 PM25 PM1 IP BkF BbF BghiP Fl BaP Py BjF BaA Chr Phe Ant DahA BeP As Cd Cr Cu Hg Ni Pb Se Zn Al Ti Fe Ba Si
# OS
fi <- list.files(path = "post/spec_street/", full.names = T)
ni <- list.files(path = "post/spec_street/", full.names = F)
ni <- gsub(".rds", "", ni)
lapply(fi, readRDS) -> lvoc
names(lvoc) <- ni
names(lvoc)

lmunich_voc <- list()

for(j in seq_along(lvoc)) {
  
  x <- lvoc[[j]]
  
  df <- st_explode(x)
  lkm <- units::set_units(st_length(df), km)
  
  
  for(i in seq_along(vars)) df[[vars[i]]] <- units::set_units(df[[vars[i]]], mol/h)
  
  # changing units for ug, required in munich
  for(i in seq_along(vars)) df[[vars[i]]] <- units::set_units(df[[vars[i]]], umol/h)/lkm
  # result in units umol/h/km
  
  lmunich_voc[[j]] <- to_munich(sdf = df[vars])
  cat(names(lvoc)[j], "\n")
}

names(lmunich_voc) <- names(lvoc)

lmunich_voc$E_ALD2$Emissions

head(lmunich_voc$E_ALD2$Emissions)

lx <- c(lmunich_pol, lmunich_voc)
names(lx)
base <- lmunich_voc$E_ALD2$Emissions[, 1:6]

vars <- paste0("V", 1:nrow(tfs))
# hora 1
for(j in 1:nrow(tfs)) {
  for(i in seq_along(lx)) {
    base[[names(lx)[i]]] <- lx[[names(lx)[i]]]$Emissions[[vars[j]]]
}

ifi <- ifelse(nchar(j) == 1, paste0("00", j), 
              ifelse(nchar(j) == 2, paste0("0", j),
                     j))
saveRDS(base, paste0("munich/2_approach/H", ifi, ".rds"))
fwrite(base, paste0("munich/2_approach/H", ifi, ".csv"))
}
sink("munich/2_approach/README")
cat("units for CO, NO, NO2 and COV: ug/h/km\n")
cat("units for CB05 gases: umol/h/km\n")
sink()

head(base)
# 3 approach ####
# generate CO.bin and other
# in development

ls()
suppressWarnings(
  rm(
    "base", "co", "d","df", "etm", "fi", "H001","i","ifi", "j",
    "language", "letm", "letmpol", "lkm",
    "lmunich_pol", "lmunich_voc", "lpol", "lvoc", "lx",
    "net", "ni", "tfs", "vars", "voc", "voc1", "voc2",
    "voc3", "voc4", "voc5", "voc6","x" 
  )
)
gc()
