#' Inventory function.
#'
#' @description \code{inventory} produces an structure of directories and scripts
#' in order to run vein. It is required to know the vehicular composition of the
#' fleet.
#'
#' @param name one word indicating the name of the main directory for running
#' vein. It is better to write the pull path to the new directory.
#' @param vehcomp Vehicular composition of the fleet. It is required a named
#' numerical vector with the names "PC", "LCV", "HGV", "BUS" and "MC". In the
#' case that tthere are no vehiles for one category of the composition, the name
#' should be included with the number zero, for example PC = 0. The maximum
#' number allowed is 99 per category.
#' @param scripts Boolean value for aggregate or no scripts.
#' @param show.dir Boolean alue for printing the created directories.
#' @param show.scripts Boolean value for printing the created scripts.
#' @param clear Boolean value for removing recursively the directory and create
#' another one.
#' @return Structure of directories and scripts for automating compilation of
#' vehicular emissions inventory. The structure can be used with other type of
#' sources of emissions. The structure of the directories is: daily, ef, emi,
#' est, images, network and veh. This structure is a suggestion and the user can
#' use another.
#'
#' daily: it is for storing the profiles saved as .csv files
#'
#' ef: it is for storing the emission factors data-frame, similar to data(fe2015)
#' but including one column for each of the categories of the vehicular
#' composition. For intance, if PC = 5, there should be 5 columns with emission
#' factors in this file. If LCV = 5, another 5 columns should be present, and
#' so on.
#'
#' emi: Directory for saving the estimates. It is suggested to use .rds
#' extension instead of .rda.
#'
#' est: Directory with subdirectories matching the vehicular composition for
#' storing the scripts named input.R.
#'
#' images: Directory for saving images.
#'
#' network: Directory for saving the road network with the required attributes.
#' This file will includes the vehicular flow per street to be used by age*
#' functions.
#'
#' veh: Directory for storing the distribution by age of use of each category of
#' the vehicular composition. Those are data-frames with number of columns with
#' the age distribution and number of rows as the number of streets. The class
#' of these objects is "Vehicles". Future versions of vein will generate
#' Vehicles objects with the explicit spatial component.
#'
#' The name of the scripts and directories are based on the vehicular
#' composition, however, there is included a file named main.R which is just
#' an R script to estimate all the emissions. It is important to note that the
#' user must add the emission factors for other pollutants. Also, this function
#' creates the scripts input.R where the user must specify the inputs for the
#' estimation of emissions of each category. Also, there is a file called
#' traffic.R to generates objects of class "Vehicles".
#' The user can rename these scripts.
#' @export
#' @examples {
#' inventory(name = file.path(tempdir(), "YourCity"), show.dir = TRUE,
#'           show.scripts = TRUE)
#' }
inventory <- function(name,
                      vehcomp = c(PC = 1, LCV = 1, HGV = 1, BUS = 1, MC = 1),
                      scripts = TRUE,
                      show.dir = FALSE,
                      show.scripts = FALSE,
                      clear = TRUE){
  # directorys
  dovein <- function(){
    dir.create(path = name)
    dir.create(path = paste0(name, "/profiles"))
    dir.create(path = paste0(name, "/ef"))
    dir.create(path = paste0(name, "/emi"))
    dir.create(path = paste0(name, "/est"))
    dir.create(path = paste0(name, "/images"))
    dir.create(path = paste0(name, "/network"))
    dir.create(path = paste0(name, "/veh"))

    if(vehcomp["PC"] > 0){
      for(i in 1:vehcomp[1]){
        if(i < 10) {
          dir.create(path = paste0(name, "/emi/PC_0", i))
          dir.create(path = paste0(name, "/est/PC_0", i))
        } else {
          dir.create(path = paste0(name, "/emi/PC_", i))
          dir.create(path = paste0(name, "/est/PC_", i))
        }}
    } else {message("no PC")}

    if(vehcomp["LCV"] > 0){
      for(i in 1:vehcomp[2]){
        if(i < 10) {
          dir.create(path = paste0(name, "/emi/LCV_0", i))
          dir.create(path = paste0(name, "/est/LCV_0", i))
        } else {
          dir.create(path = paste0(name, "/emi/LCV_", i))
          dir.create(path = paste0(name, "/est/LCV_", i))
        }}
    } else {message("no LCV")}

    if(vehcomp["HGV"] > 0){
      for(i in 1:vehcomp[3]){
        if(i < 10) {
          dir.create(path = paste0(name, "/emi/HGV_0", i))
          dir.create(path = paste0(name, "/est/HGV_0", i))
        } else {
          dir.create(path = paste0(name, "/emi/HGV_", i))
          dir.create(path = paste0(name, "/est/HGV_", i))
        }}
    } else {message("no HGV")}

    if(vehcomp["BUS"] > 0){
      for(i in 1:vehcomp[4]){
        if(i < 10) {
          dir.create(path = paste0(name, "/emi/BUS_0", i))
          dir.create(path = paste0(name, "/est/BUS_0", i))
        } else {
          dir.create(path = paste0(name, "/emi/BUS_", i))
          dir.create(path = paste0(name, "/est/BUS_", i))
        }}
    } else {message("no BUS")}

    if(vehcomp["MC"] > 0){
      for(i in 1:vehcomp[5]){
        if(i < 10) {
          dir.create(path = paste0(name, "/emi/MC_0", i))
          dir.create(path = paste0(name, "/est/MC_0", i))
        } else {
          dir.create(path = paste0(name, "/emi/MC_", i))
          dir.create(path = paste0(name, "/est/MC_", i))
        }}
    } else {message("no MC")}
  }
  if(clear == FALSE){
    dovein()
  } else {
    unlink(name, recursive=TRUE)
    dovein()
  }
  # # files
  if(scripts == FALSE){
    message("no scripts")
  } else{
    lista <- list.dirs(path = paste0(name,"/est"))
    lista <- lista[2:length(lista)]
    lista2 <- gsub(pattern = name, x = lista, replacement = "")
    lista3 <- gsub(pattern = "/est/", x = lista2, replacement = "")

    dirs <- list.dirs(path = name, full.names = TRUE)
    cat(paste0("setwd('", dirs[1], "')\n"))


    for (i in 1:length(lista)){
      sink(paste0(dirs[1], "/est/", lista3[i],"_input.R"))
      cat("# Network \n")
      cat("net <- readRDS('network/net.rds')\n")
      cat("lkm <- net$lkm\n")
      cat("# speed <- readRDS('network/speed.rds')\n\n")
      cat("# Vehicles\n")
      cat(paste0("veh <- readRDS('veh/", lista3[[i]], ".rds')"), "\n")
      cat("# Profiles\n")
      cat("data(profiles)\n")
      cat("pc <- profiles[[1]]\n")
      cat("# pc <- read.csv('profiles/pc.csv') #Change with your data\n")
      cat("# Emission Factors data-set\n")
      cat("data(fe2015)\n")
      cat("efe <- fe2015\n")
      cat("# efe <- read.csv('ef/fe2015.csv')\n")
      cat("efeco <- 11 # Number of column of the respective EF\n")
      cat("efero <- ifelse(is.data.frame(veh), ncol(veh), ncol(veh[[1]]))\n")
      cat("# efero reads the number of the vehicle distribution\n")
      cat("trips_per_day <- 5\n\n")
      cat("# Mileage, Check name of categories with names(fkm)\n")
      cat("# data(fkm)\n")
      cat("# pckm <- fkm[['KM_PC_E25']](1:efero)\n")
      cat("# pckm <- cumsum(pckm)\n\n")
      cat("# Sulphur\n")
      cat("# sulphur <- 50 # ppm\n\n\n")
      cat("# Input and Output\n\n")
      cat(paste0("directory <- ", lista3[i], "\n"))
      cat("vfuel <- 'E_25' \n")
      cat("vsize <- '' # It can be small/big/<=1400, one word\n")
      cat("vname <- ", lista3[i], "\n")
      cat("\n\n")
      cat("# CO \n")
      cat("pol <- 'CO' \n")
      cat("print(pol)\n")
      cat("x <- efe[efe$Pollutant == pol, efeco]\n")
      cat("lefe <- EmissionFactorsList(x)\n")
      cat("array_x <- emis(veh = veh, lkm = lkm, ef = lefe, profile = pc)\n")
      cat("x_DF <- emis_post(arra = array_x, veh = vname, size = vsize,\n")
      cat("                  fuel = vfuel, pollutant = pol, by = 'veh')\n")
      cat("x_STREETS <- emis_post(arra = array_x, pollutant = pol,\n")
      cat("                       by = 'streets_wide') \n")
      cat("saveRDS(x_DF, file = paste0('emi/', pol, '_', ",
          deparse(lista3[i]),", '_DF.rds'))\n")
      cat("saveRDS(x_STREETS, file = paste0('emi/', pol, '_', ",
          deparse(lista3[i]),", '_STREETS.rds'))\n")
      cat("rm(array_x, x_DF, x_STREETS, pol, lefe)\n\n")
      cat("# Other Pollutants...")
      sink()
    }
    dirs <- list.dirs(path = name, full.names = TRUE, recursive = TRUE)
    message(paste0("files at ", dirs[1]))
    sink(paste0(name, "/main.R"))
    cat(paste0("setwd('", dirs[1], "')\n"))
    cat("library(vein)\n")
    cat("sessionInfo()\n\n")
    cat("# 1) Network ####\n")
    cat("# Edit your net information and save net.rds it in network directory\n")
    cat("# Your net must contain traffic per street at morning rush hour\n")
    cat("# Below an example using the data in VEIN\n")
    cat("data(net)\n")
    cat("head(net)\n")
    cat("# Are you going to need Speed?\n")
    cat("# if yes, follow the example in netspeed\n")
    cat("# ?netspeed\n")
    cat("saveRDS(net, 'network/net.rds')\n\n")
    cat("# 2) Traffic ####\n")
    cat("# Edit your file traffic.R\n\n")
    cat("source('traffic.R') # Edit traffic.R\n\n")
    cat("# 3) Estimation #### \n")
    cat("# Edit each input.R\n")
    cat("# You must have all the information required in each input.R\n")
    cat("inputs <- list.files(path = 'est', pattern = 'input.R',\n")
    cat("                     recursive = TRUE, full.names = TRUE)\n")
    cat("for (i in 2:length(inputs)){\n")
    cat( "  source(inputs[i])\n" )
    cat("}\n")
    cat("# 4) Post-estimation #### \n")
    cat("CO <- emis_merge('CO', net = net, crs = 31983)\n")
    cat("g <- make_grid(net, 1/102.47, crs = 31983)\n")
    cat("gCO <- emis_grid(CO, g)\n")
    cat("plot(gCO['V1'], axes = TRUE)\n")
    cat("dfco <- emis_merge('CO', what = 'DF.rds', FALSE)\n")
    cat("head(dfco)\n")
    cat("aggregate(dfco$g, by = list(dfco$veh), sum)\n")
    sink()
    sink(paste0(name, "/traffic.R"))
    cat("net <- readRDS('network/net.rds')\n")
    cat("PC_01 <- age_ldv(x = net$ldv, name = 'PC', k = 3/4)\n")
    cat("saveRDS(PC_01, file = 'veh/PC_01.rds')\n")
    cat("LCV_01 <- age_ldv(x = net$ldv, name = 'LCV', k = 1/4/2)\n")
    cat("saveRDS(PC_01, file = 'veh/LCV_01.rds')\n")
    cat("HGV_01 <- age_ldv(x = net$hdv, name = 'HGV', k = 3/4)\n")
    cat("saveRDS(PC_01, file = 'veh/HGV_01.rds')\n")
    cat("BUS_01 <- age_ldv(x = net$hdv, name = 'BUS', k = 1/4)\n")
    cat("# BUS only for example  purposes\n")
    cat("# BUS a traffic simulation only for BUS, or other source of information\n")
    cat("saveRDS(BUS_01, file = 'veh/BUS_01.rds')\n")
    cat("MC_01 <- age_ldv(x = net$ldv, name = 'MC', k = 1/4/2)\n")
    cat("saveRDS(MC_01, file = 'veh/MC_01.rds')\n")
    cat(" # Add more\n")
    sink()

  }

  if(show.dir){
    dirs <- list.dirs(path = name, full.names = TRUE, recursive = TRUE)
    cat("Directories:\n")
    print(dirs)
  }
  if(show.scripts){
    sc <- list.files(path = name, pattern = ".R", recursive = TRUE)
    cat("Scripts:\n")
    print(sc)
  }
}
