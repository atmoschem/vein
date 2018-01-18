#' Vehicular Emissions Inventory (VEIN) function.
#'
#' @description This function produces an structure of directories and scripts
#' in order to run vein. It is required to know the vehicular composition of the
#' fleet.
#'
#' @param name one word indicating the name of the main directory for running
#' vein
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
#' composition, however, there is included a file named vein.R which is just
#' an R script to estimate all the emissions. It is important to note that the
#' user must add the emission factors for other pollutants. Also, this function
#' creates the scripts input.R where the user must specify the inputs for the
#' estimation of emissions of each category. The user can rename these scripts.
#' @export
#' @examples \dontrun{
#' # Do not run
#' # setwd("path")
#' vein(name = "cityVEIN", show.dir = T)
#' }
vein <- function(name,
                 vehcomp = c(PC = 3, LCV = 4, HGV = 5, BUS = 3, MC = 3),
                 scripts = TRUE,
                 show.dir = FALSE,
                 show.scripts = FALSE,
                 clear = TRUE){
  # directorys
  dovein <- function(){
    dir.create(path = name)
    dir.create(path = paste0(name, "/daily"))
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
    } else { NULL }

    if(vehcomp["LCV"] > 0){
      for(i in 1:vehcomp[2]){
        if(i < 10) {
          dir.create(path = paste0(name, "/emi/LCV_0", i))
          dir.create(path = paste0(name, "/est/LCV_0", i))
        } else {
          dir.create(path = paste0(name, "/emi/LCV_", i))
          dir.create(path = paste0(name, "/est/LCV_", i))
        }}
    } else { NULL }

    if(vehcomp["HGV"] > 0){
      for(i in 1:vehcomp[3]){
        if(i < 10) {
          dir.create(path = paste0(name, "/emi/HGV_0", i))
          dir.create(path = paste0(name, "/est/HGV_0", i))
        } else {
          dir.create(path = paste0(name, "/emi/HGV_", i))
          dir.create(path = paste0(name, "/est/HGV_", i))
        }}
    } else { NULL }

    if(vehcomp["BUS"] > 0){
      for(i in 1:vehcomp[4]){
        if(i < 10) {
          dir.create(path = paste0(name, "/emi/BUS_0", i))
          dir.create(path = paste0(name, "/est/BUS_0", i))
        } else {
          dir.create(path = paste0(name, "/emi/BUS_", i))
          dir.create(path = paste0(name, "/est/BUS_", i))
        }}
    } else { NULL }

    if(vehcomp["MC"] > 0){
      for(i in 1:vehcomp[5]){
        if(i < 10) {
          dir.create(path = paste0(name, "/emi/MC_0", i))
          dir.create(path = paste0(name, "/est/MC_0", i))
        } else {
          dir.create(path = paste0(name, "/emi/MC_", i))
          dir.create(path = paste0(name, "/est/MC_", i))
        }}
    } else { NULL }
  }
  if(clear == FALSE){
    dovein()
  } else {
    unlink(name, recursive=TRUE)
    dovein()
  }
  # # files
  if(scripts == FALSE){
    NULL
  } else{
    lista <- list.dirs(path = paste0(name,"/est"))
    lista <- lista[2:length(lista)]
    lista2 <- gsub(pattern = name, x = lista, replacement = "")
    lista3 <- gsub(pattern = "/est/", x = lista2, replacement = "")

    for (i in 1:length(lista)){
      sink(paste0(lista[[i]], "/input.R"))
      cat("# Network \n")
      cat("net <- readRDS('PATH')\n")
      cat("lkm <- net$lkm\n")
      cat("speed <- readRDS('PATH')\n\n")
      cat("# Vehicles\n")
      cat("veh <- readRDS('PATH')\n")
      cat("# Profiles\n")
      cat("pc <- read.csv('daily/pc.csv') #Change accordingly\n")
      cat("pcf <- read.csv('daily/pcf.csv') #For Cold Starts\n\n")
      cat("# Emission Factors data-set\n")
      cat("efe <- read.csv('ef/ef2014.csv')\n")
      cat("efeco <- 10 #Number of column of the respective EF\n")
      cat("efero <- ifelse(is.data.frame(veh), ncol(veh), ncol(veh[[1]]))\n")
      cat("# efero reads the number of the vehicle distribution\n")
      cat("# Evaporative Emission Factors\n")
      cat("evap <- read.csv('PATH')\n")
      cat("trips_per_day <- 5\n\n")
      cat("# Mileage, Check name of categories with names(fkm)\n")
      cat("data(fkm)\n")
      cat("pckm <- fkm[['KM_PC_E25']](1:efero)\n")
      cat("pckm <- cumsum(pckm)\n\n")
      cat("# Sulphur\n")
      cat("sulphur <- 50 # ppm\n")
      cat("# Input and Output\n\n")
      cat(paste0("directory <- ", deparse(lista3[[i]]), "\n"))
      cat("vfuel <- 'E_25' \n")
      cat("vsize <- '<=1400' \n")
      cat("vname <- 'PC'\n")
      cat("\n\n")
      cat("# CO \n")
      cat("pol <- 'CO' \n")
      cat("x <- efe[1:efero & efe$Pollutant == pol, efeco]\n")
      cat("lefe <- EmissionFactorsList(x)\n")
      cat("array_x <- emis(veh = veh, lkm = lkm,ef = lefe,  speed = speed,\n")
      cat("                profile = profile\n")
      cat("x_DF <- emis_post(arra = array_x, veh = vname, size = vsize,\n")
      cat("                  fuel = vfuel, pollutant = pol, by = 'veh'\n")
      cat("x_STREETS <- emis_post(arra = array_x, pollutant = pol,\n")
      cat("                       by = streets_wide) \n")
      cat("saveRDS(x_DF, file = paste0('emi/', directory, '/', pol, '_',\n")
      cat("        vname, '_', vsize, '_', vfuel,'_DF.rds')) \n")
      cat("saveRDS(x_STREETS, file = paste0('emi/', directory, '/', pol, '_',\n")
      cat("        vname, '_', vsize, '_', vfuel,'_DF.rds')) \n")
      cat("rm(array_x); rm(x_DF); rm(x_STREETS); rm(pol); rm(lefe)\n\n")
      cat("# Other Pollutants...")
      sink()
    }
    sink(paste0(name, "/vein.R"))
    cat("library(vein)\n")
    cat("sessionInfo()\n\n")
    cat("# Estimation\n")
    cat("dirs <- list.dirs(path = ", deparse(paste0(name, "/est")), ")\n")
    cat("for (i in 2:length(dirs)){\n")
    cat( "  source(paste0(dirs[i], '/input.R'))\n" )
    cat("}\n")
    sink()
  }
  if(show.dir){
    dirs <- list.dirs(path = name, full.names = T, recursive = T)
    cat("Directories:\n")
    print(dirs)
  }
  if(show.scripts){
    sc <- list.files(path = name, pattern = ".R", recursive = T)
    cat("Scripts:\n")
    print(sc)
  }
}
