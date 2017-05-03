#' Construction function for class "Emissions", "EmissionsList" or
#' "EmissionsArray"
#'
#' @description Returns a tranformed object with class "Emissions",
#' "Emissionslist" or "EmissionsArray". This functions has arguments to change
#' the units. The type of objects supported are of classes "matrix",
#' "data.frame", "list", "array" and "numeric". If the class of the object is "matrix"
#' this function returns a dataframe. When then object is a list with numerics,
#' or a nested list, it returns an "EmissionsList".
#'
#' @param e object with class "Emissions", "EmissionsList" or "EmissionsArray"
#' @param mass Character to determine the unit of the mass. Default is "g"
#' @param time Character to determine the time unit. Default is "h"
#' @return Objects of class "Emissions", "EmissionsList", "EmissionsArray" or
#' "units"
#' @export
#' @examples \dontrun{
#' data(net)
#' data(pc_profile)
#' data(fe2015)
#' data(fkm)
#' PC_G <- c(33491,22340,24818,31808,46458,28574,24856,28972,37818,49050,87923,
#'           133833,138441,142682,171029,151048,115228,98664,126444,101027,
#'           84771,55864,36306,21079,20138,17439, 7854,2215,656,1262,476,512,
#'           1181, 4991, 3711, 5653, 7039, 5839, 4257,3824, 3068)
#' veh <- data.frame(PC_G = PC_G)
#' pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
#' pcw <- temp_fact(net$ldv+net$hdv, pc_profile)
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1,
#' isList = T)
#' pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)
#' cod1 <- emis_det(po = "CO", cc = 1000, eu = "III", km = pckma[1:11])
#' cod2 <- emis_det(po = "CO", cc = 1000, eu = "I", km = pckma[12:24])
#' #vehicles newer than pre-euro
#' co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
#' cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
#' lef <- ef_ldv_scaled(co1, cod, v = "PC", t = "ALL", cc = "ALL",
#'                      f = "G",p = "CO", eu=co1$Euro_LDV)
#' lef <- c(lef,lef[length(lef)],lef[length(lef)],lef[length(lef)],
#'          lef[length(lef)],lef[length(lef)])
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed, agemax = 41,
#'              profile = pc_profile, hour = 24, day = 7, array = F)
#' class(E_CO)
#' E_COl <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed, agemax = 41,
#'               profile = pc_profile, hour = 24, day = 7, array = F)
#' class(E_COl)
#' }
as.Emissions <- function(e, mass = "g", time = "h", ...) {
  if ( is.matrix(e) ) {
    ex <- as.data.frame(e)
    for(i in 1:ncol(ex)){
      ex[,i] <- ex[,i] * parse_unit(paste0(mass," ", time,"-1"))
    }
    class(ex) <- c("Emissions", class(ex))
  } else if (is.data.frame(e)) {
    for(i in 1:ncol(e)){
      e[,i] <- e[,i] * parse_unit(paste0(mass," ", time,"-1"))
    }
    ex <- e
    class(ex) <- c("Emissions",class(e))
  } else if ( is.list(e) && is.numeric(e[[1]]) ){
    ex <-  lapply(1:length(e), function(i)  {
      e[[i]] <- e[[i]] * parse_unit(paste0(mass," ", time, "-1"))
    })
    class(ex) <- c("EmissionsList",class(e))
  } else if ( is.list(e) && is.list(e[[1]]) && is.numeric(e[[1]][[1]]) ) {
    ex <-  lapply(1:length(e), function(i)  {
      lapply(1:length(e[[1]]), function(j)  {
        e[[i]][[j]] <- e[[i]][[j]] * parse_unit(paste0(mass," ", time, "-1"))
      }) })
    class(ex) <- c("EmissionsList",class(e))
    class(ex[[1]]) <- c("EmissionsList",class(e))
  } else if ( is.list(e) && is.list(e[[1]]) && is.list(e[[1]][[1]] ) &&
              is.numeric(e[[1]][[1]][[1]]) ){
    ex <-  lapply(1:length(e), function(i)  {
      lapply(1:length(e[[1]]), function(j) {
        lapply(1:length(e[[1]][[1]]), function(k) {
          e[[i]][[j]][[k]] <- e[[i]][[j]][[k]] * parse_unit(paste0(mass," ", time, "-1"))
        }) }) })
    class(ex[[1]][[1]]) <- c("EmissionsList",class(e))
    class(ex[[1]]) <- c("EmissionsList",class(e))
    class(ex) <- c("EmissionsList",class(e))
  } else if ( is.array(e) ) {
    ex <- e
    class(ex) <- c("EmissionsArray",class(e))
  } else if (is.numeric(e)) {
    ex <- e * parse_unit(paste0(mass," ", time, "-1"))
  }
  return(ex)
}
