#' Estimation of cold start emissions hourly for the of the week
#'
#' The vehicular emissions are estimated as the product of the vehicles on a
#' road, length of the road, emission factor avaliated at the respective speed.
#' The estimation considers beta
#' parameter, the fraction of mileage driven
#'
#' @param veh Numeric vector with length of elements equals to number of streets
#' @param lkm Length of each link
#' @param ef List of functions of emission factors of vehicular categories
#' @param efcold List of functions of cold start emission factors of vehicular categories
#' @param beta Datraframe with the hourly cold-start distribution to each day
#' of the period. Number of rows are hours and columns are days
#' @param speed List of speeds
#' @param agemax Age of oldest vehicles for that category
#' @param profile Numerical or dataframe with nrows equal to 24 and ncol 7 day
#' of the week
#' @param hour Number of considered hours in estimation
#' @param day Number of considered days in estimation
#' @param array When FALSE produces a dataframe of the estimation. When TRUE
#' expects a profile as a dataframe producing an array with dimensions
#' (streets x columns x hours x days)
#' @return EmissionsArray  g/h
#' @export
#' @note Actually dcold is not necessary, it would be enough to multiply
#' an existing cold-start distribution with the daily profile, but it was added
#' because it is important to clarify both, the data and the concepts
#' @examples \dontrun{
#' # Do not run
#' data(net)
#' data(pc_profile)
#' data(fe2015)
#' data(fkm)
#' data(pc_cold)
#' pcf <- as.data.frame(cbind(pc_cold,pc_cold,pc_cold,pc_cold,pc_cold,pc_cold,
#' pc_cold))
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
#' # Mohtly average temperature 18 Celcius degrees
#' lefc <- ef_ldv_cold_list(df = co1, ta = 18, cc = "<=1400", f = "G",
#'                           eu = co1$Euro_LDV, p = "CO" )
#' lefec <- c(lefc,lefc[[length(lefc)]],lefc[[length(lefc)]],
#'             lefc[[length(lefc)]],lefc[[length(lefc)]],lef[[length(lef)]])
#' class(lefec)
#' PC_CO_COLD <- emis_cold(veh = pc1, lkm = net$lkm, ef = lef, efcold = lefec,
#' beta = pcf, speed = speed, agemax = 41, profile = pc_profile, hour = 24,
#' day = 7, array = T)
#' class(PC_CO_COLD)
#' plot(PC_CO_COLD)
#' }
emis_cold <- function (veh, lkm, ef, efcold, beta, speed, agemax, profile,
                       hour = 1, day = 1, array = F) {
  veh <- as.data.frame(veh)
  lkm <- as.numeric(lkm)
  for(i in 1:ncol(veh)){
    veh[,i] <- as.numeric(veh[,i])
  }
  if(array == F){
    lista <- lapply(1:day,function(j){
      lapply(1:hour,function(i){
        lapply(1:agemax, function(k){
          beta[i,j]*veh[, k]*profile[i,j]*lkm*ef[[k]](speed[[j]][[i]])*
            ifelse((efcold[[k]](speed[[j]][[i]])-1)<0,0,
                   (efcold[[k]](speed[[j]][[i]])-1))
          })
      })
    })
    return(EmissionsList(lista))
  } else {
    veh <- as.data.frame(veh)
    lkm <- as.numeric(lkm)
    for(i in 1:ncol(veh)){
      veh[,i] <- as.numeric(veh[,i])
    }
    d <-  simplify2array(
    lapply(1:day,function(j){
      simplify2array(
        lapply(1:hour,function(i){
          simplify2array(
            lapply(1:agemax, function(k){
              beta[i,j]*veh[, k]*profile[i,j]*lkm*ef[[k]](speed[[j]][[i]])*
                ifelse((efcold[[k]](speed[[j]][[i]])-1)<0,0,
                       (efcold[[k]](speed[[j]][[i]])-1))
              })
            )
          })
        )
      })
    )
  return(EmissionsArray(d))
  }
}

