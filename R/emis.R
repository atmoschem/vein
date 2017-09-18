#' Emissions estimation hourly for the of the week
#'
#' The vehicular emissions are estimated as the product of the vehicles on a
#' road, length of the road, emission factor avaliated at the respective speed.
#'  \eqn{E = VEH*LENGTH*EF(speed)}
#'
#' @param veh "Vehicles" data-frame or list of "Vehicles" data-frame. Each data-frame
#' as number of columns matching the age distribution of that ype of vehicle.
#' The number of rows is equal to the number of streets link
#' @param lkm Length of each link
#' @param ef List of functions of emission factors
#' @param speed Speed data-frame with number of columns as hours
#' @param agemax Age of oldest vehicles for that category
#' @param profile Numerical or dataframe with nrows equal to 24 and ncol 7 day of the week
#' @param hour Number of considered hours in estimation
#' @param day Number of considered days in estimation
#' @param array When FALSE produces a dataframe of the estimation. When TRUE expects a
#' profile as a dataframe producing an array with dimensions (streets x columns x hours x days)
#' @return emission estimation  g/h
#' @export
#' @examples \dontrun{
#' # Do not run
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
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)
#' cod1 <- emis_det(po = "CO", cc = 1000, eu = "III", km = pckma[1:11])
#' cod2 <- emis_det(po = "CO", cc = 1000, eu = "I", km = pckma[12:24])
#' #vehicles newer than pre-euro
#' co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
#' cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
#' lef <- ef_ldv_scaled(co1, cod, v = "PC", t = "4S", cc = "<=1400",
#'                      f = "G",p = "CO", eu=co1$Euro_LDV)
#' lef <- c(lef,lef[length(lef)],lef[length(lef)],lef[length(lef)],
#'          lef[length(lef)],lef[length(lef)])
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed,
#'              profile = pc_profile, hour = 24, day = 7, array = T)
#' class(E_CO)
#' lpc <- list(pc1,pc1)
#' E_COv2 <- emis(veh = lpc,lkm = net$lkm, ef = lef, speed = speed,
#'                hour = 2, day = 1, array = T)
#' # Entering wrong results
#' pc1[ , ncol(pc1) + 1] <- pc1$PC_1
#' dim(pc1)
#' length(lef)
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed,
#'              profile = pc_profile, hour = 24, day = 7, array = T)
#' E_COv2 <- emis(veh = lpc,lkm = net$lkm, ef = lef, speed = speed,
#'                hour = 2, day = 1, array = T)
#' }
emis <- function (veh, lkm, ef, speed,
                  agemax = if (!inherits(x = veh, what = "list")) {
                    ncol(veh)
                    } else {
                      ncol(veh[[1]])
                    },
                  profile, hour = 24, day = 7,
                  array = T) {
  if(units(lkm)$numerator == "m" ){
    warning("Units of lkm is 'm' ")
  }
  lkm <- as.numeric(lkm)
  speed <- as.data.frame(speed)
  for (i  in 1:ncol(speed) ) {
    speed[, i] <- as.numeric(speed[, i])
  }
  if (!inherits(x = veh, what = "list")) {
      veh <- as.data.frame(veh)
      for (i  in 1:ncol(veh) ) {
        veh[,i] <- as.numeric(veh[,i])
      }
      if (ncol(veh) != length(ef)){
        stop("Number of columns in 'veh' must be the same as length of ef")
      }

      if(array == F){
      lista <- lapply(1:day,function(j){
        lapply(1:hour,function(i){
          lapply(1:agemax, function(k){
            veh[, k]*profile[i,j]*lkm*ef[[k]](speed[, i])
            }) }) })
        return(EmissionsList(lista))
      } else {
      d <-  simplify2array(
        lapply(1:day,function(j){
          simplify2array(
            lapply(1:hour,function(i){
              simplify2array(
                lapply(1:agemax, function(k){
                  veh[, k]*profile[i,j]*lkm*ef[[k]](speed[, i])
                  }) ) }) ) }) )
      return(EmissionsArray(d))
      message(sum(d, na.rm = T)/1000, " kg emissions in", hour, "hours and", day, "days")
      }
  } else {
    if (ncol(veh[[1]]) != length(ef)){
      stop("Number of columns in 'veh' must be the same as length of ef")
    } else if(length(veh) != ncol(speed)) {
      stop("Length of 'veh' must be the same as number of columns of speed")
    }
    for (j in 1:length(veh)) {
        for (i  in 1:ncol(veh[[j]]) ) {
          veh[[j]][,i] <- as.numeric(veh[[j]][,i])
        } }
        if(array == F){
          lista <- lapply(1:length(veh),function(i){
              lapply(1:agemax, function(k){
                veh[[i]][, k]*lkm*ef[[k]](speed[, i])
              } ) } )
          return(EmissionsList(lista))
        } else {
          d <-  simplify2array(
                lapply(1:length(veh),function(i){
                  simplify2array(
                    lapply(1:agemax, function(k){
                      veh[[i]][, k]*lkm*ef[[k]](speed[, i])
                   }) ) }) )
          return(EmissionsArray(d))
          message(sum(d, na.rm = T)/1000, " kg emissions in", hour, "hours")
        }
      }
}

