#' Emission estimation from tyre, brake and road surface wear
#'
#' @description \code{emis_wear} estimates wear emissions. The sources are tyres,
#' breaks and road surface.
#'
#' @param veh Object of class "Vehicles"
#' @param lkm Length of the road in km.
#' @param ef list of emission factor functions class "EmissionFactorsList",
#' length equals to hours.
#' @param what Character for indicating "tyre", "break" or "road"
#' @param speed Speed data-frame with number of columns as hours
#' @param agemax Age of oldest vehicles for that category
#' @param profile Numerical or dataframe with nrows equal to 24 and ncol
#' 7 day of the week
#' @param hour Number of considered hours in estimation
#' @param day Number of considered days in estimation
#' @return emission estimation  g/h
#' @references Ntziachristos and Boulter 2016. Automobile tyre and break wear
#' and road abrasion. In: EEA, EMEP. EEA air pollutant emission inventory
#' guidebook-2009. European Environment Agency, Copenhagen, 2016
#' @export
#' @examples \dontrun{
#' data(net)
#' data(pc_profile)
#' pc_week <- temp_fact(net$ldv[1:10] + net$hdv[1:10], pc_profile[, 1])
#' df <- netspeed(pc_week, net$ps[1:10], net$ffs[1:10],
#'               net$capacity[1:10], net$lkm[1:10], alpha = 1)
#' ef <- ef_wear(wear = "tyre", type = "PC", pol = "PM10", speed = df)
#' emi <- emis_wear(veh = age_ldv(net$ldv[1:10], name = "VEH"),
#'                  lkm = net$lkm[1:10], ef = ef, speed = df,
#'                  profile = pc_profile[, 1])
#' emi
#' }
emis_wear <- function (veh,
                       lkm,
                       ef,
                       what = "tyre",
                       speed,
                       agemax = ncol(veh),
                       profile,
                       hour = nrow(profile),
                       day = ncol(profile)) {
  if(units(lkm)$numerator == "m" ){
    stop("Units of lkm is 'm'")
  }
  veh <- as.data.frame(veh)
  lkm <- as.numeric(lkm)
  for (i  in 1:ncol(veh) ) {
    veh[,i] <- as.numeric(veh[,i])
  }
  for (i  in 1:ncol(speed) ) {
    speed[,i] <- as.numeric(speed[, i])
  }
  #profile
  if(is.vector(profile)){
    profile <- matrix(as.numeric(profile), ncol = 1)
  } # if it is data.frame or matrix, OK
  # if(!missing(profile) & is.vector(profile)){
  #   profile <- matrix(profile, ncol = 1)
  # }

  # ncol ef
  if(ncol(ef)/24 != day){
    stop("Number of days of ef and profile must be the same")
  }

  lef <- lapply(1:day, function(i){
    as.list(ef[, (24*(i-1) + 1):(24*i)])
  })

  # lapply(ef, as.list)
  if (what == "tyre"){
    d <-  simplify2array(
      lapply(1:day,function(j){
        simplify2array(
          lapply(1:hour,function(i){
            simplify2array(
              lapply(1:agemax, function(k){
                ifelse(
                  speed[,i] < 40,
                  veh[, k]*profile[i,j]*lkm*lef[[j]][[i]]*1.67,
                  ifelse(
                    speed[,i] >= 40 & speed[,i] <= 95,
                    veh[, k]*profile[i,j]*lkm*lef[[j]][[i]]*(-0.0270*speed[, i] + 2.75),
                    veh[, k]*profile[i,j]*lkm*lef[[j]][[i]]*0.185
                  )) })) })) }))
  } else if(what == "break"){
    d <-  simplify2array(
      lapply(1:day,function(j){
        simplify2array(
          lapply(1:hour,function(i){
            simplify2array(
              lapply(1:agemax, function(k){
                ifelse(
                  speed[,i] < 40,
                  veh[, k]*profile[i,j]*lkm*lef[[j]][[i]]*1.39,
                  ifelse(
                    speed[,i] >= 40 & speed[,i] < 80,
                    veh[, k]*profile[i,j]*lkm*lef[[j]][[i]]*(-0.00974*speed[, i] + 1.78),
                    ifelse(
                      speed[,i] == 80,
                      veh[, k]*profile[i,j]*lkm*lef[[j]][[i]],
                      ifelse(
                        speed[,i] > 80 & speed[,i] <= 90,
                        veh[, k]*profile[i,j]*lkm*lef[[j]][[i]]*(-0.00974*speed[, i] + 1.78),
                        veh[, k]*profile[i,j]*lkm*lef[[j]][[i]]*0.902
                      )))) })) })) }))

  } else if (what == "road"){
    d <-  simplify2array(
      lapply(1:day,function(j){
        simplify2array(
          lapply(1:hour,function(i){
            simplify2array(
              lapply(1:agemax, function(k){
                veh[, k]*profile[i,j]*lkm*lef[[j]][[i]]
              })) })) }))

  }
  return(EmissionsArray(d))
}
