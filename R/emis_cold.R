#' Estimation of cold start emissions hourly for the of the week
#'
#' @description \code{emis_cold} emissions are estimated as the product of the
#' vehicles on a road, length of the road, emission factor avaliated at the
#' respective speed.The estimation considers beta parameter, the fraction of
#' mileage driven
#'
#' @param veh "Vehicles" data-frame or list of "Vehicles" data-frame. Each data-frame
#' as number of columns matching the age distribution of that ype of vehicle.
#' The number of rows is equal to the number of streets link
#' @param lkm Length of each link
#' @param ef List of functions of emission factors of vehicular categories
#' @param efcold List of functions of cold start emission factors of vehicular categories
#' @param beta Datraframe with the hourly cold-start distribution to each day
#' of the period. Number of rows are hours and columns are days
#' @param speed Speed data-frame with number of columns as hours
#' @param agemax Age of oldest vehicles for that category
#' @param profile Numerical or dataframe with nrows equal to 24 and ncol 7 day
#' of the week
#' @param simplify Logical; to determine if EmissionsArray should les dimensions,
#' being streets, vehicle categories and hours or default (streets, vehicle
#' categories, hours and days). Default is FALSE to avoid break old code, but
#' the recommendation is that new estimations use this parameter as TRUE
#' @param hour Number of considered hours in estimation
#' @param day Number of considered days in estimation
#' @param array Deprecated! \code{\link{emis_cold}} returns only arrays.
#' When TRUE and veh is not a list, expects a profile as a dataframe producing
#' an array with dimensions (streets x columns x hours x days)
#' @param verbose Logical; To show more information
#' @return EmissionsArray  g/h
#' @export
#' @examples {
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
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' pckm <- units::set_units(fkm[[1]](1:24), "km"); pckma <- cumsum(pckm)
#' cod1 <- emis_det(po = "CO", cc = 1000, eu = "III", km = pckma[1:11])
#' cod2 <- emis_det(po = "CO", cc = 1000, eu = "I", km = pckma[12:24])
#' #vehicles newer than pre-euro
#' co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
#' cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
#' lef <- ef_ldv_scaled(co1, cod, v = "PC", cc = "<=1400",
#'                      f = "G",p = "CO", eu=co1$Euro_LDV)
#' # Mohtly average temperature 18 Celcius degrees
#' lefec <- ef_ldv_cold_list(df = co1, ta = 18, cc = "<=1400", f = "G",
#'                           eu = co1$Euro_LDV, p = "CO" )
#' lefec <- c(lefec,lefec[length(lefec)], lefec[length(lefec)],
#'            lefec[length(lefec)], lefec[length(lefec)],
#'            lefec[length(lefec)])
#' length(lefec) == ncol(pc1)
#' #emis change length of 'ef' to match ncol of 'veh'
#' class(lefec)
#' PC_CO_COLD <- emis_cold(veh = pc1,
#'                         lkm = net$lkm,
#'                         ef = lef,
#'                         efcold = lefec,
#'                         beta = pcf,
#'                         speed = speed,
#'                         profile = pc_profile)
#' class(PC_CO_COLD)
#' plot(PC_CO_COLD)
#' lpc <- list(pc1, pc1)
#' PC_CO_COLDv2 <- emis_cold(veh = pc1,
#'                           lkm = net$lkm,
#'                           ef = lef,
#'                           efcold = lefec,
#'                           beta = pcf,
#'                           speed = speed,
#'                           profile = pc_profile,
#'                           hour = 2,
#'                           day = 1)
#' }
emis_cold <- function (veh, lkm, ef, efcold, beta, speed = 34,
                       agemax = if (!inherits(x = veh, what = "list")) {
                         ncol(veh)
                       } else {
                         ncol(veh[[1]])
                       },
                       profile,
                       simplify = FALSE,
                       hour = nrow(profile),
                       day = ncol(profile),
                       array = TRUE,
                       verbose = FALSE) {
  # Check units
  if(class(lkm) != "units"){
    stop("lkm neeeds to has class 'units' in 'km'. Please, check package 'units'")
  }
  if(units(lkm)$numerator == "m" ){
    stop("Units of lkm is 'm' ")
  }

  # At least, on e of these
  if(any(!class(ef) %in% c("list", "units", "EmissionFactorsList",
                           "EmissionFactors", "data.frame"))){
    stop("ef must be either of 'list', 'units', 'EmissionFactorsList', 'EmissionFactors' or 'data.frame'")
  }

  # Checking sf
  if(any(class(veh) %in% "sf")){
    if(verbose) message("Transforming sf to data.frame")
    veh <- sf::st_set_geometry(veh, NULL)
  }

  lkm <- as.numeric(lkm)
  speed <- as.data.frame(speed)
  for (i  in 1:ncol(speed) ) {
    speed[, i] <- as.numeric(speed[, i])
  }

  # veh is "Vehicles" data-frame
  if (!inherits(x = veh, what = "list")) {
    veh <- as.data.frame(veh)
    lkm <- as.numeric(lkm)
    for(i in 1:ncol(veh)){
      veh[,i] <- as.numeric(veh[,i])
    }

    # top down
    if(missing(profile)){
      stop("For top down approach, use 'emis_cold_td'")
    }

    if(!missing(profile) & is.vector(profile)){
      profile <- matrix(profile, ncol = 1)
    }

    if(ncol(veh) != length(ef)){
      if(verbose) message(
        paste0("Number of columns of 'veh' is different than length of 'ef'\n",
               "adjusting length of ef to the number of colums of 'veh'\n"))
      if(ncol(veh) > length(ef)){
        for(i in (length(ef) + 1):ncol(veh) ){

          # for(i in (ncol(veh) - length(ef)):ncol(veh) ){
          ef[[i]] <- ef[[length(ef)]]
        }
      } else if (ncol(veh) < length(ef)){
        ff <- list()
        for(i in 1:ncol(veh)){
          ff[[i]] <- ef[[i]]
        }
        ef <- ff
      }
    }
    # if(array == F){
    #   lista <- lapply(1:day,function(j){
    #     lapply(1:hour,function(i){
    #       lapply(1:agemax, function(k){
    #         beta[i,j]*veh[, k]*profile[i,j]*lkm*ef[[k]](speed[, i])*
    #           ifelse((efcold[[k]](speed[, i]) - 1) < 0, 0,
    #                  (efcold[[k]](speed[, i]) - 1))
    #       })
    #     })
    #   })
    #   return(EmissionsList(lista))
    # } else {
    veh <- as.data.frame(veh)
    for(i in 1:ncol(veh)){
      veh[,i] <- as.numeric(veh[,i])
    }
    if(simplify) {
      d <-  simplify2array(
        lapply(1:length(unlist(profile)) ,function(j){ # 7 dias
          simplify2array(
            lapply(1:agemax, function(k){ # categorias
              unlist(beta)[j]*veh[, k]*unlist(profile)[j]*lkm*ef[[k]](speed[, j])*
                    ifelse((efcold[[k]](speed[, j])- 1) < 0, 0,
                           (efcold[[k]](speed[, j]) - 1))
        }) ) }) )
  } else {
      d <-  simplify2array(
        lapply(1:day,function(j){
          simplify2array(
            lapply(1:hour,function(i){
              simplify2array(
                lapply(1:agemax, function(k){
                  beta[i,j]*veh[, k]*profile[i,j]*lkm*ef[[k]](speed[, (1:nrow(profile))[i] + nrow(profile)*(j - 1)])*
                    ifelse((efcold[[k]](speed[, (1:nrow(profile))[i] + nrow(profile)*(j - 1)]) - 1) < 0, 0,
                           (efcold[[k]](speed[, (1:nrow(profile))[i] + nrow(profile)*(j - 1)]) - 1))
                }) ) }) ) }) )
    }
    if(verbose) message(round(sum(d, na.rm = TRUE)/1000,2),
                        " kg emissions in ", hour, " hours and ", day, " days")
    return(EmissionsArray(d))
    # }
  } else {
    if(ncol(veh[[1]]) != length(ef)){
      if(verbose) message(
        paste0("Number of columns of 'veh' is different than length of 'ef'\n",
               "adjusting length of ef to the number of colums of 'veh'\n"))
      if(ncol(veh[[1]]) > length(ef)){
        for(i in (length(ef) + 1):ncol(veh[[1]]) ){
          ef[[i]] <- ef[[length(ef)]]
        }
      } else  if (ncol(veh[[1]]) < length(ef)){
        ff <- list()
        for(i in 1:ncol(veh[[1]])){
          ff[[i]] <- ef[[i]]
        }
        ef <- ff
      }

    }
    for (j in 1:length(veh)) {
      for (i  in 1:ncol(veh[[j]]) ) {
        veh[[j]][,i] <- as.numeric(veh[[j]][,i])
      } }
    # if(array == F){
    #   lista <- lapply(1:length(veh),function(i){
    #     lapply(1:agemax, function(k){
    #       unlist(beta)[i]*veh[[i]][, k]*lkm*ef[[k]](speed[, i])*
    #         ifelse((efcold[[k]](speed[, i]) - 1) < 0, 0,
    #                (efcold[[k]](speed[, i]) - 1))
    #     }) })
    #   return(EmissionsList(lista))
    # } else {
    d <-  simplify2array(
      lapply(1:length(veh),function(i){
        simplify2array(
          lapply(1:agemax, function(k){
            unlist(beta)[i]*veh[[i]][, k]*lkm*ef[[k]](speed[, i])*
              ifelse((efcold[[k]](speed[, i]) - 1) < 0, 0,
                     (efcold[[k]](speed[, i]) - 1))
          }) ) }) )
    if(verbose)  message(round(sum(d, na.rm = TRUE)/1000,2),
                         " kg emissions in ", hour, " hours and ", day, " days")
    return(EmissionsArray(d))
    # }
  }
}

