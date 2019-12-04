#' Estimation of emissions
#'
#' @description \code{\link{emis}} estimates vehicular emissions as the product of the
#' vehicles on a road, length of the road, emission factor avaliated at the
#' respective speed. \eqn{E = VEH*LENGTH*EF(speed)}
#'
#' @param veh "Vehicles" data-frame or list of "Vehicles" data-frame. Each data-frame
#' as number of columns matching the age distribution of that ype of vehicle.
#' The number of rows is equal to the number of streets link. If this is a list,
#' the length of the list is the vehicles for each hour.
#' @param lkm Length of each link in km
#' @param ef List of functions of emission factors
#' @param speed Speed data-frame with number of columns as hours. The default value is 34km/h
#' @param agemax Age of oldest vehicles for that category
#' @param profile Dataframe or Matrix with nrows equal to 24 and ncol 7 day of
#' the week
#' @param simplify Logical; to determine if EmissionsArray should les dimensions,
#' being streets, vehicle categories and hours or default (streets, vehicle
#' categories, hours and days). Default is FALSE to avoid break old code, but
#' the recommendation is that new estimations use this parameter as TRUE
#' @param hour Number of considered hours in estimation. Default value is number
#' of rows of argument profile
#' @param day Number of considered days in estimation
#' @param array Deprecated! \code{\link{emis_cold}} returns only arrays.
#' When TRUE and veh is not a list, expects a profile as a dataframe producing
#' an array with dimensions (streets x columns x hours x days)
#' @param verbose Logical; To show more information
#' @return If the user applies a top-down approach, the resulting units will be
#' according its own data. For instance, if the vehicles are veh/day, the units
#' of the emissions implicitly will be g/day.
#' @note Hour and day will be deprecated because they can be infered from the profile
#' matrix.
#' @export
#' @importFrom sf st_set_geometry
#' @examples \dontrun{
#' # Do not run
#' data(net)
#' data(pc_profile)
#' data(profiles)
#' data(fe2015)
#' data(fkm)
#' PC_G <- c(33491,22340,24818,31808,46458,28574,24856,28972,37818,49050,87923,
#'           133833,138441,142682,171029,151048,115228,98664,126444,101027,
#'           84771,55864,36306,21079,20138,17439, 7854,2215,656,1262,476,512,
#'           1181, 4991, 3711, 5653, 7039, 5839, 4257,3824, 3068)
#' veh <- data.frame(PC_G = PC_G)
#' pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
#' # Estimation for morning rush hour and local emission factors
#' speed <- data.frame(S8 = net$ps)
#' lef <- EmissionFactorsList(fe2015[fe2015$Pollutant=="CO", "PC_G"])
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed,
#'              profile = 1)
#'
#' # Estimation for 168 hour and local factors
#' pcw <- temp_fact(net$ldv+net$hdv, pc_profile)
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' lef <- EmissionFactorsList(fe2015[fe2015$Pollutant=="CO", "PC_G"])
#'
#' E_CO <- emis(veh = pc1,
#'              lkm = net$lkm,
#'              ef = lef,
#'              speed = speed,
#'              profile = profiles$PC_JUNE_2014)
#' summary(E_CO)
#' lpc <- list(pc1, pc1)
#' E_COv2 <- emis(veh = lpc,lkm = net$lkm, ef = lef, speed = speed)
#' # top down
#' veh <- age_ldv(x = net$ldv[1:4], name = "PC_E25_1400", agemax = 4)
#' mil <- fkm$KM_PC_E25(1:4)
#' ef <- ef_cetesb("COd", "PC_G")[1:4]
#' emis(veh, units::set_units(mil, "km"), ef)
#' # group online
#' bus1 <- age_hdv(30, agemax = 4)
#' veh = bus1
#' lkm = units::set_units(400, "km")
#' speed = 40
#' efco <- ef_cetesb("COd", "UB", agemax = 4)
#' lef <- ef_hdv_scaled(dfcol = as.numeric(efco),
#'                      v = "Ubus",
#'                      t = "Std",
#'                      g = ">15 & <=18",
#'                      eu = rep("IV", 4),
#'                      gr = 0,
#'                      l = 0.5,
#'                      p = "CO")
#' for(i in 1:length(lef)) print(lef[[i]](10))
#' emis(veh = bus1, lkm = lkm, ef = efco, verbose = T)
#' }
emis <- function (veh,
                  lkm,
                  ef,
                  speed,
                  agemax = ifelse(is.data.frame(veh), ncol(veh),
                                  ncol(veh[[1]])),
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
    stop("Units of lkm is 'm'. Please, check package '?units::set_units'")
  }
  # At least, on e of these
  if(any(!class(ef) %in% c("list", "units", "EmissionFactorsList",
                           "EmissionFactors", "data.frame"))){
    stop("ef must be either of 'list', 'units', 'EmissionFactorsList', 'EmissionFactors' or 'data.frame'")
  }
  # Checking sf
  if(any(class(veh) %in% "sf")){
    if(verbose) message("Converting sf to data.frame")
    veh <- sf::st_set_geometry(veh, NULL)
  }
  if(!missing(hour) | !missing(day)){
    warning("Arguments hour and day will be deprecated, they will derived from profile")
  }

  lkm <- as.numeric(lkm)
  # veh is "Vehicles" data-frame
  if (!inherits(x = veh, what = "list")) {
    veh <- as.data.frame(veh)
    for (i  in 1:ncol(veh) ) {
      veh[,i] <- as.numeric(veh[,i])
    }


    # top down sin perfil, FE numerico
    if(missing(profile) & is.numeric(ef)){
      if(verbose) message(
        paste0("If this is a top down approach, you may try emis_hot_td\n",
               "With this approach speed is not necessary\n"))
      if(nrow(veh) != length(lkm)) stop("Number of rows of `veh` must be the same as the length of `lkm`` ")
      a <- lapply(1:ncol(veh), function(i){
        veh[, i] * as.numeric(lkm) * as.numeric(ef)[i]
      })
      a <- Emissions(do.call("cbind", a))
      return(a)


      # top down sin perfil, FE lista y con velocidad
    } else if(missing(profile) & class(ef)[1] == "EmissionFactorsList"){
      #Check speed
      if(missing(speed)) stop("Add speed to be read by the EmissionFactorsList")
      speed <- as.data.frame(speed)
      for (i  in 1:ncol(speed) ) {
        speed[, i] <- as.numeric(speed[, i])
      }
      if(nrow(veh) != length(lkm)) stop("Number of rows of `veh` must be the same as the length of `lkm`")
      if(nrow(veh) != length(unlist(speed))) stop("Number of rows of `veh` must be the same rows of `speed`")

      a <- lapply(1:ncol(veh), function(i){
        veh[, i] * as.numeric(lkm) * ef[[i]](speed)
      })
      a <- Emissions(do.call("cbind", a))
      return(a)

    }


    #bottom up
    if(!missing(profile) & is.vector(profile)){
      profile <- matrix(profile, ncol = 1)
    }



    if(inherits(x = ef, what = "list")) {
      if(verbose) message("Emission factors inherits from list")

      # Check length
      if(ncol(veh) != length(ef)){
        if(verbose) message(
          paste0("Number of columns of 'veh' is different than length of 'ef'\n",
                 "adjusting length of ef to the number of colums of 'veh'\n"))
        if(ncol(veh) > length(ef)){
          for(i in (length(ef) + 1):ncol(veh) ){
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
      #Check speed
      if(missing(speed)) stop("Add speed to be read by the EmissionFactorsList")
      speed <- as.data.frame(speed)
      for (i  in 1:ncol(speed) ) {
        speed[, i] <- as.numeric(speed[, i])
      }

      # simplify?
      if(simplify) {
        d <-  simplify2array(
          lapply(1:length(unlist(profile)) ,function(j){ # 7 dias
            simplify2array(
              lapply(1:agemax, function(k){ # categorias
                veh[, k]*unlist(profile)[j]*lkm*ef[[k]](speed[, j])
              }) ) }) )

      } else {
        d <-  simplify2array(
          lapply(1:ncol(profile),function(j){ # 7 dias
            simplify2array(
              lapply(1:nrow(profile),function(i){ # 24 horas
                simplify2array(
                  lapply(1:agemax, function(k){ # categorias
                    veh[, k]*profile[i,j]*lkm*ef[[k]](speed[, (1:nrow(profile))[i] + nrow(profile)*(j - 1)])
                  }) ) }) ) }) )
        # checked with for(j in 1:7) for(i in 1:24) print((1:24)[i] + 24*(j - 1))
      }

    } else {
      if(verbose) message("Emission factors does not inherits from list")

      # Check length
      if(ncol(veh) != length(ef)){
        if(verbose) message(
          paste0("Number of columns of 'veh' is different than length of 'ef'\n",
                 "adjusting length of ef to the number of colums of 'veh'\n"))
        if(ncol(veh) > length(ef)){
          for(i in (length(ef) + 1):ncol(veh) ){
            ef[[i]] <- ef[[length(ef)]]
          }
        } else if (ncol(veh) < length(ef)){
          ff <- list()
          for(i in 1:ncol(veh)){
            ff[[i]] <- ef[i]
          }
          ef <- unlist(ff)
        }
      }
      #Check speed
      if(!missing(speed)) warning("When ef is not EmissionFactorsList, speed is not needed")

      # simplify?
      if(simplify) {
        vkm <- veh*lkm
        vkmef <- t(t(vkm) * ef)
        d <- simplify2array(lapply(unlist(profile), "*", vkmef))

      } else {


        d <-  simplify2array(
          lapply(1:ncol(profile),function(j){ # 7 dias
            simplify2array(
              lapply(1:nrow(profile),function(i){ # 24 horas
                simplify2array(
                  lapply(1:agemax, function(k){ # categorias
                    veh[, k]*profile[i,j]*lkm*ef[k]
                  }) ) }) ) }) )
        # checked with for(j in 1:7) for(i in 1:24) print((1:24)[i] + 24*(j - 1))
      }
    }

    if(verbose) message(round(sum(d, na.rm = T)/1000,2), " kg emissions")
    return(EmissionsArray(d))








    # veh is a list of "Vehicles" data-frames
    # each member of the list is an hour
  } else {
    if(ncol(veh[[1]]) != length(ef)){
      #check speed
      speed <- as.data.frame(speed)
      for (i  in 1:ncol(speed) ) {
        speed[, i] <- as.numeric(speed[, i])
      }

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
    #       veh[[i]][, k]*lkm*ef[[k]](speed[, i])
    #     } ) } )
    #   return(EmissionsList(lista))
    # } else {
    d <-  simplify2array(
      lapply(1:length(veh),function(i){
        simplify2array(
          lapply(1:agemax, function(k){
            veh[[i]][, k]*lkm*ef[[k]](speed[, i])
          }) ) }) )
    if(verbose) message(round(sum(d, na.rm = T)/1000,2), " kg emissions ")
    return(EmissionsArray(d))
    # }
  }
}

