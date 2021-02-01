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
#' @param fortran Logical; to try the fortran calculation when speed is not used.
#' I will add fortran for EmissionFactorsList soon.
#' @param hour Number of considered hours in estimation. Default value is number
#' of rows of argument profile
#' @param day Number of considered days in estimation
#' @param verbose Logical; To show more information
#' @param nt Integer; Number of threads wich must be lower than max available.
#' See \code{\link{check_nt}}. Only when fortran = TRUE
#' @useDynLib  vein, .registration = TRUE
#' @return If the user applies a top-down approach, the resulting units will be
#' according its own data. For instance, if the vehicles are veh/day, the units
#' of the emissions implicitly will be g/day.
#' @note Hour and day will be deprecated because they can be infered from the profile
#' matrix.
#' @export
#' @importFrom sf st_set_geometry
#' @importFrom dotCall64 .C64 numeric_dc
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
#' pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
#'
#' # Estimation for morning rush hour and local emission factors and speed
#' speed <- data.frame(S8 = net$ps)
#' lef <- EmissionFactorsList(ef_cetesb("CO", "PC_G", agemax = ncol(pc1)))
#' system.time(E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed))
#' system.time(E_CO_2 <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed, simplify = TRUE))
#' identical(E_CO, E_CO_2)
#'
#' # Estimation for morning rush hour and local emission factors without speed
#' lef <- ef_cetesb("CO", "PC_G", agemax = ncol(pc1))
#' system.time(E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef))
#' system.time(E_CO_2 <- emis(veh = pc1,lkm = net$lkm, ef = lef, fortran = TRUE))
#' identical(E_CO, E_CO_2)
#'
#' # Estimation for 168 hour and local factors and speed
#' pcw <- temp_fact(net$ldv+net$hdv, pc_profile)
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' lef <- EmissionFactorsList(ef_cetesb("CO", "PC_G", agemax = ncol(pc1)))
#' system.time(
#' E_CO <- emis(veh = pc1,
#'              lkm = net$lkm,
#'              ef = lef,
#'              speed = speed,
#'              profile = profiles$PC_JUNE_2014))
#' system.time(
#' E_CO_2 <- emis(veh = pc1,
#'              lkm = net$lkm,
#'              ef = lef,
#'              speed = speed,
#'              profile = profiles$PC_JUNE_2014,
#'              simplify = TRUE))
#'
#' # Estimation for 168 hour and local factors and without speed
#' lef <- ef_cetesb("CO", "PC_G", agemax = ncol(pc1))
#' system.time(
#' E_CO <- emis(veh = pc1,
#'              lkm = net$lkm,
#'              ef = lef,
#'              profile = profiles$PC_JUNE_2014)) ; sum(E_CO)
#' system.time(
#' E_CO_2 <- emis(veh = pc1,
#'              lkm = net$lkm,
#'              ef = lef,
#'              profile = profiles$PC_JUNE_2014,
#'              fortran = TRUE)) ; sum(E_CO)
#' system.time(
#' E_CO_3 <- emis(veh = pc1,
#'              lkm = net$lkm,
#'              ef = lef,
#'              profile = profiles$PC_JUNE_2014,
#'              simplify = TRUE)) ; sum(E_CO)
#' system.time(
#' E_CO_4 <- emis(veh = pc1,
#'              lkm = net$lkm,
#'              ef = lef,
#'              profile = profiles$PC_JUNE_2014,
#'              simplify = TRUE,
#'              fortran = TRUE)) ; sum(E_CO)
#' identical(round(E_CO, 2), round(E_CO_2, 2))
#' identical(round(E_CO_3, 2), round(E_CO_4, 2))
#' identical(round(E_CO_3[,,1], 2), round(E_CO_4[,,1], 2))
#' dim(E_CO_3)
#' dim(E_CO_4)
#' # but
#' a <- unlist(lapply(1:41, function(i){
#'            unlist(lapply(1:168, function(j) {
#'            identical(E_CO_3[, i, j], E_CO_4[, i, j])
#'            }))}))
#' unique(a)
#'
#' #Estimation with list of vehicles
#' lpc <- list(pc1, pc1)
#' lef <- EmissionFactorsList(ef_cetesb("CO", "PC_G", agemax = ncol(pc1)))
#' E_COv2 <- emis(veh = lpc,lkm = net$lkm, ef = lef, speed = speed)
#'
#' # top down
#' veh <- age_ldv(x = net$ldv[1:4], name = "PC_E25_1400", agemax = 4)
#' mil <- fkm$KM_PC_E25(1:4)
#' ef <- ef_cetesb("COd", "PC_G")[1:4]
#' emis(veh, units::set_units(mil, "km"), ef)
#'
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
#' (a <- emis(veh = bus1, lkm = lkm, ef = efco, verbose = TRUE))
#' (b <- emis(veh = bus1, lkm = lkm, ef = efco, verbose = TRUE, fortran = TRUE))
#' }
emis <- function (veh,
                  lkm,
                  ef,
                  speed,
                  agemax = ifelse(is.data.frame(veh), ncol(veh),
                                  ncol(veh[[1]])),
                  profile,
                  simplify = FALSE,
                  fortran = FALSE,
                  hour = nrow(profile),
                  day = ncol(profile),
                  verbose = FALSE,
                  nt = ifelse(check_nt()==1, 1, check_nt()/2)) {
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
  # veh is "Vehicles" data-frame ####
  if (!inherits(x = veh, what = "list")) {
    veh <- remove_units(veh)

    # top down sin perfil, FE numerico
    if(missing(profile) & is.numeric(ef)){
      if(verbose) message(
        paste0("If this is a top down approach, you may try emis_hot_td\n",
               "With this approach speed is not necessary\n"))

      if(nrow(veh) != length(lkm)) stop("Number of rows of `veh` must be the same as the length of `lkm`` ")

      if(fortran){
        veh <- as.matrix(veh)
        lkm <- as.numeric(lkm)
        ef <- as.numeric(ef)
        nrowv = as.integer(nrow(veh))
        ncolv = as.integer(ncol(veh))

        if(length(lkm) != nrow(veh)) stop("length of `lkm` must be equal to number of rows of `veh`")
        if(length(ef) != ncol(veh)) stop("length of `ef` and number of cols of `veh` must be equal")
        # emis(i, j) = veh(i,j) * lkm(i) * ef(j)
        if(!missing(nt)) {
          if(nt >= check_nt()) stop("Your machine has ", check_nt(),
                                    " threads and nt must be lower")

          if(verbose) message("Calling emis2dfpar.f95")
          a <- dotCall64::.C64(
            .NAME = "emis2dfpar",
            SIGNATURE = c("integer", "integer",
                          "double", "double", "double",
                          "integer", "double"),
            nrowv = nrowv,
            ncolv = ncolv,
            veh = veh,
            lkm = lkm,
            ef = ef,
            nt = as.integer(nt),
            emis = dotCall64::numeric_dc(nrowv*ncolv),
            INTENT = c("r", "r",
                       "r", "r","r",
                       "r","w"),
            PACKAGE = "vein",
            VERBOSE = 1)$emis

        } else {
          if(verbose) message("Calling emis2df.f95")
          a <- dotCall64::.C64(
            .NAME = "emis2df",
            SIGNATURE = c("integer", "integer",
                          "double", "double", "double",
                          "double"),
            nrowv = nrowv,
            ncolv = ncolv,
            veh = veh,
            lkm = lkm,
            ef = ef,
            emis = dotCall64::numeric_dc(nrowv*ncolv),
            INTENT = c("r", "r",
                       "r", "r","r",
                       "w"),
            PACKAGE = "vein",
            VERBOSE = 1)$emis
        }

        e <- matrix(a, nrow = nrowv, ncol =  ncolv)
        return(Emissions(e))
      } else {
        a <- lapply(1:ncol(veh), function(i){
          veh[, i] * as.numeric(lkm) * as.numeric(ef)[i]
        })
        a <- Emissions(do.call("cbind", a))
        return(a)
      }

      # top down sin perfil, FE lista y con velocidad
    } else if(missing(profile) & class(ef)[1] == "EmissionFactorsList"){
      #Check speed
      if(missing(speed)) stop("Add speed to be read by the EmissionFactorsList")
      speed <- remove_units(speed)

      if(nrow(veh) != length(lkm)) stop("Number of rows of `veh` must be the same as the length of `lkm`")
      if(nrow(veh) != length(unlist(speed))) stop("Number of rows of `veh` must be the same rows of `speed`")

      if(fortran) message("Not implemented yet when ef is 'EmissionsFactorsList`, changing to default")
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
      speed <- remove_units(speed)

      # simplify?
      if(simplify) {
        if(fortran) message("Not implemented yet when ef is 'EmissionsFactorsList`, changing to default")
        d <-  simplify2array(
          lapply(1:length(unlist(profile)) ,function(j){ # 7 dias
            simplify2array(
              lapply(1:agemax, function(k){ # categorias
                veh[, k]*unlist(profile)[j]*lkm*ef[[k]](speed[, j])
              }) ) }) )


      } else {
        if(fortran) message("Not implemented yet when ef is 'EmissionsFactorsList`, changing to default")
        d <-  simplify2array(
          lapply(1:ncol(profile),function(j){ # 7 dias
            simplify2array(
              lapply(1:nrow(profile),function(i){ # 24 horas
                simplify2array(
                  lapply(1:agemax, function(k){ # categorias
                    veh[, k]*profile[i,j]*lkm*ef[[k]](speed[, (1:nrow(profile))[i] + nrow(profile)*(j - 1)])
                  }) ) }) ) }) )
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
        if(fortran) {
          veh <- as.matrix(veh)
          lkm <- as.numeric(lkm)
          ef <- as.numeric(ef)
          profile <- as.numeric(unlist(profile))
          nrowv <- as.integer(nrow(veh))
          ncolv <- as.integer(ncol(veh))
          prok <- as.integer(length(unlist(profile)))

          if(length(lkm) != nrow(veh)) stop("length of `lkm` must be equal to number of rows of `veh`")
          if(length(ef) != ncol(veh)) stop("length of `ef` and number of cols of `veh` must be equal")
          # emis(i, j, k) = veh(i, j) * lkm(i) * ef(j)*pro(k)

          if(!missing(nt)) {
            if(nt >= check_nt()) stop("Your machine has ", check_nt(),
                                      " threads and nt must be lower")

            if(verbose) message("Calling emis3dfpar.f95")
            a <-   dotCall64::.C64(
              .NAME = "emis3dfpar",
              SIGNATURE = c("integer", "integer","integer",
                            "double", "double", "double","double",
                            "integer", "double"),
              nrowv = nrowv,
              ncolv = ncolv,
              prok = prok,
              veh = veh,
              lkm = lkm,
              ef = ef,
              pro = profile,
              nt = as.integer(nt),
              emis = dotCall64::numeric_dc(nrowv*ncolv*prok),
              INTENT = c("r", "r", "r",
                         "r", "r", "r", "r",
                         "r","w"),
              PACKAGE = "vein",
              VERBOSE = 1)$emis

          } else {

            if(verbose) message("Calling emis3df.f95")
            a <-   dotCall64::.C64(
              .NAME = "emis3df",
              SIGNATURE = c("integer", "integer","integer",
                            "double", "double", "double","double",
                            "double"),
              nrowv = nrowv,
              ncolv = ncolv,
              prok = prok,
              veh = veh,
              lkm = lkm,
              ef = ef,
              pro = profile,
              emis = numeric(nrowv*ncolv*prok),
              INTENT = c("r", "r", "r",
                         "r", "r", "r", "r",
                         "w"),
              PACKAGE = "vein",
              VERBOSE = 1)$emis
          }
          # fortran
          # do concurrent(i= 1:nrowv, j = 1:ncolv, k = 1:prok)
          # emis(i, j,k) = veh(i,j) * lkm(i) * ef(j)*pro(k)
          # end do
          e <- array(a, dim = c(nrowv, ncolv,prok))
          return(EmissionsArray(e))
        } else {
          vkm <- veh*lkm
          vkmef <- t(t(vkm) * ef)
          d <- simplify2array(lapply(unlist(profile), "*", vkmef))
        }

      } else {
        if(fortran){
          veh <- as.matrix(veh)
          profile <- as.matrix(profile)
          lkm <- as.numeric(lkm)
          ef <- as.numeric(ef)
          nrowv <- as.integer(nrow(veh))
          ncolv <- as.integer(ncol(veh))
          proh <- as.integer(nrow(profile))
          prod <- as.integer(ncol(profile))


          if(length(lkm) != nrow(veh)) stop("length of `lkm` must be equal to number of rows of `veh`")
          if(length(ef) != ncol(veh)) stop("length of `ef` and number of cols of `veh` must be equal")
          # emis(i, j, k, l) = veh(i,j) * lkm(i) * ef(j)*pro(k,l)

          if(!missing(nt)) {

            if(nt >= check_nt()) stop("Your machine has ", check_nt(),
                                      " threads and nt must be lower")

            if(verbose) message("Calling emis4dfpar.f95")
            a <-   dotCall64::.C64(
              .NAME = "emis4dfpar",
              SIGNATURE = c("integer", "integer", "integer", "integer",
                            "double", "double", "double", "double",
                            "integer", "double"),
              nrowv = nrowv,
              ncolv = ncolv,
              proh = proh,
              prod = prod,
              veh = veh,
              lkm = lkm,
              ef = ef,
              pro = profile,
              nt = as.integer(nt),
              emis = dotCall64::numeric_dc(nrowv*ncolv*proh*prod),
              INTENT = c("r", "r", "r", "r",
                         "r", "r", "r", "r",
                         "r","w"),
              PACKAGE = "vein",
              VERBOSE = 1)$emis
          } else {
            if(verbose) message("Calling emis4df.f95")
            a <-   dotCall64::.C64(
              .NAME = "emis4df",
              SIGNATURE = c("integer", "integer", "integer", "integer",
                            "double", "double", "double", "double",
                            "double"),
              nrowv = nrowv,
              ncolv = ncolv,
              proh = proh,
              prod = prod,
              veh = veh,
              lkm = lkm,
              ef = ef,
              pro = profile,
              emis = dotCall64::numeric_dc(nrowv*ncolv*proh*prod),
              INTENT = c("r", "r", "r", "r",
                         "r", "r", "r", "r",
                         "w"),
              PACKAGE = "vein",
              VERBOSE = 1)$emis

          }
          e <- array(a, dim = c(nrowv, ncolv,proh, prod))
          return(EmissionsArray(e))

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

