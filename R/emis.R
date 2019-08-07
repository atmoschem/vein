#' Estimation of emissions
#'
#' @description \code{\link{emis}} estimates vehicular emissions as the product of the
#' vehicles on a road, length of the road, emission factor avaliated at the
#' respective speed. \eqn{E = VEH*LENGTH*EF(speed)}
#'
#' @param veh "Vehicles" data-frame or list of "Vehicles" data-frame. Each data-frame
#' as number of columns matching the age distribution of that ype of vehicle.
#' The number of rows is equal to the number of streets link
#' @param lkm Length of each link in km
#' @param ef List of functions of emission factors
#' @param speed Speed data-frame with number of columns as hours. The default value is 34km/h
#' @param agemax Age of oldest vehicles for that category
#' @param profile Dataframe or Matrix with nrows equal to 24 and ncol 7 day of
#' the week
#' @param hour Number of considered hours in estimation. Default value is number
#' of rows of argument profile
#' @param day Number of considered days in estimation
#' @param array When FALSE produces a dataframe of the estimation. When TRUE expects a
#' profile as a dataframe producing an array with dimensions (streets x columns x hours x days)
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
#' # Estimation for 168 hour and local factors
#' pcw <- temp_fact(net$ldv+net$hdv, pc_profile)
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' lef <- EmissionFactorsList(fe2015[fe2015$Pollutant=="CO", "PC_G"])
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed,
#'              profile = profiles$PC_JUNE_2014)
#' summary(E_CO)
#' # Estimation for 168 hour and COPERT factors
#' pcw <- temp_fact(net$ldv+net$hdv, pc_profile)
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' euro <- as.character(fe2015[fe2015$Pollutant=="CO", "Euro_LDV"])
#' lef <- lapply(1:length(euro), function(i) {
#' ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G", p = "CO",
#'              eu= euro[i], show.equation = FALSE)
#' })
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed,
#'              profile = profiles$PC_JUNE_2014)
#' # Estimation for 168 hour and scaled factors
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
#' length(lef) != ncol(pc1)
#' #emis change length of 'ef' to match ncol of 'veh'
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed,
#'              profile = profiles$PC_JUNE_2014)
#' class(E_CO)
#' lpc <- list(pc1, pc1)
#' E_COv2 <- emis(veh = lpc,lkm = net$lkm, ef = lef, speed = speed,
#'                hour = 2, day = 1)
#' # Entering wrong results
#' pc1[ , ncol(pc1) + 1] <- pc1$PC_1
#' dim(pc1)
#' length(lef)
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed,
#'              profile = profiles$PC_JUNE_2014)
#' E_COv2 <- emis(veh = lpc,lkm = net$lkm, ef = lef, speed = speed,
#'                hour = 2, day = 1)
#' # top down
#' veh <- age_ldv(x = net$ldv[1:2], name = "PC_E25_1400", agemax = 4)
#' mil <- fkm$KM_PC_E25(1:4)
#' ef <- ef_cetesb("COd", "PC_G")[1:4]
#' emis(veh, lkm, ef)
#' }
emis <- function (veh,
                  lkm,
                  ef,
                  speed = 34,
                  agemax = ifelse(is.data.frame(veh), ncol(veh),
                                  ncol(veh[[1]])),
                  profile,
                  hour = nrow(profile),
                  day = ncol(profile),
                  array = T,
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
    if(verbose) message("converting sf to data.frame")
    veh <- sf::st_set_geometry(veh, NULL)
  }
  if(!missing(hour) | !missing(day)){
    warning("Arguments hour and day will be deprecated, they will derived from profile")
  }

  lkm <- as.numeric(lkm)
  speed <- as.data.frame(speed)
  for (i  in 1:ncol(speed) ) {
    speed[, i] <- as.numeric(speed[, i])
  }
  # veh is "Vehicles" data-frame
  if (!inherits(x = veh, what = "list")) {
    veh <- as.data.frame(veh)
    for (i  in 1:ncol(veh) ) {
      veh[,i] <- as.numeric(veh[,i])
    }
    # top down
    if(missing(profile) & missing(speed)){
   if(verbose) message("If this is a top down approach try emis_hot_td ")
      if(nrow(veh) != length(lkm)) stop("number of rows of `veh` must be the same as the length of `lkm`` ")
      a <- lapply(1:ncol(veh), function(i){
        veh[, i] * as.numeric(lkm) * as.numeric(ef)[i]
      })
      a <- Emissions(do.call("cbind", a))
      return(a)
    } else if(missing(profile) & !missing(speed)){
      if(verbose) message("Speed functions with `ef` EmissionFactorsList")
      if(nrow(veh) != length(lkm)) stop("number of rows of `veh` must be the same as the length of `lkm`")
      if(nrow(veh) != length(speed)) stop("number of rows of `veh` must be the same as the length of `speed`")
      if(!is.list(ef)) stop("`ef` must be EmissionFactorsList, or a list of speed functions")
      a <- lapply(1:ncol(veh), function(i){
        veh[, i] * as.numeric(lkm) * ef[[i]](speed)
      })
      a <- Emissions(do.call("cbind", a))
      return(a)

    }

    if(!missing(profile) & is.data.frame(profile)){
      profile <- profile
    } else if(!missing(profile) & is.matrix(profile)){
      profile <- profile
    } else if(!missing(profile) & is.vector(profile)){
      profile <- matrix(profile, ncol = 1)
    }

    if(ncol(veh) != length(ef)){
      if(verbose) message("Number of columns of 'veh' is different than length of 'ef'")
      if(verbose) message("adjusting length of ef to the number of colums of 'veh'\n")
      if(ncol(veh) > length(ef)){
        for(i in (length(ef) + 1):ncol(veh) ){
          ef[[i]] <- ef[[length(ef)]]
        }
        if (ncol(veh) < length(ef)){
          ff <- list()
          for(i in 1:ncol(veh)){
            ff[[i]] <- ef[[i]]
          }
          ef <- ff
        }
      }
    }

    if(array == F){
      lista <- lapply(1:ncol(profile),function(j){
        lapply(1:nrow(profile),function(i){
          lapply(1:agemax, function(k){
            veh[, k]*profile[i,j]*lkm*ef[[k]](speed[, i])
          }) }) })
      return(EmissionsList(lista))
    } else {

      d <-  simplify2array(
        lapply(1:ncol(profile),function(j){
          simplify2array(
            lapply(1:nrow(profile),function(i){
              simplify2array(
                lapply(1:agemax, function(k){
                  veh[, k]*profile[i,j]*lkm*ef[[k]](speed[, i*j])
                }) ) }) ) }) )
      if(verbose) message(round(sum(d, na.rm = T)/1000,2), " kg emissions")
      return(EmissionsArray(d))
    }
    # veh is a list of "Vehicles" data-frames
  } else {
    if(ncol(veh[[1]]) != length(ef)){
      if(verbose) message("Number of columns of 'veh' is different than length of 'ef'")
      if(verbose) message("adjusting length of ef to the number of colums of 'veh'\n")
      if(ncol(veh[[1]]) > length(ef)){
        for(i in (length(ef) + 1):ncol(veh[[1]]) ){
          ef[[i]] <- ef[[length(ef)]]
        }
        if (ncol(veh[[1]]) < length(ef)){
          ff <- list()
          for(i in 1:ncol(veh[[1]])){
            ff[[i]] <- ef[[i]]
          }
          ef <- ff
        }
      }
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
      if(verbose) message(round(sum(d, na.rm = T)/1000,2), " kg emissions ")
      return(EmissionsArray(d))
    }
  }
}

