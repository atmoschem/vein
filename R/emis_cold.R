#' Estimation of cold start emissions hourly for the of the week
#'
#' @description \code{emis_cold} emissions are estimated as the product of the
#' vehicles on a road, length of the road, emission factor evaluated at the
#' respective speed. The estimation considers the beta parameter, the fraction of
#' mileage driven
#'
#' @param veh "Vehicles" data-frame or list of "Vehicles" data-frame. Each data-frame
#' as number of columns matching the age distribution of that type of vehicle.
#' The number of rows is equal to the number of streets link
#' @param lkm Length of each link with units of length (class 'units'),
#' converted to km.
#' @param ef List of functions of emission factors of vehicular categories
#' @param efcold List of functions of cold start emission factors of vehicular
#' categories. As `ef`, when it is shorter than the number of columns of `veh`,
#' the last function is repeated.
#' @param beta Dataframe with the hourly cold-start distribution to each day
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
emis_cold <- function(veh,
                      lkm,
                      ef,
                      efcold,
                      beta,
                      speed = 34,
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
  # lkm can have any length unit, it is converted to km
  lkm <- vein_as_km(lkm, "lkm")

  # At least, on e of these
  if (any(!class(ef) %in% c("list", "units", "EmissionFactorsList",
    "EmissionFactors", "data.frame"))) {
    stop("ef must be either of 'list', 'units', 'EmissionFactorsList', 'EmissionFactors' or 'data.frame'")
  }

  # Checking sf
  if (inherits(veh, "sf")) {
    if (verbose) message("Transforming sf to data.frame")
    veh <- sf::st_set_geometry(veh, NULL)
  }

  speed <- as.data.frame(remove_units(speed))
  for (i  in 1:ncol(speed)) {
    speed[, i] <- as.numeric(speed[, i])
  }
  beta <- as.numeric(unlist(beta))

  # cold factor: efcold/ef - 1, without negative values
  coldf <- function(x) {
    x <- as.numeric(x) - 1
    ifelse(x < 0, 0, x)
  }

  # veh is "Vehicles" data-frame
  if (!inherits(x = veh, what = "list")) {
    veh <- as.data.frame(remove_units(veh))
    for (i in 1:ncol(veh)) {
      veh[, i] <- as.numeric(veh[, i])
    }

    # top down
    if (missing(profile)) {
      stop("For top down approach, use 'emis_cold_td'")
    }
    profile <- remove_units(profile)
    if (is.vector(profile)) {
      profile <- matrix(as.numeric(profile), ncol = 1)
    } else {
      profile <- as.matrix(profile)
    }
    nh <- nrow(profile)
    nd <- ncol(profile)

    if (agemax > ncol(veh)) {
      stop("'agemax' (", agemax, ") cannot be higher than the number of columns of 'veh' (", ncol(veh), ")")
    }
    nr <- nrow(veh)
    nc <- agemax
    if (length(lkm) == 1 && nr > 1) lkm <- rep(lkm, nr)
    if (nr != length(lkm)) stop("Number of rows of `veh` must be the same as the length of `lkm`")

    ef <- vein_adjust_eflist(ef, ncol(veh), verbose)
    efcold <- vein_adjust_eflist(efcold, ncol(veh), verbose)

    # speed is passed unevaluated to the ef functions, as many of them
    # (e.g. constant emission factors) do not use it
    if (simplify) {
      pro <- as.numeric(profile)
      d <- array(NA_real_, dim = c(nr, nc, length(pro)))
      for (j in seq_along(pro)) { # hours x days
        for (k in seq_len(nc)) { # categorias
          d[, k, j] <- beta[j] * veh[, k] * pro[j] * lkm *
            as.numeric(ef[[k]](speed[, j])) *
            coldf(efcold[[k]](speed[, j]))
        }
      }
    } else {
      d <- array(NA_real_, dim = c(nr, nc, nh, nd))
      for (j in seq_len(nd)) { # days
        for (i in seq_len(nh)) { # hours
          for (k in seq_len(nc)) { # categorias
            d[, k, i, j] <- beta[i + nh * (j - 1)] * veh[, k] * profile[i, j] * lkm *
              as.numeric(ef[[k]](speed[, i + nh * (j - 1)])) *
              coldf(efcold[[k]](speed[, i + nh * (j - 1)]))
          }
        }
      }
    }
    if (verbose) message(round(sum(d, na.rm = TRUE) / 1000, 2),
      " kg emissions in ", nh, " hours and ", nd, " days")
    return(EmissionsArray(d))
  } else {
    for (j in seq_along(veh)) {
      veh[[j]] <- as.data.frame(remove_units(veh[[j]]))
      for (i  in 1:ncol(veh[[j]])) {
        veh[[j]][, i] <- as.numeric(veh[[j]][, i])
      }
    }
    if (agemax > ncol(veh[[1]])) {
      stop("'agemax' (", agemax, ") cannot be higher than the number of columns of 'veh' (", ncol(veh[[1]]), ")")
    }
    nr <- nrow(veh[[1]])
    nc <- agemax
    if (length(lkm) == 1 && nr > 1) lkm <- rep(lkm, nr)
    if (nr != length(lkm)) stop("Number of rows of `veh` must be the same as the length of `lkm`")
    ef <- vein_adjust_eflist(ef, ncol(veh[[1]]), verbose)
    efcold <- vein_adjust_eflist(efcold, ncol(veh[[1]]), verbose)

    d <- array(NA_real_, dim = c(nr, nc, length(veh)))
    for (i in seq_along(veh)) {
      for (k in seq_len(nc)) {
        d[, k, i] <- beta[i] * veh[[i]][, k] * lkm *
          as.numeric(ef[[k]](speed[, i])) *
          coldf(efcold[[k]](speed[, i]))
      }
    }
    if (verbose) message(round(sum(d, na.rm = TRUE) / 1000, 2),
      " kg emissions in ", length(veh), " hours")
    return(EmissionsArray(d))
  }
}
