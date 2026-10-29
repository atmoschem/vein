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
#' @param lkm Length of each link with units of length (class 'units'). It is
#' converted to km with \code{units::set_units}.
#' @param ef List of functions of emission factors ("EmissionFactorsList"),
#' or numeric emission factors with one value for each column of `veh`.
#' Emission factors with units are converted to 'g/km'. When `ef` is a
#' data.frame with the same number of rows of `veh`, it is applied by street
#' (only in the R calculation). When the length of `ef` is lower than the
#' number of columns of `veh`, the last emission factor is repeated, also in
#' the top-down approach.
#' @param speed Speed data-frame with number of columns as hours. The default value is 34km/h
#' @param agemax Age of oldest vehicles for that category. Only the first
#' `agemax` columns of `veh` are used, in the R and fortran calculations.
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
#' See \code{\link{check_nt}}. Only when fortran = TRUE. The fortran
#' calculation supports outputs up to \code{.Machine$integer.max} elements.
#' @useDynLib  vein, .registration = TRUE
#' @return If the user applies a top-down approach, the resulting units will be
#' according its own data. For instance, if the vehicles are veh/day, the units
#' of the emissions implicitly will be g/day. The EmissionsArray always has
#' the dimensions streets x categories x hours (x days), also when there is
#' only one street.
#' @export
#' @importFrom sf st_set_geometry
#' @importFrom dotCall64 .C64 vector_dc
#' @examples
#' \dontrun{
#' # Do not run
#' data(net)
#' data(pc_profile)
#' data(profiles)
#' data(fe2015)
#' data(fkm)
#' PC_G <- c(
#'   33491, 22340, 24818, 31808, 46458, 28574, 24856, 28972, 37818, 49050, 87923,
#'   133833, 138441, 142682, 171029, 151048, 115228, 98664, 126444, 101027,
#'   84771, 55864, 36306, 21079, 20138, 17439, 7854, 2215, 656, 1262, 476, 512,
#'   1181, 4991, 3711, 5653, 7039, 5839, 4257, 3824, 3068
#' )
#' pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
#'
#' # Estimation for morning rush hour and local emission factors and speed
#' speed <- data.frame(S8 = net$ps)
#' lef <- EmissionFactorsList(ef_cetesb("CO", "PC_G", agemax = ncol(pc1)))
#' system.time(E_CO <- emis(veh = pc1, lkm = net$lkm, ef = lef, speed = speed))
#' system.time(E_CO_2 <- emis(veh = pc1, lkm = net$lkm, ef = lef, speed = speed, simplify = TRUE))
#' identical(E_CO, E_CO_2)
#'
#' # Estimation for morning rush hour and local emission factors without speed
#' lef <- ef_cetesb("CO", "PC_G", agemax = ncol(pc1))
#' system.time(E_CO <- emis(veh = pc1, lkm = net$lkm, ef = lef))
#' system.time(E_CO_2 <- emis(veh = pc1, lkm = net$lkm, ef = lef, fortran = TRUE))
#' identical(E_CO, E_CO_2)
#'
#' # Estimation for 168 hour and local factors and speed
#' pcw <- temp_fact(net$ldv + net$hdv, pc_profile)
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' lef <- EmissionFactorsList(ef_cetesb("CO", "PC_G", agemax = ncol(pc1)))
#' system.time(
#'   E_CO <- emis(
#'     veh = pc1,
#'     lkm = net$lkm,
#'     ef = lef,
#'     speed = speed,
#'     profile = profiles$PC_JUNE_2014
#'   )
#' )
#' system.time(
#'   E_CO_2 <- emis(
#'     veh = pc1,
#'     lkm = net$lkm,
#'     ef = lef,
#'     speed = speed,
#'     profile = profiles$PC_JUNE_2014,
#'     simplify = TRUE
#'   )
#' )
#'
#' # Estimation for 168 hour and local factors and without speed
#' lef <- ef_cetesb("CO", "PC_G", agemax = ncol(pc1))
#' system.time(
#'   E_CO <- emis(
#'     veh = pc1,
#'     lkm = net$lkm,
#'     ef = lef,
#'     profile = profiles$PC_JUNE_2014
#'   )
#' )
#' sum(E_CO)
#' system.time(
#'   E_CO_2 <- emis(
#'     veh = pc1,
#'     lkm = net$lkm,
#'     ef = lef,
#'     profile = profiles$PC_JUNE_2014,
#'     fortran = TRUE
#'   )
#' )
#' sum(E_CO)
#' system.time(
#'   E_CO_3 <- emis(
#'     veh = pc1,
#'     lkm = net$lkm,
#'     ef = lef,
#'     profile = profiles$PC_JUNE_2014,
#'     simplify = TRUE
#'   )
#' )
#' sum(E_CO)
#' system.time(
#'   E_CO_4 <- emis(
#'     veh = pc1,
#'     lkm = net$lkm,
#'     ef = lef,
#'     profile = profiles$PC_JUNE_2014,
#'     simplify = TRUE,
#'     fortran = TRUE
#'   )
#' )
#' sum(E_CO)
#' identical(round(E_CO, 2), round(E_CO_2, 2))
#' identical(round(E_CO_3, 2), round(E_CO_4, 2))
#' identical(round(E_CO_3[, , 1], 2), round(E_CO_4[, , 1], 2))
#' dim(E_CO_3)
#' dim(E_CO_4)
#' # but
#' a <- unlist(lapply(1:41, function(i) {
#'   unlist(lapply(1:168, function(j) {
#'     identical(E_CO_3[, i, j], E_CO_4[, i, j])
#'   }))
#' }))
#' unique(a)
#'
#' # Estimation with list of vehicles
#' lpc <- list(pc1, pc1)
#' lef <- EmissionFactorsList(ef_cetesb("CO", "PC_G", agemax = ncol(pc1)))
#' E_COv2 <- emis(veh = lpc, lkm = net$lkm, ef = lef, speed = speed)
#'
#' # top down
#' veh <- age_ldv(x = net$ldv[1:4], name = "PC_E25_1400", agemax = 4)
#' mil <- fkm$KM_PC_E25(1:4)
#' ef <- ef_cetesb("COd", "PC_G")[1:4]
#' emis(veh, units::set_units(mil, "km"), ef)
#'
#' # group online
#' bus1 <- age_hdv(30, agemax = 4)
#' veh <- bus1
#' lkm <- units::set_units(400, "km")
#' speed <- 40
#' efco <- ef_cetesb("COd", "UB", agemax = 4)
#' lef <- ef_hdv_scaled(
#'   dfcol = as.numeric(efco),
#'   v = "Ubus",
#'   t = "Std",
#'   g = ">15 & <=18",
#'   eu = rep("IV", 4),
#'   gr = 0,
#'   l = 0.5,
#'   p = "CO"
#' )
#' for (i in 1:length(lef)) print(lef[[i]](10))
#' (a <- emis(veh = bus1, lkm = lkm, ef = efco, verbose = TRUE))
#' (b <- emis(veh = bus1, lkm = lkm, ef = efco, verbose = TRUE, fortran = TRUE))
#' }
emis <- function(veh,
                 lkm,
                 ef,
                 speed,
                 agemax = if (!inherits(veh, "list")) ncol(veh) else ncol(veh[[1]]),
                 profile,
                 simplify = FALSE,
                 fortran = FALSE,
                 hour = nrow(profile),
                 day = ncol(profile),
                 verbose = FALSE,
                 nt = ifelse(check_nt() == 1, 1, check_nt() / 2)) {
  # lkm can have any length unit, it is converted to km
  lkm <- vein_as_km(lkm, "lkm")

  # Checking sf
  if (inherits(veh, "sf")) {
    if (verbose) message("Converting sf to data.frame")
    veh <- sf::st_set_geometry(veh, NULL)
  }
  if (!missing(hour) | !missing(day)) {
    warning("Arguments hour and day will be deprecated, they will derived from profile")
  }

  # veh is "Vehicles" data-frame ####
  if (!inherits(x = veh, what = "list")) {
    veh <- as.data.frame(remove_units(veh))
    if (agemax > ncol(veh)) {
      stop("'agemax' (", agemax, ") cannot be higher than the number of columns of 'veh' (", ncol(veh), ")")
    }
    if (agemax < ncol(veh)) {
      if (verbose) message("Using the first ", agemax, " columns of 'veh' ('agemax')")
      veh <- veh[, seq_len(agemax), drop = FALSE]
    }
    nr <- nrow(veh)
    nc <- ncol(veh)

    # emission factors that are not functions: numeric vector (one ef by
    # column of veh) or matrix (one ef by street and column of veh)
    if (!inherits(ef, "list")) {
      ef <- vein_ef_gkm(ef, nr)
      ef <- vein_adjust_ef(ef, nc, verbose)
    }

    # top down sin perfil ####
    if (missing(profile) && !inherits(ef, "list")) {
      if (verbose) {
        message(
          paste0(
            "If this is a top down approach, you may try emis_hot_td\n",
            "With this approach speed is not necessary\n"
          )
        )
      }
      if (nr != length(lkm)) stop("Number of rows of `veh` must be the same as the length of `lkm`` ")

      if (fortran && is.matrix(ef)) {
        message("fortran is not implemented when 'ef' varies by street, changing to default")
        fortran <- FALSE
      }
      if (fortran) {
        veh <- as.matrix(veh)
        storage.mode(veh) <- "double"
        nrowv <- as.integer(nr)
        ncolv <- as.integer(nc)
        vein_check_size(nrowv, ncolv)
        # emis(i, j) = veh(i,j) * lkm(i) * ef(j)
        if (!missing(nt)) {
          if (nt >= check_nt()) {
            stop(
              "Your machine has ", check_nt(),
              " threads and nt must be lower"
            )
          }

          if (verbose) message("Calling emis2dfpar.f95")
          a <- dotCall64::.C64(
            .NAME = "emis2dfpar",
            SIGNATURE = c(
              rep("integer", 2),
              rep("double", 3),
              "integer", "double"
            ),
            nrowv = nrowv,
            ncolv = ncolv,
            veh = veh,
            lkm = lkm,
            ef = ef,
            nt = as.integer(nt),
            emis = dotCall64::vector_dc("double", nrowv * ncolv),
            INTENT = c(
              "r", "r",
              "r", "r", "r",
              "r", "w"
            ),
            PACKAGE = "vein",
            VERBOSE = 1
          )$emis
        } else {
          if (verbose) message("Calling emis2df.f95")
          a <- dotCall64::.C64(
            .NAME = "emis2df",
            SIGNATURE = c(
              rep("integer", 2),
              rep("double", 4)
            ),
            nrowv = nrowv,
            ncolv = ncolv,
            veh = veh,
            lkm = lkm,
            ef = ef,
            emis = dotCall64::vector_dc("double", nrowv * ncolv),
            INTENT = c(
              "r", "r",
              "r", "r", "r",
              "w"
            ),
            PACKAGE = "vein",
            VERBOSE = 1
          )$emis
        }
        e <- matrix(a, nrow = nrowv, ncol = ncolv)
        return(Emissions(e))
      } else {
        return(Emissions(vein_vkmef(veh, lkm, ef)))
      }

      # top down sin perfil, FE lista y con velocidad ####
    } else if (missing(profile) && inherits(ef, "list")) {
      # Check speed
      if (missing(speed)) stop("Add speed to be read by the EmissionFactorsList")
      speed <- remove_units(speed)

      if (nr != length(lkm)) stop("Number of rows of `veh` must be the same as the length of `lkm`")
      if (nr != length(unlist(speed))) stop("Number of rows of `veh` must be the same rows of `speed`")

      if (fortran) message("Not implemented yet when ef is 'EmissionsFactorsList`, changing to default")
      ef <- vein_adjust_eflist(ef, nc, verbose)
      a <- matrix(NA_real_, nrow = nr, ncol = nc)
      for (i in seq_len(nc)) {
        a[, i] <- veh[, i] * lkm * as.numeric(unlist(ef[[i]](speed)))
      }
      return(Emissions(a))
    }

    # bottom up ####
    if (length(lkm) == 1 && nr > 1) lkm <- rep(lkm, nr)
    if (nr != length(lkm)) stop("Number of rows of `veh` must be the same as the length of `lkm`")

    profile <- remove_units(profile)
    if (is.vector(profile)) {
      profile <- matrix(as.numeric(profile), ncol = 1)
    } else {
      profile <- as.matrix(profile)
      storage.mode(profile) <- "double"
    }
    nh <- nrow(profile)
    nd <- ncol(profile)

    if (inherits(x = ef, what = "list")) {
      if (verbose) message("Emission factors inherits from list")

      ef <- vein_adjust_eflist(ef, nc, verbose)
      # Check speed
      if (missing(speed)) stop("Add speed to be read by the EmissionFactorsList")
      speed <- remove_units(speed)
      if (fortran) message("Not implemented yet when ef is 'EmissionsFactorsList`, changing to default")

      # speed is passed unevaluated to the ef functions, as many of them
      # (e.g. constant emission factors) do not use it
      if (simplify) {
        pro <- as.numeric(profile)
        d <- array(NA_real_, dim = c(nr, nc, length(pro)))
        for (j in seq_along(pro)) { # hours x days
          for (k in seq_len(nc)) { # categorias
            d[, k, j] <- veh[, k] * pro[j] * lkm * as.numeric(ef[[k]](speed[, j]))
          }
        }
      } else {
        d <- array(NA_real_, dim = c(nr, nc, nh, nd))
        for (j in seq_len(nd)) { # 7 dias
          for (i in seq_len(nh)) { # 24 horas
            for (k in seq_len(nc)) { # categorias
              d[, k, i, j] <- veh[, k] * profile[i, j] * lkm *
                as.numeric(ef[[k]](speed[, i + nh * (j - 1)]))
            }
          }
        }
      }
    } else {
      if (verbose) message("Emission factors does not inherits from list")
      # Check speed
      if (!missing(speed)) warning("When ef is not EmissionFactorsList, speed is not needed")
      if (fortran && is.matrix(ef)) {
        message("fortran is not implemented when 'ef' varies by street, changing to default")
        fortran <- FALSE
      }

      # simplify?
      if (simplify) {
        if (fortran) {
          veh <- as.matrix(veh)
          storage.mode(veh) <- "double"
          profile <- as.numeric(profile)
          nrowv <- as.integer(nr)
          ncolv <- as.integer(nc)
          prok <- as.integer(length(profile))
          vein_check_size(nrowv, ncolv, prok)
          # emis(i, j, k) = veh(i, j) * lkm(i) * ef(j)*pro(k)

          if (!missing(nt)) {
            if (nt >= check_nt()) {
              stop(
                "Your machine has ", check_nt(),
                " threads and nt must be lower"
              )
            }

            if (verbose) message("Calling emis3dfpar.f95")
            a <- dotCall64::.C64(
              .NAME = "emis3dfpar",
              SIGNATURE = c(
                rep("integer", 3),
                rep("double", 4),
                "integer", "double"
              ),
              nrowv = nrowv,
              ncolv = ncolv,
              prok = prok,
              veh = veh,
              lkm = lkm,
              ef = ef,
              pro = profile,
              nt = as.integer(nt),
              emis = dotCall64::vector_dc("double", nrowv * ncolv * prok),
              INTENT = c(
                "r", "r", "r",
                "r", "r", "r", "r",
                "r", "w"
              ),
              PACKAGE = "vein",
              VERBOSE = 1
            )$emis
          } else {
            if (verbose) message("Calling emis3df.f95")
            a <- dotCall64::.C64(
              .NAME = "emis3df",
              SIGNATURE = c(
                rep("integer", 3),
                rep("double", 5)
              ),
              nrowv = nrowv,
              ncolv = ncolv,
              prok = prok,
              veh = veh,
              lkm = lkm,
              ef = ef,
              pro = profile,
              emis = dotCall64::vector_dc("double", nrowv * ncolv * prok),
              INTENT = c(
                "r", "r", "r",
                "r", "r", "r", "r",
                "w"
              ),
              PACKAGE = "vein",
              VERBOSE = 1
            )$emis
          }
          e <- array(a, dim = c(nrowv, ncolv, prok))
          return(EmissionsArray(e))
        } else {
          # streets x categories x (hours * days)
          d <- outer(vein_vkmef(veh, lkm, ef), as.numeric(profile))
        }
      } else {
        if (fortran) {
          veh <- as.matrix(veh)
          storage.mode(veh) <- "double"
          nrowv <- as.integer(nr)
          ncolv <- as.integer(nc)
          proh <- as.integer(nh)
          prod <- as.integer(nd)
          vein_check_size(nrowv, ncolv, proh, prod)
          # emis(i, j, k, l) = veh(i,j) * lkm(i) * ef(j)*pro(k,l)

          if (!missing(nt)) {
            if (nt >= check_nt()) {
              stop(
                "Your machine has ", check_nt(),
                " threads and nt must be lower"
              )
            }

            if (verbose) message("Calling emis4dfpar.f95")
            a <- dotCall64::.C64(
              .NAME = "emis4dfpar",
              SIGNATURE = c(
                rep("integer", 4),
                rep("double", 4),
                "integer",
                "double"
              ),
              nrowv = nrowv,
              ncolv = ncolv,
              proh = proh,
              prod = prod,
              veh = veh,
              lkm = lkm,
              ef = ef,
              pro = profile,
              nt = as.integer(nt),
              emis = dotCall64::vector_dc("double", nrowv * ncolv * proh * prod),
              INTENT = c(
                "r", "r", "r", "r",
                "r", "r", "r", "r",
                "r", "w"
              ),
              PACKAGE = "vein",
              VERBOSE = 1
            )$emis
          } else {
            if (verbose) message("Calling emis4df.f95")
            a <- dotCall64::.C64(
              .NAME = "emis4df",
              SIGNATURE = c(
                rep("integer", 4),
                rep("double", 5)
              ),
              nrowv = nrowv,
              ncolv = ncolv,
              proh = proh,
              prod = prod,
              veh = veh,
              lkm = lkm,
              ef = ef,
              pro = profile,
              emis = dotCall64::vector_dc("double", nrowv * ncolv * proh * prod),
              INTENT = c(
                "r", "r", "r", "r",
                "r", "r", "r", "r",
                "w"
              ),
              PACKAGE = "vein",
              VERBOSE = 1
            )$emis
          }
          e <- array(a, dim = c(nrowv, ncolv, proh, prod))
          return(EmissionsArray(e))
        } else {
          # streets x categories x hours x days
          d <- outer(vein_vkmef(veh, lkm, ef), profile)
        }
      }
    }

    if (verbose) message(round(sum(d, na.rm = T) / 1000, 2), " kg emissions")
    return(EmissionsArray(d))

    # veh is a list of "Vehicles" data-frames
    # each member of the list is an hour
  } else {
    if (missing(speed)) stop("Add speed to be read by the EmissionFactorsList")
    speed <- as.data.frame(remove_units(speed))
    for (j in seq_along(veh)) {
      veh[[j]] <- as.data.frame(remove_units(veh[[j]]))
    }
    nr <- nrow(veh[[1]])
    nc <- agemax
    if (nc > ncol(veh[[1]])) {
      stop("'agemax' (", nc, ") cannot be higher than the number of columns of 'veh' (", ncol(veh[[1]]), ")")
    }
    if (length(lkm) == 1 && nr > 1) lkm <- rep(lkm, nr)
    if (nr != length(lkm)) stop("Number of rows of `veh` must be the same as the length of `lkm`")
    ef <- vein_adjust_eflist(ef, ncol(veh[[1]]), verbose)

    d <- array(NA_real_, dim = c(nr, nc, length(veh)))
    for (i in seq_along(veh)) {
      for (k in seq_len(nc)) {
        d[, k, i] <- veh[[i]][, k] * lkm * as.numeric(ef[[k]](speed[, i]))
      }
    }
    if (verbose) message(round(sum(d, na.rm = T) / 1000, 2), " kg emissions ")
    return(EmissionsArray(d))
  }
}

# Internal helpers shared by the emission functions ####

#' Converts a length with units into numeric km
#' @noRd
vein_as_km <- function(x, name = "lkm") {
  if (!inherits(x, "units")) {
    stop(name, " needs to has class 'units' in 'km'. Please, check package 'units'")
  }
  x <- tryCatch(
    units::set_units(x, "km", mode = "standard"),
    error = function(e) {
      stop("Units of ", name, " must be a length, e.g. 'km': ",
        conditionMessage(e), call. = FALSE)
    }
  )
  as.numeric(x)
}

#' Converts a vector with units into numeric g/km
#' @noRd
vein_as_gkm <- function(x, name = "ef") {
  if (!inherits(x, "units")) return(as.numeric(x))
  x <- tryCatch(
    units::set_units(x, "g/km", mode = "standard"),
    error = function(e) {
      stop("Units of ", name, " must be convertible to 'g/km': ",
        conditionMessage(e), call. = FALSE)
    }
  )
  as.numeric(x)
}

#' Numeric emission factors for emis: vector (one by column of veh) or
#' matrix (one by street and column of veh)
#' @noRd
vein_ef_gkm <- function(ef, nr) {
  if (is.data.frame(ef) || is.matrix(ef)) {
    if (inherits(ef, "sf")) ef <- sf::st_set_geometry(ef, NULL)
    ef <- as.data.frame(ef)
    m <- vapply(ef, vein_as_gkm, numeric(nrow(ef)))
    m <- matrix(m, nrow = nrow(ef))
    if (nrow(m) == 1) {
      return(as.numeric(m))
    } else if (nrow(m) == nr) {
      return(m)
    } else {
      stop("When 'ef' is a data.frame it must have 1 row or the same number of rows of 'veh'")
    }
  }
  vein_as_gkm(ef)
}

#' Adjust numeric ef to the number of columns of veh
#' @noRd
vein_adjust_ef <- function(ef, nc, verbose = FALSE) {
  n <- if (is.matrix(ef)) ncol(ef) else length(ef)
  if (n == nc) return(ef)
  if (verbose) {
    message(
      paste0(
        "Number of columns of 'veh' is different than length of 'ef'\n",
        "adjusting length of ef to the number of colums of 'veh'\n"
      )
    )
  }
  idx <- pmin(seq_len(nc), n)
  if (is.matrix(ef)) ef[, idx, drop = FALSE] else ef[idx]
}

#' Adjust list of ef functions to the number of columns of veh
#' @noRd
vein_adjust_eflist <- function(ef, nc, verbose = FALSE) {
  n <- length(ef)
  if (n == nc) return(ef)
  if (verbose) {
    message(
      paste0(
        "Number of columns of 'veh' is different than length of 'ef'\n",
        "adjusting length of ef to the number of colums of 'veh'\n"
      )
    )
  }
  cl <- class(ef)
  ef <- unclass(ef)[pmin(seq_len(nc), n)]
  class(ef) <- cl
  ef
}

#' veh * lkm * ef as a numeric matrix streets x categories
#' @noRd
vein_vkmef <- function(veh, lkm, ef) {
  vkm <- as.matrix(veh) * lkm
  storage.mode(vkm) <- "double"
  if (is.matrix(ef)) {
    e <- vkm * ef
  } else {
    e <- sweep(vkm, 2, ef, "*")
  }
  dimnames(e) <- NULL
  e
}

#' Stops when the number of elements of a Fortran array is too big for
#' the integer arguments of the Fortran subroutines
#' @noRd
vein_check_size <- function(...) {
  n <- prod(as.numeric(c(...)))
  if (n > .Machine$integer.max) {
    stop(
      "The output would have ", format(n, big.mark = ","), " elements, more than ",
      format(.Machine$integer.max, big.mark = ","), " supported by fortran = TRUE. ",
      "Use fortran = FALSE or split 'veh' in chunks of streets",
      call. = FALSE
    )
  }
  invisible(n)
}
