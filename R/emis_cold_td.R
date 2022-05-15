#' Estimation of cold start emissions with top-down approach
#'
#' @description \code{\link{emis_cold_td}} estimates cld start emissions with
#' a top-down appraoch. This is, annual or monthly emissions or region.
#' Especifically, the emissions are esitmated for row of the simple feature (row
#' of the spatial feature).
#'
#' In general was designed so that each simple feature is a region with
#' different average monthly temperature.
#' This funcion, as other in this package, adapts to the class of the input data.
#' providing flexibility to the user.
#'
#' @param veh "Vehicles" data-frame or spatial feature, wwhere columns are the
#'  age distribution of that vehicle. and rows each simple feature or region.
#' The number of rows is equal to the number of streets link
#' @param lkm Numeric; mileage by the age of use of each vehicle.
#' @param ef Numeric; emission factor with
#' @param efcold Data.frame. When it is a data.frame, each column is for each
#' type of vehicle by age of use, rows are are each simple feature. When you have
#' emission factors for each month, the order should a data.frame ina long format,
#' as rurned by \code{\link{ef_ldv_cold}}.
#' @param beta Data.frame with the fraction of cold starts. The rows are the fraction
#' for each spatial feature or subregion, the columns are the age of use of vehicle.
#' @param pro_month Numeric; montly profile to distribuite annual mileage in each month.
#' @param params List of parameters; Add columns with information to returning data.frame
#' @param verbose Logical; To show more information
#' @param fortran Logical; to try the fortran calculation.
#' @param nt Integer; Number of threads wich must be lower than max available.
#' See \code{\link{check_nt}}. Only when fortran = TRUE
#' @return Emissions data.frame
#' @importFrom dotCall64 .C64 vector_dc
#' @seealso \code{\link{ef_ldv_cold}}
#' @export
#' @examples
#' \dontrun{
#' # Do not run
#' veh <- age_ldv(1:10, agemax = 8)
#' euros <- c("V", "V", "IV", "III", "II", "I", "PRE", "PRE")
#' dt <- matrix(rep(2:25, 5), ncol = 12, nrow = 10) # 12 months, 10 rows
#' row.names(dt) <- paste0("Simple_Feature_", 1:10)
#' efc <- ef_ldv_cold(ta = dt, cc = "<=1400", f = "G", eu = euros, p = "CO", speed = Speed(34))
#' efh <- ef_ldv_speed(
#'   v = "PC", t = "4S", cc = "<=1400", f = "G",
#'   eu = euros, p = "CO", speed = Speed(runif(nrow(veh), 15, 40))
#' )
#' lkm <- units::as_units(18:11, "km") * 1000
#' cold_lkm <- cold_mileage(ltrip = units::as_units(20, "km"), ta = celsius(dt))
#' names(cold_lkm) <- paste0("Month_", 1:12)
#' veh_month <- c(rep(8, 1), rep(10, 5), 9, rep(10, 5))
#' system.time(
#'   a <- emis_cold_td(
#'     veh = veh,
#'     lkm = lkm,
#'     ef = efh[1, ],
#'     efcold = efc[1:10, ],
#'     beta = cold_lkm[, 1],
#'     verbose = TRUE
#'   )
#' )
#' system.time(
#'   a2 <- emis_cold_td(
#'     veh = veh,
#'     lkm = lkm,
#'     ef = efh[1, ],
#'     efcold = efc[1:10, ],
#'     beta = cold_lkm[, 1],
#'     verbose = TRUE,
#'     fortran = TRUE
#'   )
#' ) # emistd2coldf.f95
#' a$emissions <- round(a$emissions, 8)
#' a2$emissions <- round(a2$emissions, 8)
#' identical(a, a2)
#'
#' # Adding parameters
#' emis_cold_td(
#'   veh = veh,
#'   lkm = lkm,
#'   ef = efh[1, ],
#'   efcold = efc[1:10, ],
#'   beta = cold_lkm[, 1],
#'   verbose = TRUE,
#'   params = list(
#'     paste0("data_", 1:10),
#'     "moredata"
#'   )
#' )
#' system.time(
#'   aa <- emis_cold_td(
#'     veh = veh,
#'     lkm = lkm,
#'     ef = efh,
#'     efcold = efc,
#'     beta = cold_lkm,
#'     pro_month = veh_month,
#'     verbose = TRUE
#'   )
#' )
#' system.time(
#'   aa2 <- emis_cold_td(
#'     veh = veh,
#'     lkm = lkm,
#'     ef = efh,
#'     efcold = efc,
#'     beta = cold_lkm,
#'     pro_month = veh_month,
#'     verbose = TRUE,
#'     fortran = TRUE
#'   )
#' ) # emistd5coldf.f95
#' aa$emissions <- round(aa$emissions, 8)
#' aa2$emissions <- round(aa2$emissions, 8)
#' identical(aa, aa2)
#' }
emis_cold_td <- function(veh,
                         lkm,
                         ef,
                         efcold,
                         beta,
                         pro_month,
                         params,
                         verbose = FALSE,
                         fortran = FALSE,
                         nt = ifelse(check_nt() == 1, 1, check_nt() / 2)) {
  # Check units
  if (!inherits(lkm,"units")) {
    stop("lkm neeeds to has class 'units' in 'km'. Please, check package '?units::set_units'")
  }
  if (units(lkm)$numerator == "m") {
    stop("Units of lkm is 'm' ")
  }
  if (units(lkm)$numerator == "km") {
    lkm <- as.numeric(lkm)
  }
  if (length(lkm) != ncol(veh)) stop("Length of 'lkm' must be the as the number of columns of 'veh'")
  # Checking ef
  if (is.matrix(ef) | is.data.frame(ef)) {
    ef <- as.data.frame(ef)
    if (!inherits(ef[, 1], "units")) {
      stop("columns of ef must has class 'units' in 'g/km'. Please, check package '?units::set_units'")
    }
    if (units(ef[, 1])$numerator != "g" || units(ef[, 1])$denominator != "km") {
      stop("Units of efcold must be 'g/km' ")
    }
    if (units(ef[, 1])$numerator == "g" || units(ef[, 1])$denominator == "km") {
      for (i in 1:ncol(veh)) {
        ef[, i] <- as.numeric(ef[, i])
      }
    }
  } else {
    if (!inherits(ef, "units")) {
      stop("ef must has class 'units' in 'g/km'. Please, check package '?units::set_units'")
    }
    if (units(ef)$numerator != "g" || units(ef)$denominator != "km") {
      stop("Units of ef must be 'g/km' ")
    }
    # if(units(ef)$numerator == "g" || units(ef)$denominator == "km"){ # not tested
    # ef <- as.numeric(ef)
    # }
  }

  # Checking ef cold
  if (!inherits(efcold[, 1],  "units")) {
    stop("columns of efcold must has class 'units' in 'g/km'. Please, check package '?units::set_units'")
  }
  if (units(efcold[, 1])$numerator != "g" || units(efcold[, 1])$denominator != "km") {
    stop("Units of efcold must be 'g/km' ")
  }
  if (units(efcold[, 1])$numerator == "g" && units(efcold[, 1])$denominator == "km") {
    for (i in 1:ncol(veh)) {
      efcold[, i] <- as.numeric(efcold[, i])
    }
  }
  # Checking veh
  for (i in 1:ncol(veh)) {
    veh[, i] <- as.numeric(veh[, i])
  }

  # Checking sf
  if (inherits(veh, "sf")) {
    if (verbose) message("converting sf to data.frame")
    veh <- sf::st_set_geometry(veh, NULL)
  }
  # checking beta
  beta <- as.data.frame(beta)

  # pro_month
  if (!missing(pro_month)) {
    if (is.data.frame(pro_month) | is.matrix(pro_month)) {
      pro_month <- as.data.frame(pro_month)
      for (i in 1:nrow(pro_month)) {
        pro_month[i, ] <- pro_month[i, ] / sum(pro_month[i, ])
      }
    } else if (is.numeric(pro_month)) {
      pro_month <- pro_month / sum(pro_month)
    }
  }

  # Checking pro_month
  if (!missing(pro_month)) {
    if (verbose) message("Estimation with monthly profile")



    if (length(pro_month) != 12) stop("Length of pro_month must be 12")

    mes <- ifelse(nchar(1:12) < 2, paste0(0, 1:12), 1:12)

    if (is.data.frame(ef)) {
      if (verbose) message("Assuming you have emission factors for each simple feature and then for each month")

      # when pro_month varies in each simple feature
      if (is.data.frame(pro_month)) {
        if (nrow(pro_month) == 1) {
          message("Replicating one-row matrix to match number of rows of `veh`")
          pro_month <- matrix(as.numeric(pro_month), nrow = nrow(veh), ncol = ncol(pro_month))
        }

        if (fortran) {
          efcold$month <- rep(1:12, each = nrow(veh))
          efcold <- split(efcold[, 1:ncol(veh)], efcold$month)
          efcold <- as.numeric(unlist(lapply(efcold, unlist)))

          nrowv <- as.integer(nrow(veh))
          ncolv <- as.integer(ncol(veh))
          pmonth <- as.integer(ncol(pro_month))
          lkm <- as.numeric(lkm)
          ef <- as.matrix(ef)

          month <- as.matrix(pro_month)
          beta <- as.matrix(beta)

          if (nrow(beta) != nrow(veh)) stop("number of rows of 'beta' and 'veh' must be equal")
          if (ncol(beta) != ncol(month)) stop("number of cols of 'beta' and 'month' must be equal")
          if (length(efcold) != length(unlist(veh)) * pmonth) stop("`efcold` and `veh` must be dimensionally compatible")
          if (length(lkm) != ncol(veh)) stop("length of `lkm` must be equal to number of columns of `veh`")
          if (nrow(ef) != nrow(veh)) stop("number of rows of `ef` and `veh` must be equal")
          if (ncol(ef) != ncol(veh)) stop("number of cols of `ef` and `veh` must be equal")
          if (nrow(month) != nrow(veh)) stop("number of rows of `month` and `veh` must be equal")
          # emis(i, j, k) = beta(i, k) * veh(i, j) * lkm(j) * ef(i, j) * efcold(i, j, k) * month(i, k)

          if (!missing(nt)) {
            if (nt >= check_nt()) {
              stop(
                "Your machine has ", check_nt(),
                " threads and nt must be lower"
              )
            }

            if (verbose) message("Calling emistd4coldfpar.f95")
            a <- dotCall64::.C64(
              .NAME = "emistd4coldfpar",
              SIGNATURE = c(
                rep("integer", 3),
                rep("double", 6),
                "integer", "double"
              ),
              nrowv = nrowv,
              ncolv = ncolv,
              pmonth = pmonth,
              veh = as.matrix(veh),
              lkm = lkm,
              ef = ef,
              efcold = efcold,
              beta = beta,
              month = month,
              nt = as.integer(nt),
              emis = dotCall64::vector_dc("double", nrowv * ncolv * pmonth),
              INTENT = c(
                rep("r", 10), "w"
              ),
              PACKAGE = "vein",
              VERBOSE = 1
            )$emis
          } else {
            if (verbose) message("Calling emistd4coldf.f95")
            a <- dotCall64::.C64(
              .NAME = "emistd4coldf",
              SIGNATURE = c(
                rep("integer", 3),
                rep("double", 7)
              ),
              nrowv = nrowv,
              ncolv = ncolv,
              pmonth = pmonth,
              veh = as.matrix(veh),
              lkm = lkm,
              ef = ef,
              efcold = efcold,
              beta = beta,
              month = month,
              emis = dotCall64::vector_dc("double", nrowv * ncolv * pmonth),
              INTENT = c(
                rep("r", 9), "w"
              ),
              PACKAGE = "vein",
              VERBOSE = 1
            )$emis
          }
          e <- data.frame(emissions = a)
          e <- Emissions(e)
          e$rows <- rep(row.names(veh), ncolv * pmonth)
          e$age <- rep(rep(seq(1, ncolv), each = nrowv), pmonth)
          e$month <- rep(seq(1, pmonth), each = ncolv * nrowv)
        } else {
          efcold$month <- rep(1:12, each = nrow(veh))
          efcold <- split(efcold, efcold$month)
          e <- do.call("rbind", lapply(1:12, function(k) {
            dfi <- unlist(lapply(1:ncol(veh), function(j) {
              beta[, k] * lkm[j] * veh[, j] * pro_month[, k] * ef[, j] * efcold[[k]][, j]
            }))
            dfi <- as.data.frame(dfi)
            names(dfi) <- "emissions"
            dfi <- Emissions(dfi)
            dfi$rows <- row.names(veh)
            dfi$age <- rep(1:ncol(veh), each = nrow(veh))
            dfi$month <- (1:length(pro_month))[k]
            dfi
          }))
        }
      } else if (is.numeric(pro_month)) {
        if (fortran) {
          nrowv <- as.integer(nrow(veh))
          ncolv <- as.integer(ncol(veh))
          pmonth <- as.integer(length(pro_month))
          lkm <- as.numeric(lkm)
          ef <- as.matrix(ef[, 1:ncol(veh)])
          efcold$month <- rep(1:12, each = nrow(veh))
          efcold <- split(efcold[, 1:ncol(veh)], efcold$month)
          efcold <- as.numeric(unlist(lapply(efcold, unlist)))
          month <- as.numeric(pro_month)
          beta <- as.matrix(beta)


          if (nrow(beta) != nrow(veh)) stop("number of rows of 'beta' and 'veh' must be equal")
          if (ncol(beta) != length(month)) stop("number of cols of 'beta' length of 'month' must be equal")
          if (length(efcold) != length(unlist(veh)) * pmonth) stop("`efcold` and `veh` must be dimensionally compatible")
          if (length(lkm) != ncol(veh)) stop("length of `lkm` must be equal to number of columns of `veh`")
          if (nrow(ef) != nrow(veh)) stop("number of rows of `ef` and `veh` must be equal")
          if (ncol(ef) != ncol(veh)) stop("number of cols of `ef` and `veh` must be equal")
          # emis(i, j, k) = beta(i, k) * veh(i, j) * lkm(j) * ef(i, j) * efcold(i, j, k) * month(k)

          if (!missing(nt)) {
            if (nt >= check_nt()) {
              stop(
                "Your machine has ", check_nt(),
                " threads and nt must be lower"
              )
            }

            if (verbose) message("Calling emistd5coldfpar.f95")
            a <- dotCall64::.C64(
              .NAME = "emistd5coldfpar",
              SIGNATURE = c(
                rep("integer", 3),
                rep("double", 6),
                "integer", "double"
              ),
              nrowv = nrowv,
              ncolv = ncolv,
              pmonth = pmonth,
              veh = as.matrix(veh),
              lkm = lkm,
              ef = ef,
              ef = efcold,
              beta = beta,
              month = month,
              nt = as.integer(nt),
              emis = dotCall64::vector_dc("double", nrowv * ncolv * pmonth),
              INTENT = c(
                rep("r", 10), "w"
              ),
              PACKAGE = "vein",
              VERBOSE = 1
            )$emis
          } else {
            if (verbose) message("Calling emistd5coldf.f95")
            a <- dotCall64::.C64(
              .NAME = "emistd5coldf",
              SIGNATURE = c(
                rep("integer", 3),
                rep("double", 7)
              ),
              nrowv = nrowv,
              ncolv = ncolv,
              pmonth = pmonth,
              veh = as.matrix(veh),
              lkm = lkm,
              ef = ef,
              ef = efcold,
              beta = beta,
              month = month,
              emis = dotCall64::vector_dc("double", nrowv * ncolv * pmonth),
              INTENT = c(
                rep("r", 9), "w"
              ),
              PACKAGE = "vein",
              VERBOSE = 1
            )$emis
          }
          e <- data.frame(emissions = a)
          e <- Emissions(e)
          e$rows <- rep(row.names(veh), ncolv * pmonth)
          e$age <- rep(rep(seq(1, ncolv), each = nrowv), pmonth)
          e$month <- rep(seq(1, pmonth), each = ncolv * nrowv)
        } else {
          efcold$month <- rep(1:12, each = nrow(veh))
          efcold <- split(efcold, efcold$month)
          e <- do.call("rbind", lapply(1:12, function(k) {
            dfi <- unlist(lapply(1:ncol(veh), function(j) {
              beta[, k] * lkm[j] * veh[, j] * pro_month[k] * ef[, j] * efcold[[k]][, j]
            }))
            dfi <- as.data.frame(dfi)
            names(dfi) <- "emissions"
            dfi <- Emissions(dfi)
            dfi$rows <- row.names(veh)
            dfi$age <- rep(1:ncol(veh), each = nrow(veh))
            dfi$month <- (1:length(pro_month))[k]
            dfi
          }))
        }
      }
      if (!missing(params)) {
        if (!is.list(params)) stop("'params' must be a list")
        if (is.null(names(params))) {
          if (verbose) message("Adding names to params")
          names(params) <- paste0("P_", 1:length(params))
        }
        for (i in 1:length(params)) {
          e[, names(params)[i]] <- params[[i]]
        }
      }
      if (verbose) cat("Sum of emissions:", sum(e$emissions), "\n")
    } else {
      # ef inherits numeric
      if (verbose) message("Assuming you have emission factors for each simple feature and then for each month")


      # when pro_month vary each month
      if (is.data.frame(pro_month)) {
        if (nrow(pro_month) == 1) {
          message("Replicating one-row matrix to match number of rows of `veh`")
          pro_month <- matrix(as.numeric(pro_month), nrow = nrow(veh), ncol = ncol(pro_month))
        }

        if (fortran) {
          nrowv <- as.integer(nrow(veh))
          ncolv <- as.integer(ncol(veh))
          pmonth <- as.integer(ncol(pro_month))
          lkm <- as.numeric(lkm)
          ef <- as.numeric(ef)
          efcold$month <- rep(1:12, each = nrow(veh))
          efcold <- split(efcold[, 1:ncol(veh)], efcold$month)
          efcold <- as.numeric(unlist(lapply(efcold, unlist)))
          month <- as.matrix(pro_month)
          beta <- as.matrix(beta)


          if (nrow(beta) != nrow(veh)) stop("number of rows of 'beta' and 'veh' must be equal")
          if (ncol(beta) != ncol(month)) stop("number of cols of 'beta' and 'month' must be equal")
          if (length(efcold) != length(unlist(veh)) * pmonth) stop("`efcold` and `veh` must be dimensionally compatible")
          if (length(lkm) != ncol(veh)) stop("length of `lkm` must be equal to number of columns of `veh`")
          if (length(ef) != ncol(veh)) stop("length of `ef` and number of cols of `veh` must be equal")
          if (nrow(month) != nrow(veh)) stop("number of rows of `month` and `veh` must be equal")
          # emis(i, j, k) = beta(i, k) * veh(i, j) * lkm(j) * ef(j) * efcold(i, j, k) * month(i, k)

          if (!missing(nt)) {
            if (nt >= check_nt()) {
              stop(
                "Your machine has ", check_nt(),
                " threads and nt must be lower"
              )
            }

            if (verbose) message("Calling emistd6coldfpar.f95")
            a <- dotCall64::.C64(
              .NAME = "emistd6coldfpar",
              SIGNATURE = c(
                rep("integer", 3),
                rep("double", 6),
                "integer", "double"
              ),
              nrowv = nrowv,
              ncolv = ncolv,
              pmonth = pmonth,
              veh = as.matrix(veh),
              lkm = lkm,
              ef = ef,
              ef = efcold,
              beta = beta,
              month = month,
              nt = as.integer(nt),
              emis = dotCall64::vector_dc("double", nrowv * ncolv * pmonth),
              INTENT = c(
                rep("r", 10), "w"
              ),
              PACKAGE = "vein",
              VERBOSE = 1
            )$emis
          } else {
            if (verbose) message("Calling emistd6coldf.f95")
            a <- dotCall64::.C64(
              .NAME = "emistd6coldf",
              SIGNATURE = c(
                rep("integer", 3),
                rep("double", 7)
              ),
              nrowv = nrowv,
              ncolv = ncolv,
              pmonth = pmonth,
              veh = as.matrix(veh),
              lkm = lkm,
              ef = ef,
              ef = efcold,
              beta = beta,
              month = month,
              emis = dotCall64::vector_dc("double", nrowv * ncolv * pmonth),
              INTENT = c(
                rep("r", 9), "w"
              ),
              PACKAGE = "vein",
              VERBOSE = 1
            )$emis
          }
          e <- data.frame(emissions = a)
          e <- Emissions(e)
          e$rows <- rep(row.names(veh), ncolv * pmonth)
          e$age <- rep(rep(seq(1, ncolv), each = nrowv), pmonth)
          e$month <- rep(seq(1, pmonth), each = ncolv * nrowv)
        } else {
          efcold$month <- rep(1:12, each = nrow(veh))
          efcold <- split(efcold, efcold$month)
          e <- do.call("rbind", lapply(1:12, function(k) {
            dfi <- unlist(lapply(1:ncol(veh), function(j) {
              beta[, k] * lkm[j] * veh[, j] * pro_month[, k] * ef[j] * efcold[[k]][, j]
            }))
            dfi <- as.data.frame(dfi)
            names(dfi) <- "emissions"
            dfi <- Emissions(dfi)
            dfi$rows <- row.names(veh)
            dfi$age <- rep(1:ncol(veh), each = nrow(veh))
            dfi$month <- (1:length(pro_month))[k]
            dfi
          }))
        }
      } else if (is.numeric(pro_month)) {
        if (fortran) {
          nrowv <- as.integer(nrow(veh))
          ncolv <- as.integer(ncol(veh))
          pmonth <- as.integer(length(pro_month))
          lkm <- as.numeric(lkm)
          ef <- as.numeric(ef)
          efcold <- split(efcold[, 1:ncol(veh)], efcold[ncol(efcold)])
          efcold <- as.numeric(unlist(lapply(efcold, unlist)))
          month <- as.numeric(pro_month)
          beta <- as.matrix(beta)

          if (nrow(beta) != nrow(veh)) stop("number of rows of 'beta' and 'veh' must be equal")
          if (ncol(beta) != length(month)) stop("number of cols of 'beta' length of 'month' must be equal")
          if (length(efcold) != length(unlist(veh)) * pmonth) stop("`efcold` and `veh` must be dimensionally compatible")
          if (length(lkm) != ncol(veh)) stop("length of `lkm` must be equal to number of columns of `veh`")
          if (length(ef) != ncol(veh)) stop("number of rows of `ef` and `veh` must be equal")
          # emis(i, j, k) = beta(i, k) * veh(i, j) * lkm(j) * ef(j) * efcold(i, j, k) * month(k)

          if (!missing(nt)) {
            if (nt >= check_nt()) {
              stop(
                "Your machine has ", check_nt(),
                " threads and nt must be lower"
              )
            }

            if (verbose) message("Calling emistd3coldfpar.f95")

            a <- dotCall64::.C64(
              .NAME = "emistd3coldfpar",
              SIGNATURE = c(
                rep("integer", 3),
                rep("double", 6),
                "integer", "double"
              ),
              nrowv = nrowv,
              ncolv = ncolv,
              pmonth = pmonth,
              veh = as.matrix(veh),
              lkm = lkm,
              ef = ef,
              ef = efcold,
              beta = beta,
              month = month,
              nt = as.integer(nt),
              emis = dotCall64::vector_dc("double", nrowv * ncolv * pmonth),
              INTENT = c(
                rep("r", 10), "w"
              ),
              PACKAGE = "vein",
              VERBOSE = 1
            )$emis
          } else {
            if (verbose) message("Calling emistd3coldf.f95")
            a <- dotCall64::.C64(
              .NAME = "emistd3coldf",
              SIGNATURE = c(
                rep("integer", 3),
                rep("double", 7)
              ),
              nrowv = nrowv,
              ncolv = ncolv,
              pmonth = pmonth,
              veh = as.matrix(veh),
              lkm = lkm,
              ef = ef,
              ef = efcold,
              beta = beta,
              month = month,
              emis = dotCall64::vector_dc("double", nrowv * ncolv * pmonth),
              INTENT = c(
                rep("r", 9), "w"
              ),
              PACKAGE = "vein",
              VERBOSE = 1
            )$emis
          }
          e <- data.frame(emissions = a)
          e <- Emissions(e)
          e$rows <- rep(row.names(veh), ncolv * pmonth)
          e$age <- rep(rep(seq(1, ncolv), each = nrowv), pmonth)
          e$month <- rep(seq(1, pmonth), each = ncolv * nrowv)
        } else {
          efcold$month <- rep(1:12, each = nrow(veh))
          efcold <- split(efcold, efcold$month)
          e <- do.call("rbind", lapply(1:12, function(k) {
            dfi <- unlist(lapply(1:ncol(veh), function(j) {
              beta[, k] * lkm[j] * veh[, j] * pro_month[k] * ef[j] * efcold[[k]][, j]
              t
            }))
            dfi <- as.data.frame(dfi)
            names(dfi) <- "emissions"
            dfi <- Emissions(dfi)
            dfi$rows <- row.names(veh)
            dfi$age <- rep(1:ncol(veh), each = nrow(veh))
            dfi$month <- (1:length(pro_month))[k]
            dfi
          }))
        }
      }
      if (!missing(params)) {
        if (!is.list(params)) stop("'params' must be a list")
        if (is.null(names(params))) {
          if (verbose) message("Adding names to params")
          names(params) <- paste0("P_", 1:length(params))
        }
        for (i in 1:length(params)) {
          e[, names(params)[i]] <- params[[i]]
        }
      }
      if (verbose) cat("Sum of emissions:", sum(e$emissions), "\n")
    }
  } else {
    if (verbose) message("Estimation without monthly profile")
    if (fortran) {
      nrowv <- as.integer(nrow(veh))
      ncolv <- as.integer(ncol(veh))
      lkm <- as.numeric(lkm)
      ef <- as.numeric(ef)
      efcold <- as.matrix(efcold[, 1:ncol(veh)])
      beta <- as.numeric(unlist(beta))


      if (length(beta) != nrow(veh)) stop("length of 'beta' and number of rows of 'veh' must be equal")
      if (nrow(efcold) != nrow(veh)) stop("number of rows of 'efcold' and 'veh' must be equal")
      if (ncol(efcold) != ncol(veh)) stop("number of cols of 'efcold' and 'veh' must be equal")
      if (length(lkm) != ncol(veh)) stop("length of `lkm` must be equal to number of columns of `veh`")
      if (length(ef) != ncol(veh)) stop("number of rows of `ef` and `veh` must be equal")
      # emis(i, j) = beta(i) * veh(i, j) * lkm(j) * ef(j) * efcold(i, j)

      if (!missing(nt)) {
        if (nt >= check_nt()) {
          stop(
            "Your machine has ", check_nt(),
            " threads and nt must be lower"
          )
        }

        if (verbose) message("Calling emistd2coldfpar.f95")
        a <- dotCall64::.C64(
          .NAME = "emistd2coldfpar",
          SIGNATURE = c(
            rep("integer", 2),
            rep("double", 5),
            "integer", "double"
          ),
          nrowv = nrowv,
          ncolv = ncolv,
          veh = as.matrix(veh),
          lkm = lkm,
          ef = ef,
          ef = efcold,
          beta = beta,
          nt = as.integer(nt),
          emis = dotCall64::vector_dc("double", nrowv * ncolv),
          INTENT = c(
            rep("r", 8), "w"
          ),
          PACKAGE = "vein",
          VERBOSE = 1
        )$emis
      } else {
        if (verbose) message("Calling emistd2coldf.f95")
        a <- dotCall64::.C64(
          .NAME = "emistd2coldf",
          SIGNATURE = c(
            rep("integer", 2),
            rep("double", 6)
          ),
          nrowv = nrowv,
          ncolv = ncolv,
          veh = as.matrix(veh),
          lkm = lkm,
          ef = ef,
          ef = efcold,
          beta = beta,
          emis = dotCall64::vector_dc("double", nrowv * ncolv),
          INTENT = c(
            rep("r", 7), "w"
          ),
          PACKAGE = "vein",
          VERBOSE = 1
        )$emis
      }
      e <- data.frame(emissions = a)
      e <- Emissions(e)
      e$rows <- rep(row.names(veh), ncolv)
      e$age <- rep(seq(1, ncolv), each = nrowv)
    } else {
      e <- unlist(lapply(1:ncol(veh), function(j) {
        unlist(beta) * as.numeric(lkm[j]) * veh[, j] * as.numeric(ef[j]) * as.numeric(efcold[, j])
      }))
      e <- as.data.frame(e)
      names(e) <- "emissions"
      e <- Emissions(e)
      e$rows <- row.names(veh)
      e$age <- rep(1:ncol(veh), each = nrow(veh))
    }

    if (!missing(params)) {
      if (!is.list(params)) stop("'params' must be a list")
      if (is.null(names(params))) {
        if (verbose) message("Adding names to params")
        names(params) <- paste0("P_", 1:length(params))
      }
      for (i in 1:length(params)) {
        e[, names(params)[i]] <- params[[i]]
      }
    }
    if (verbose) cat("Sum of emissions:", sum(e$emissions), "\n")
  }


  return(e)
}
