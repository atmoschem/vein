#' Estimation of hot exhaust emissions with top-down approach
#'
#' @description \code{\link{emis_hot_td}} estimates cld start emissions with
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
#' @param ef Numeric or data.frame; emission factors. When it is a data.frame
#' number of rows can be for each region, or also, each region repeated
#' along 12 months. For instance, if you have 10 regions the number
#' of rows of ef can also be 120 (10 * 120).
#' when you have emission factors that varies with month, see \code{\link{ef_china}}.
#' @param pro_month Numeric or data.frame; montly profile to distribuite annual mileage
#' in each month. When it is a data.frame, each region (row) can have a different
#' monthly profile.
#' @param params List of parameters; Add columns with information to returning data.frame
#' @param verbose Logical; To show more information
#' @return Emissions data.frame
#' @seealso \code{\link{ef_ldv_speed}} \code{\link{ef_china}}
#' @export
#' @examples {
#' # Do not run
#' euros <- c("V", "V", "IV", "III", "II", "I", "PRE", "PRE")
#' efh <- ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
#'           eu = euros, p = "CO", speed = Speed(34))
#' lkm <- units::as_units(18:10, "km")*1000
#' veh_month <- c(rep(8, 1), rep(10, 5), 9, rep(10, 5))
#' veh <- age_ldv(1:10, agemax = 8)
#' a <- emis_hot_td(veh = veh, lkm = lkm, ef = efh, verbose = TRUE)
#' head(a)
#' plot(aggregate(a$emissions, by = list(a$age), sum)$x,type ="b")
#' emis_hot_td(veh = veh, lkm = lkm, ef = efh, verbose = TRUE,
#' params = list(paste0("data_", 1:10), "moredata"))
#' aa <- emis_hot_td(veh = veh, lkm = lkm, ef = efh,
#' pro_month = veh_month, verbose = TRUE)
#' head(aa)
#' aa <- emis_hot_td(veh = veh, lkm = lkm, ef = efh,
#' pro_month = veh_month, verbose = FALSE,
#' params = list(paste0("data_", 1:10), "moredata"))
#' }
emis_hot_td <- function (veh,
                          lkm,
                          ef,
                          pro_month,
                          params,
                          verbose = FALSE) {
  # Check units
  if(class(lkm) != "units"){
    stop("lkm neeeds to has class 'units' in 'km'. Please, check package '?units::set_units'")
  }
  if(units(lkm)$numerator == "m" ){
    stop("Units of lkm is 'm' ")
  }
  if(units(lkm)$numerator == "km" ) {
    lkm <- as.numeric(lkm)
  }

  # Checking ef
  if(is.matrix(ef) | is.data.frame(ef)){
    ef <- as.data.frame(ef)
    if(class(ef[, 1]) != "units"){
      stop("columns of ef must has class 'units' in 'g/km'. Please, check package '?units::set_units'")
    }
    if(units(ef[, 1])$numerator != "g" | units(ef[, 1])$denominator != "km"){
      stop("Units of ef must be 'g/km' ")
    }
    if(units(ef[, 1])$numerator == "g" | units(ef[, 1])$denominator == "km"){
      for(i in 1:ncol(veh)){
        ef[, i] <- as.numeric(ef[, i])
      }
    }
    # When row = 1, transform to vector.
    if(nrow(ef) == 1){
      ef <- as.numeric(ef)
    }

  } else {
    if(class(ef) != "units"){
      stop("ef must has class 'units' in 'g/km'. Please, check package '?units::set_units'")
    }
    if(units(ef)$numerator != "g" | units(ef)$denominator != "km"){
      stop("Units of ef must be 'g/km' ")
    }
    if(units(ef)$numerator == "g" | units(ef)$denominator == "km"){
      ef <- as.numeric(ef)
    }

  }
  # Checking veh
  for(i in 1:ncol(veh)){
    veh[, i] <- as.numeric(veh[, i])
  }

  # Checking sf
  if(any(class(veh) %in% "sf")){
    if(verbose) message("converting sf to data.frame")
    veh <- sf::st_set_geometry(veh, NULL)
  }

  # pro_month
  if(!missing(pro_month)){
    if(is.data.frame(pro_month) | is.matrix(pro_month)){
      pro_month <- as.data.frame(pro_month)
      for(i in 1:nrow(pro_month)){
        pro_month[i, ] <- pro_month[i, ]/sum(pro_month[i, ])
      }
    } else if (is.numeric(pro_month)){
      pro_month <- pro_month/sum(pro_month)
    }
  }

  # Checking pro_month
  if(!missing(pro_month)){
    if(verbose) message("Estimation with monthly profile")
    if(length(pro_month) != 12) stop("Length of pro_month must be 12")
    mes <- ifelse(nchar(1:12)<2, paste0(0, 1:12), 1:12)
    if(is.data.frame(ef)){
      if(verbose) message("Assuming you have emission factors for each simple feature and then for each month")

      #when pro_month varies in each simple feature
      if(is.data.frame(pro_month) & nrow(ef) == nrow(veh)){
        if(verbose) message("'pro_month' is data.frame and number of rows of 'ef' and 'veh' are equal")
        if(nrow(ef) != nrow(veh)) stop("Number of rows of 'veh' and 'ef' must be equal")
        e <- do.call("rbind",lapply(1:12, function(k){
          dfi <- unlist(lapply(1:ncol(veh), function(i){
            lkm[i]*veh[, i] * pro_month[,k] *ef[,i]
          }))
          dfi <- as.data.frame(dfi)
          names(dfi) <- "emissions"
          dfi <- Emissions(dfi)
          dfi$rows <- row.names(veh)
          dfi$age <- rep(1:ncol(veh), each = nrow(veh))
          dfi$month <- (1:length(pro_month))[k]
          dfi
        }))

      } else if(is.numeric(pro_month) & nrow(ef) == nrow(veh)){
        if(verbose) message("'pro_month' is numeric and number of rows of 'ef' and 'veh' are equal")
        if(nrow(ef) != nrow(veh)) stop("Number of rows of 'veh' and 'ef' must be equal")
        e <- do.call("rbind",lapply(1:12, function(k){
          dfi <- unlist(lapply(1:ncol(veh), function(i){
            lkm[i]*veh[, i] * pro_month[k] *ef[,i]
          }))
          dfi <- as.data.frame(dfi)
          names(dfi) <- "emissions"
          dfi <- Emissions(dfi)
          dfi$rows <- row.names(veh)
          dfi$age <- rep(1:ncol(veh), each = nrow(veh))
          dfi$month <- (1:length(pro_month))[k]
          dfi
        }))

      } else if(is.data.frame(pro_month) & nrow(ef) == 12*nrow(veh)){
        if(verbose) message("'pro_month' is data.frame and number of rows of 'ef' is 12*number of rows 'veh'")
        ef$month <- rep(1:12, each = nrow(veh))
        ef <- split(ef, ef$month)

        e <- do.call("rbind",lapply(1:12, function(k){
          dfi <- unlist(lapply(1:ncol(veh), function(i){
            lkm[i]*veh[, i] * pro_month[,k] *ef[[k]][,i]
          }))
          dfi <- as.data.frame(dfi)
          names(dfi) <- "emissions"
          dfi <- Emissions(dfi)
          dfi$rows <- row.names(veh)
          dfi$age <- rep(1:ncol(veh), each = nrow(veh))
          dfi$month <- (1:length(pro_month))[k]
          dfi
        }))
      } else if(is.numeric(pro_month) & nrow(ef) == 12*nrow(veh)){
        if(verbose) message("'pro_month' is numeric and number of rows of 'ef' is 12*number of rows 'veh'")
        ef$month <- rep(1:12, each = nrow(veh))
        ef <- split(ef, ef$month)

        e <- do.call("rbind",lapply(1:12, function(k){
          dfi <- unlist(lapply(1:ncol(veh), function(i){
            lkm[i]*veh[, i] * pro_month[k] *ef[[k]][,i]
          }))
          dfi <- as.data.frame(dfi)
          names(dfi) <- "emissions"
          dfi <- Emissions(dfi)
          dfi$rows <- row.names(veh)
          dfi$age <- rep(1:ncol(veh), each = nrow(veh))
          dfi$month <- (1:length(pro_month))[k]
          dfi
        }))
      } else if(nrow(ef) !=  nrow(veh) & nrow(ef) != 12)(
        stop("The number of rows can be equal to number of rows of veh, or number of rows of veh times 12")
      )

      if(!missing(params)){
        if(!is.list(params)) stop("'params' must be a list")
        if(is.null(names(params))) {
          if(verbose) message("Adding names to params")
          names(params) <- paste0("P_", 1:length(params))
        }
        for (i in 1:length(params)){
          e[, names(params)[i]] <- params[[i]]
        }
      }

      if(verbose) cat("Sum of emissions:", sum(e$emissions), "\n")
    } else if(!is.data.frame(ef)){
      if(verbose) message("Assuming you have the same emission factors in each simple feature")

      # when pro_month vary each month
      if(is.data.frame(pro_month)){
        if(verbose) message("'pro_month' is data.frame and 'ef' is numeric")
        if(length(ef) != nrow(veh)) stop("Number of rows of 'veh' and length of 'ef' must be equal")
        e <- do.call("rbind",lapply(1:12, function(k){
          dfi <- unlist(lapply(1:ncol(veh), function(i){
            lkm[i]*veh[, i] * pro_month[, k] *ef[i]
          }))
          dfi <- as.data.frame(dfi)
          names(dfi) <- "emissions"
          dfi <- Emissions(dfi)
          dfi$rows <- row.names(veh)
          dfi$age <- rep(1:ncol(veh), each = nrow(veh))
          dfi$month <- (1:length(pro_month))[k]
          dfi
        }))

      } else if(is.numeric(pro_month)){
        if(verbose) message("'pro_month' is numeric and 'ef' is numeric")
        if(length(ef) != nrow(veh)) stop("Number of rows of 'veh' and length of 'ef' must be equal")
        e <- do.call("rbind",lapply(1:12, function(k){
          dfi <- unlist(lapply(1:ncol(veh), function(i){
            lkm[i]*veh[, i] * pro_month[k] *ef[i]
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
      if(!missing(params)){
        if(!is.list(params)) stop("'params' must be a list")
        if(is.null(names(params))) {
          if(verbose) message("Adding names to params")
          names(params) <- paste0("P_", 1:length(params))
        }
        for (i in 1:length(params)){
          e[, names(params)[i]] <- params[[i]]
        }
      }

      if(verbose) cat("Sum of emissions:", sum(e$emissions), "\n")
    }


  } else {
    if(verbose) message("Estimation without monthly profile")

    if(!is.data.frame(ef)) {
      if(verbose) message("'ef' is numeric")
      if(length(ef) != nrow(veh)) stop("Number of rows of 'veh' and length of 'ef' must be equal")
      e <-  unlist(lapply(1:ncol(veh), function(i){
        lkm[i]*veh[, i] *ef[i]
      }))
      e <- as.data.frame(e)
      names(e) <- "emissions"
      e <- Emissions(e)
      e$rows <- row.names(e)
      e$age <- rep(1:ncol(veh), each = nrow(veh))
    } else {
      if(verbose) message("'ef' is data.frame")
      if(nrow(ef) != nrow(veh)) stop("Number of rows of 'ef' and 'veh' must be equal")
      e <-  unlist(lapply(1:ncol(veh), function(i){
        lkm[i]*veh[, i] *ef[, i]
      }))
      e <- as.data.frame(e)
      names(e) <- "emissions"
      e <- Emissions(e)
      e$rows <- row.names(e)
      e$age <- rep(1:ncol(veh), each = nrow(veh))

    }

    if(!missing(params)){
      if(!is.list(params)) stop("'params' must be a list")
      if(is.null(names(params))) {
        if(verbose) message("Adding names to params")
        names(params) <- paste0("P_", 1:length(params))
      }
      for (i in 1:length(params)){
        e[, names(params)[i]] <- params[[i]]
      }
    }
    if(verbose) cat("Sum of emissions:", sum(e$emissions), "\n")

  }


  return(e)
}
