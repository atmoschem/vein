#' Estimation of cold start emissions with top-down approach
#'
#' @description \code{emis_cold_td} estimates cld start emissions with
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
#' @return Emissions data.frame
#' @seealso \code{\link{ef_ldv_cold}}
#' @export
#' @examples \dontrun{
#' # Do not run
#' euros <- c("V", "V", "IV", "III", "II", "I", "PRE", "PRE")
#' dt <- matrix(rep(2:25,5), ncol = 12, nrow = 10) # 12 months, 10 rows
#' row.names(dt) <- paste0("Simple_Feature_", 1:10)
#' efc <- ef_ldv_cold(ta = dt, cc = "<=1400", f ="G", eu = euros, p = "CO", speed = Speed(34))
#' efh <- ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
#'           eu = euros, p = "CO", speed = Speed(34))
#' efh <- EmissionFactors(unlist(efh))
#' lkm <- units::as_units(18:10, "km")*1000
#' cold_lkm <- as.data.frame(sapply(1:ncol(dt), function(i) {
#' cold_mileage(ltrip = 20, ta = dt[, i])
#' }))
#' names(cold_lkm) <- paste0("Month_", 1:12)
#' veh_month <- c(rep(8, 1), rep(10, 5), 9, rep(10, 5))
#' veh <- age_ldv(1:10, agemax = 8)
#' emis_cold_td(veh = veh, lkm = lkm, ef = efh, efcold = efc[1:10, ],
#' beta = cold_lkm[,1], verbose = TRUE,)
#' emis_cold_td(veh = veh, lkm = lkm, ef = efh, efcold = efc[1:10, ],
#' beta = cold_lkm[,1], verbose = TRUE,
#' params = list(paste0("data_", 1:10), "moredata"))
#' aa <- emis_cold_td(veh = veh, lkm = lkm, ef = efh, efcold = efc,
#' beta = cold_lkm, pro_month = veh_month, verbose = TRUE)
#' aa <- emis_cold_td(veh = veh, lkm = lkm, ef = efh, efcold = efc,
#' beta = cold_lkm, pro_month = veh_month, verbose = FALSE,
#' params = list(paste0("data_", 1:10), "moredata"))
#' }
emis_cold_td <- function (veh,
                       lkm,
                       ef,
                       efcold,
                       beta,
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
  if(class(ef) != "units"){
    stop("ef must has class 'units' in 'g/km'. Please, check package '?units::set_units'")
  }
  if(units(ef)$numerator != "g" | units(ef)$denominator != "km"){
    stop("Units of ef must be 'g/km' ")
  }
  if(units(ef)$numerator == "g" & units(ef)$denominator == "km"){
    ef <- as.numeric(ef)
  }
  # Checking ef
  if(class(efcold[, 1]) != "units"){
    stop("columns of efcold must has class 'units' in 'g/km'. Please, check package '?units::set_units'")
  }
  if(units(efcold[, 1])$numerator != "g" | units(efcold[, 1])$denominator != "km"){
    stop("Units of efcold must be 'g/km' ")
  }
  if(units(efcold[, 1])$numerator == "g" & units(efcold[, 1])$denominator == "km"){
    for(i in 1:ncol(veh)){
      efcold[, i] <- as.numeric(efcold[, i])
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
# checking beta
  beta <- as.data.frame(beta)

  # Checking pro_month
  if(!missing(pro_month)){
    if(verbose) message("Estimation with monthly profile")
    if(length(pro_month) != 12) stop("Length of pro_month must be 12")
    mes <- ifelse(nchar(1:12)<2, paste0(0, 1:12), 1:12)

#TODO, IMPROVE
    if(verbose) message("Assuming you have emission factors for each simple feature and then for each month")

    efcold$month <- rep(1:12, each = nrow(veh))
    efcold <- split(efcold, efcold$month)

    e <- do.call("rbind",lapply(1:12, function(k){
        dfi <- do.call("cbind",lapply(1:ncol(veh), function(i){
          beta[i, k]*as.numeric(lkm)[i]*veh[, i] * pro_month[k] *as.numeric(ef)[i] * efcold[[k]][, i]
        }))
        dfi <- Emissions(dfi)
        names(dfi) <- paste0("Age", 1:ncol(dfi))
        dfi$month <- mes[k]
        dfi
    }))
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

  if(verbose) cat("Sum of emissions:", sum(e[, 1:ncol(veh)]), "\n")
  } else {
    if(verbose) message("Estimation without monthly profile")

    e <- dfi <- Emissions(do.call("cbind",lapply(1:ncol(veh), function(i){
          unlist(beta)[i]*as.numeric(lkm)[i]*veh[, i] *as.numeric(ef)[i] * efcold[, i]
        })))
        names(dfi) <- paste0("Age", 1:ncol(dfi))

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
    if(verbose) cat("Sum of emissions:", sum(e[, 1:ncol(veh)]), "\n")

  }


  return(e)
}
