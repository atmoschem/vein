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
#' @param efcold Data.frame or list of data.frames. When it is a data.frame, each column is for each
#' type of vehicle by age of use, rows are are each simple feature. When it is
#' a list of data.frames, each element of the list it is a month of the year.
#' efcold should be different for each simple feature, assuming different average temperature.
#' @param beta Data.frame with the fraction of cold starts. The rows are the fraction
#' for each spatial feature or subregion, the columns are the age of use of vehicle.
#' @param pro_month Numeric; montly profile to distribuite annual mileage in each month.
#' @param params Character; Add columns with information to returning data.frame
#' @param verbose Logical; To show more information
#' @return Emissions data.frame
#' @export
#' @examples \dontrun{
#' # Do not run
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
    stop("lkm neeeds to has class 'units' in 'km'. Please, check package 'units'")
  }
  if(units(lkm)$numerator == "m" ){
    stop("Units of lkm is 'm' ")
  }
  if(class(ef) != "units"){
    stop("ef neeeds to has class 'units' in 'g/km'. Please, check package 'units'")
  }
  if(units(ef)$numerator != "g" | units(ef)$denominator != "km"){
    stop("Units of g must be 'g/km' ")
  }
  lkm <- as.numeric(lkm)
  ef <- as.numeric(ef)

  # Checking sf
  if(any(class(veh) %in% "sf")){
    if(verbose) message("converting sf to data.frame")
    veh <- sf::st_set_geometry(veh, NULL)
  }
# checking beta
  beta <- as.data.frame(beta)

  # Checking pro_month
  if(!missing(pro_month)){
    if(length(pro_month) != 12) stop("Length of pro_month must be 12")
    mes <- ifelse(nchar(1:12)<2, paste0(0, 1:12), 1:12)

#TODO, IMPROVE
  e <- do.call("rbind",lapply(1:12, function(k){
    do.call("rbind", lapply(1:nrow(veh), function(j){
      dfi <- do.call("cbind",lapply(1:ncol(veh), function(i){
        beta[j, k]*as.numeric(lkm)[i]*veh[j, i] * pro_month[k] *as.numeric(ef)[i] * efcold[[k]][j, i]
      }))
      dfi <- as.data.frame(dfi)
      names(dfi) <- paste0("Age", 1:ncol(dfi))
      if(!missing(params)){
        for (i in 1:length(params)){
          e <- as.data.frame(e)
          e[, params[i]] <- params[i]
        }
      }
      dfi$month <- mes[j]
      dfi
    }))
  }))
  if(verbose) cat("Sum of emissions:", sum(e[-"month"]), "\n")
  } else {
    e <- Emissions(do.call("rbind", lapply(1:nrow(veh), function(j){
        dfi <- do.call("cbind",lapply(1:ncol(veh), function(i){
          beta[j, ]*as.numeric(lkm)[i]*veh[j, i] *as.numeric(ef)[i] * efcold[j, i]
        }))
        dfi <- as.data.frame(dfi)
        names(dfi) <- paste0("Age", 1:ncol(dfi))
      })))
    if(!missing(params)){
      for (i in 1:length(params)){
        e <- as.data.frame(e)
        e[, params[i]] <- params[i]
      }
    }

  }


  return(e)
}
