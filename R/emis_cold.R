#' Estimation of cold start emissions hourly for the of the week
#'
#' The vehicular emissions are estimated as the product of the vehicles on a
#' road, length of the road, emission factor avaliated at the respective speed.
#' The estimation considers beta
#' parameter, the fraction of mileage driven
#'
#' @param veh Numeric vector with length of elements equals to number of streets
#' @param lkm Length of each link
#' @param ef List of functions of emission factors
#' @param efcold List of functions of cold start emission factors
#' @param beta Datraframe with the hourly cold-start distribution to each day
#' of the period. Number of rows are hours and columns are days
#' @param speed List of speeds
#' @param agemax Age of oldest vehicles for that category
#' @param profile Numerical or dataframe with nrows equal to 24 and ncol 7 day
#' of the week
#' @param hour Number of considered hours in estimation
#' @param day Number of considered days in estimation
#' @param array When FALSE produces a dataframe of the estimation. When TRUE
#' expects a profile as a dataframe producing an array with dimensions
#' (streets x columns x hours x days)
#' @return emission estimation  g/h
#' @export
#' @note Actually dcold is not necessary, it would be enough to multiply
#' an existing cold-start distribution with the daily profile, but it was added
#' because it is important to clarify both, the data and the concepts
#' @examples \dontrun{
#' # Do not run
#' }
emis_cold <- function (veh, lkm, ef, efcold, beta, speed, agemax, profile,
                       hour = 1, day = 1, array = F) {
  if(array == F){
    lista <- lapply(1:day,function(j){
      lapply(1:hour,function(i){
        lapply(1:agemax, function(k){
          beta[i,j]*veh[, k]*profile[i,j]*lkm*ef[[k]](speed[[j]][[i]])*
            ifelse((efcold[[i]](speed[[j]][[i]])-1)<0,0,
                   (efcold[[i]](speed[[j]][[i]])-1))
          })
      })
    })
    return(lista)
  } else {
  d <-  simplify2array(
    lapply(1:day,function(j){
      simplify2array(
        lapply(1:hour,function(i){
          simplify2array(
            lapply(1:agemax, function(k){
              beta[i,j]*veh[, k]*profile[i,j]*lkm*ef[[k]](speed[[j]][[i]])*
                ifelse((efcold[[i]](speed[[j]][[i]])-1)<0,0,
                       (efcold[[i]](speed[[j]][[i]])-1))
              })
            )
          })
        )
      })
    )
  return(d)
  }
}

