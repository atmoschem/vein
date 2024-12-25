#' Returns amount of vehicles at each age
#'
#' @family age
#'
#' @description \code{\link{age_ldv}} returns amount of vehicles at each age
#'
#' @param x Numeric; numerical vector of vehicles with length equal to lines features of road network
#' @param name Character; of vehicle assigned to columns of dataframe
#' @param a Numeric; parameter of survival equation
#' @param b Numeric; parameter of survival equation
#' @param agemin Integer; age of newest vehicles for that category
#' @param agemax Integer; age of oldest vehicles for that category
#' @param k Numeric; multiplication factor. If its length is > 1, it must match the length of x
#' @param bystreet Logical; when TRUE it is expecting that 'a' and 'b' are numeric vectors with length equal to x
#' @param net SpatialLinesDataFrame or Spatial Feature of "LINESTRING"
#' @param verbose Logical;  message with average age and total numer of vehicles
#' @param namerows Any vector to be change row.names. For instance, name of
#' regions or streets.
#' @param time Character to be the time units as denominator, eg "1/h"
#' @return dataframe of age distrubution of vehicles
#' @importFrom sf st_sf st_as_sf
#' @note
#' The functions age* produce distribution of the circulating fleet by age of use.
#' The order of using these functions is:
#'
#' 1. If you know the distribution of the vehicles by age of use , use:  \code{\link{my_age}}
#' 2. If you know the sales of vehicles, or the registry of new vehicles,
#' use \code{\link{age}} to apply a survival function.
#' 3. If you know the theoretical shape of the circulating fleet and you can use
#' \code{\link{age_ldv}}, \code{\link{age_hdv}} or \code{\link{age_moto}}. For instance,
#' you dont know the sales or registry of vehicles, but somehow you know
#' the shape of this curve.
#' 4. You can use/merge/transform/adapt any of these functions.
#' @export
#' @note It consists in a Gompertz equation with default parameters from
#' 1 national emissions inventory for green housegases in Brazil, MCT 2006
#' @examples \dontrun{
#' data(net)
#' PC_E25_1400 <- age_ldv(x = net$ldv, name = "PC_E25_1400")
#' plot(PC_E25_1400)
#' PC_E25_1400 <- age_ldv(x = net$ldv, name = "PC_E25_1400", net = net)
#' plot(PC_E25_1400)
#' }
age_ldv <- function (x,
                     name = "age",
                     a = 1.698,
                     b = -0.2,
                     agemin = 1,
                     agemax = 50,
                     k = 1,
                     bystreet = F,
                     net,
                     verbose = FALSE,
                     namerows,
                     time){
  .Deprecated("age_veh")
}
