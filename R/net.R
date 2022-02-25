#' Road network of the west part of Sao Paulo city
#'
#' This dataset is an sf class object with roads
#' from a traffic simulation made by CET Sao Paulo, Brazil
#'
#' @format A Spatial data.frame (sf) with 1796 rows and 1 variables:
#' \describe{
#'   \item{ldv}{Light Duty Vehicles (veh/h)}
#'   \item{hdv}{Heavy Duty Vehicles (veh/h)}
#'   \item{lkm}{Length of the link (km)}
#'   \item{ps}{Peak Speed (km/h)}
#'   \item{ffs}{Free Flow Speed (km/h)}
#'   \item{tstreet}{Type of street}
#'   \item{lanes}{Number of lanes per link}
#'   \item{capacity}{Capacity of vehicles in each link (1/h)}
#'   \item{tmin}{Time for travelling each link (min)}
#'   \item{geometry}{geometry}
#' }
#' @usage data(net)
#' @docType data
"net"
