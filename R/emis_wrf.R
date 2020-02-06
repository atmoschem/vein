#' Generates emissions dataframe to generate WRF-Chem inputs
#'
#' \code{emis_wrf} returns a dataframes with columns lat, long, id, pollutants, local time
#' and GMT time. This dataframe has the proper format to be used with WRF
#' assimilation system: "ASimilation System 4 WRF (AS4WRF Vera-Vala et al (2016))
#'
#' @param sdf Gridded emissions, which can be a SpatialPolygonsDataFrame, or a list
#' of SpatialPolygonsDataFrame, or a sf object of "POLYGON". The user must enter
#' a list with 36 SpatialPolygonsDataFrame with emissions for the mechanism CBMZ.
#' @param nr Number of repetitions of the emissions period
#' @param dmyhm String indicating Day Month Year Hour and Minute in the format
#' "d-m-Y H:M" e.g.: "01-05-2014 00:00" It represents the time of the first
#' hour of emissions in Local Time
#' @param tz Time zone as required in for function \code{\link{as.POSIXct}}
#' @param crs Coordinate reference system, e.g: "+init=epsg:4326". Used to
#' transform the coordinates of the output
#' @param utc ignored.
#' @param islist logical value to indicate if sdf is a list or not
#' @seealso \code{\link{vein-deprecated}}
#' @keywords internal
#' @export
#' @examples {
#' # Do not run
#' }
emis_wrf <- function(sdf, nr = 1, dmyhm, tz, crs = 4326, islist){
  eixport::to_as4wrf(sdf = sdf, nr = nr, dmyhm = dmyhm, tz = tz, crs = crs, islist = islist)
}
