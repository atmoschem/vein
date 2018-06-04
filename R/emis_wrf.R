#' Generates emissions dataframe to generate WRF-Chem inputs (Deprecated, use exiport)
#'
#' \code{emis_wrf} returns a dataframes with columns lat, long, id, pollutants, local time
#' and GMT time. This dataframe has the proper format to be used with WRF
#' assimilation system: "Another Asimilation System 4 WRF (AAS4WRF)" as published
#' by Vera-Vala et al (2016)
#'
#' @param sdf Grid emissions, which can be a SpatialPolygonsDataFrame, or a list
#' of SpatialPolygonsDataFrame. The user must enter a list with 36
#' SpatialPolygonsDataFrame with emissions for the mechanism CBMZ. When there
#' are no emissions available, the SpatialPolygonsDataFrame must contain
#' 0.
#' @param nr Number of repetitions of the emissions period
#' @param dmyhm String indicating Day Month Year Hour and Minute in the format
#' "d-m-Y H:M" e.g.: "01-05-2014 00:00" It represents the time of the first
#' hour of emissions in Local Time
#' @param tz Time zone as required in for function \code{\link{as.POSIXct}}
#' @param crs Coordinate reference system, e.g: "+init=epsg:4326". Used to
#' transform the coordinates of the output
#' @param islist logical value to indicate if sdf is a list or not
#' @importFrom sp coordinates spTransform CRS
#' @return data-frame of gridded emissions  g/h
#' @export
#' @note DEPRECATED, use eixport https://CRAN.R-project.org/package=eixport
#' The reference of the emissions assimilation system is Vara-Vela, A.,
#' Andrade, M. F., Kumar, P., Ynoue, R. Y., and Munoz, A. G.: Impact of
#' vehicular emissions on the formation of fine particles in the Sao Paulo
#' Metropolitan Area: a numerical study with the WRF-Chem model, Atmos. Chem.
#' Phys., 16, 777-797, doi:10.5194/acp-16-777-2016, 2016.
#' A good website with timezones is http://www.timezoneconverter.com/cgi-bin/tzc
#' The crs is the same as used by \code{\link{sp}} package
#' It returns a dataframe with id,, long, lat, pollutants, time_lt, time_utc
#' and day-UTC-hour (dutch)
#' The pollutants for the CBMZ are: e_so2, e_no, e_ald, e_hcho, e_ora2, e_nh3
#' e_hc3, e_hc5, e_hc8, e_eth, e_co, e_ol2, e_olt, e_oli, e_tol, e_xyl, e_ket
#' e_csl, e_iso, e_no2, e_ch3oh, e_c2h5oh, e_pm25i, e_pm25j, e_so4i, e_so4j
#' e_no3i, e_no3j, e_orgi, e_orgj, e_eci, e_ecj, e_so4c, e_no3c, e_orgc, e_ecc
#' @seealso \code{\link{emis_post}} \code{\link{emis}}
#' @examples \dontrun{
#' # Do not run
#' }
emis_wrf <- function(sdf,nr = 1, dmyhm, tz, crs = "+init=epsg:4326", islist){
  .Deprecated(new = "to_wrf or to_as4wrf",
              package = 'eixport',
              msg = "use the function in 'eixport' https://CRAN.R-project.org/package=eixport")
  if(nr <= 0){
    stop("The argument 'nr' must be positive")
  } else if (islist == FALSE) {
    dft <- as.data.frame(sp::coordinates(
      sp::spTransform(sdf, CRSobj = sp::CRS(crs))))
    dftid <- data.frame(id = 1:nrow(dft))

    dft <- as.data.frame(cbind(dftid, dft))

    dft <- do.call("rbind", replicate(ncol(sdf), dft, simplify = FALSE))
    dft$pol <-  unlist(lapply(1:(ncol(sdf)),function(i) {
      as.numeric(sdf@data[, i])
      })
      )
    names(dft) <- c("id", "long", "lat", "pollutant")
    dft <- do.call("rbind", replicate(nr, dft, simplify = FALSE))
   # tzz <- ifelse(utc!=0,(-utc)*3600, 0)
    time_lt <- as.POSIXct(x = dmyhm, format="%d-%m-%Y %H:%M", tz=tz)
    dft$time_lt = rep(seq.POSIXt(from = time_lt,
                                 by = "1 hour",
                                 length.out = ncol(sdf)*nr),
                      each=nrow(sdf))
    dft$time_utc <- dft$time_lt
    attr(dft$time_utc, "tzone") <- "Etc/UTC"
    dft$dutch <- as.numeric(
      paste0(
        strftime(dft$time_utc, timezone = tz, format = "%d"),
        strftime(dft$time_utc, timezone = tz, format = "%H")
      ))
  } else if (class(sdf)!="list" & islist==TRUE) {
    stop("The argument 'sdf' must be a list")
    } else if (class(sdf)=="list" & islist==TRUE) {
    dft <- as.data.frame(sp::coordinates(
      sp::spTransform(sdf[[1]], CRSobj = sp::CRS(crs))))

    dftid <- data.frame(id = 1:nrow(sdf[[1]]))

    dft <- as.data.frame(cbind(dftid, dft))

    dft <- do.call("rbind", replicate(ncol(sdf[[1]]),
                                      dft, simplify = FALSE))
    dft <- cbind(dft,
                 as.data.frame(
                   do.call("cbind",
                           lapply(1:length(sdf),
                                  function(j) {
                                    unlist(lapply(1:ncol(sdf[[1]]),
                                                  function(i) {
                                                    as.numeric( sdf[[j]]@data [, i])
                                                    }
                                    ))
                                    }
                                  )))
                 )

    names(dft) <- c("id", "long", "lat",  names(sdf))
    dft <- do.call("rbind", replicate(nr, dft, simplify = FALSE))
  #  tzz <- ifelse(utc!=0,(-utc)*3600, 0)
    time_lt <- as.POSIXct(x = dmyhm, format="%d-%m-%Y %H:%M", tz=tz)
    dft$time_lt <- rep(seq.POSIXt(from = time_lt,
                                 by = "1 hour",
                                 length.out = ncol(sdf[[1]])*nr),
                      each=nrow(sdf[[1]]))
    dft$time_utc <- dft$time_lt
    attr(dft$time_utc, "tzone") <- "Etc/UTC"
    dft$dutch <- as.numeric(
      paste0(
        strftime(dft$time_utc, timezone = tz, format = "%d"),
        strftime(dft$time_utc, timezone = tz, format = "%H")
      ))
  }
  return(dft)
}
