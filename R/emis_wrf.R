#' Generates emissions dataframe to generate WRF-Chem inputs
#'
#' It returns a dataframes with columns lat, long, id, pollutants, local time
#' and GMT time. This dataframe has the proper format to be used with WRF
#' assimilation system: "Another Asimilation System 4 WRF (AAS4WRF)" as published
#' by Vera-Vala et al (2016)
#'
#' @param sdf Grid emissions, which can be a SpatialPolygonsDataFrame, or a list
#' of  SpatialPolygonsDataFrame
#' @param nr Number of repetitions of the emissions period
#' @param dmyhm String indicating Day Month Year Hour and Minute in the format
#' "d-m-Y H:M" e.g.: "01-05-2014 00:00" It represents the time of the first
#' hour of emissions in Local Time
#' @param tz Time zone as required in for function \code{\link{as.POSIXct}}
#' @param utc interger indicating the difference between local and GMT time
#' @param islist logical value to indicate if sdf is a list or not
#' @return data-frame of gridded emissions  g/h
#' @export
#' @note The reference of the emissions assimilation system is Vara-Vela, A.,
#' Andrade, M. F., Kumar, P., Ynoue, R. Y., and Munoz, A. G.: Impact of
#' vehicular emissions on the formation of fine particles in the Sao Paulo
#' Metropolitan Area: a numerical study with the WRF-Chem model, Atmos. Chem.
#' Phys., 16, 777-797, doi:10.5194/acp-16-777-2016, 2016.
#' A good website with timezones is http://www.timezoneconverter.com/cgi-bin/tzc
#' @examples \dontrun{
#' # Do not run
#' }
emis_wrf <- function(sdf,nr, dmyhm, tz, utc, islist){
  if(nr <= 0){
    stop("The argument 'nr' must be positive")
  } else if (islist==FALSE) {
    dft <- as.data.frame(coordinates(sdf))

    dft$id <- 1:nrow(dft)

    dft <- do.call("rbind", replicate(ncol(sdf), dft, simplify = FALSE))

    dft$pol <-  unlist(lapply(1:(ncol(sdf)),function(i) {
      as.numeric(sdf@data[, i])
      })
      )
    names(dft) <- c("long", "lat", "id", "pollutant")

    dft <- do.call("rbind", replicate(nr, dft, simplify = FALSE))

    tzz <- ifelse(utc!=0,(-utc)*3600, 0)

    time_lt <- as.POSIXct(x = dmyhm, format="%d-%m-%Y %H:%M", tz=tz)

    dft$time_lt = rep(seq.POSIXt(from = time_lt,
                                 by = "1 hour",
                                 length.out = ncol(sdf)*nr),
                      each=nrow(sdf))

    dft$time_utc = dft$time_lt + tzz

  } else if (class(sdf)!="list" & islist==TRUE) {

    stop("The argument 'sdf' must be a list")

    } else if (class(sdf)=="list" & islist==TRUE) {
    dft <- as.data.frame(coordinates(sdf[[1]]))
    dft$id <- 1:nrow(sdf[[1]])

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

    names(dft) <- c("long", "lat", "id", names(sdf))

    dft <- do.call("rbind", replicate(nr, dft, simplify = FALSE))

    tzz <- ifelse(utc!=0,(-utc)*3600, 0)

    time_lt <- as.POSIXct(x = dmyhm, format="%d-%m-%Y %H:%M", tz=tz)

    dft$time_lt = rep(seq.POSIXt(from = time_lt,
                                 by = "1 hour",
                                 length.out = ncol(sdf[[1]])*nr),
                      each=nrow(sdf[[1]]))
    dft$time_utc = dft$time_lt + tzz

  }


  return(dft)
}
