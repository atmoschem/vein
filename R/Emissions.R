#' Construction function for class "Emissions"
#'
#' @description \code{Emissions} returns a tranformed object with class "Emissions".
#' The type of objects supported are of classes "matrix", "data.frame" and
#' "numeric". If the class of the object is "matrix" this function returns a
#' dataframe.
#'
#' @return Objects of class "Emissions" or "units"
#'
#' @param x Object with class "data.frame", "matrix" or "numeric"
#' @param object object with class "Emissions"
#' @param pal Palette of colors available or the number of the position
#' @param rev Logical; to internally revert order of rgb color vectors.
#' @param time Character to be the time units as denominator, eg "1/h"
#' @param fig1 par parameters for fig, \code{\link{par}}.
#' @param mai1 par parameters for mai, \code{\link{par}}.
#' @param fig2 par parameters for fig, \code{\link{par}}.
#' @param mai2 par parameters for mai, \code{\link{par}}.
#' @param fig3 par parameters for fig, \code{\link{par}}.
#' @param mai3 par parameters for mai, \code{\link{par}}.
#' @param ... ignored
#' @importFrom units as_units as_units
#' @importFrom graphics par plot abline
#' @importFrom fields image.plot
#'
#' @rdname Emissions
#' @aliases Emissions print.Emissions summary.Emissions plot.Emissions
#' @examples \dontrun{
#' data(net)
#' data(pc_profile)
#' data(fe2015)
#' data(fkm)
#' PC_G <- c(33491,22340,24818,31808,46458,28574,24856,28972,37818,49050,87923,
#'           133833,138441,142682,171029,151048,115228,98664,126444,101027,
#'           84771,55864,36306,21079,20138,17439, 7854,2215,656,1262,476,512,
#'           1181, 4991, 3711, 5653, 7039, 5839, 4257,3824, 3068)
#' veh <- data.frame(PC_G = PC_G)
#' pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
#' pcw <- temp_fact(net$ldv+net$hdv, pc_profile)
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' pckm <- units::as_units(fkm[[1]](1:24), "km"); pckma <- cumsum(pckm)
#' cod1 <- emis_det(po = "CO", cc = 1000, eu = "III", km = pckma[1:11])
#' cod2 <- emis_det(po = "CO", cc = 1000, eu = "I", km = pckma[12:24])
#' #vehicles newer than pre-euro
#' co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
#' cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
#' lef <- ef_ldv_scaled(co1, cod, v = "PC",  cc = "<=1400",
#'                      f = "G", p = "CO", eu=co1$Euro_LDV)
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed, agemax = 41,
#'              profile = pc_profile)
#' dim(E_CO) # streets x vehicle categories x hours x days
#' class(E_CO)
#' plot(E_CO)
#' ####
#' Emissions(1, time = "1/h")
#' }
#' @export
Emissions <- function(x, time, ...) {

  if(inherits(x, "sf")) {
    geo <- sf::st_geometry(x)

    e <- sf::st_set_geometry(x, NULL)

    for(i in 1:ncol(e)){
      e[,i] <- e[,i]*units::as_units("g")
    }

    if(!missing(time)){
      for(i in 1:ncol(e)) e[,i] <- e[,i]*units::as_units(1, time)
    }
    e <- sf::st_sf(e, geometry = geo)

  } else if ( is.matrix(x) ) {

    e <- as.data.frame(x)

    for(i in 1:ncol(e)){
      e[,i] <- e[,i]*units::as_units("g")
    }

    if(!missing(time)){
      for(i in 1:ncol(e)) e[,i] <- e[,i]*units::as_units(1, time)
    }

    class(e) <- c("Emissions", class(e))

  } else if ( is.data.frame(x) ) {

    e <- x

    for(i in 1:ncol(x)){
      e[,i] <- e[,i]*units::as_units("g")
    }

    if(!missing(time)){
      for(i in 1:ncol(e)) e[,i] <- e[,i]*units::as_units(1, time)
    }

    class(e) <- c("Emissions",class(x))

  } else if ( class(x) == "units" ) {

    e <- x

    if(units(x)$numerator != "g") stop("units are not 'g'")

  } else if( class(x) == "numeric" | class(x) == "integer") {

    e <- x*units::as_units("g")

    if(!missing(time)){
      e <- e*units::as_units(1, time)
    }
  }
  return(e)
}

#' @rdname Emissions
#' @method print Emissions
#' @export
print.Emissions <- function(x, ...) {
  nr <- ifelse(nrow(x) <= 5, nrow(x), 5)
  if(ncol(x) == 1) {
    ndf <- names(x)
    df <- data.frame(ndf = x[1:nr, ])
    names(df) <- ndf
    print.data.frame(df)
  } else {
    print.data.frame(x[1:nr, ])
  }
  if(nrow(x) > 5)     cat(paste0("... and ", nrow(x) - 5, " more rows\n"))
}


#' @rdname Emissions
#' @method summary Emissions
#' @export
summary.Emissions <- function(object, ...) {
  e <- object
  avemi <- sum(seq(1,ncol(e))*colSums(e)/sum(e))
  cat("Total emissions by column in study area = \n")
  print(summary(colSums(e)))
  cat("\nAverage = ", round(avemi,2))
  cat(" \n\n")
  cat("Emissions by street in study area = \n")
  print(summary(rowSums(e)))
  cat(" \n\n")
  cat("Emissions by column and street in study area = \n")
  print(summary(unlist(e)))
}


#' @rdname Emissions
#' @method plot Emissions
#' @export
plot.Emissions <- function(x,
                           pal = "colo_angelafaye_Coloured_sky_in",
                           rev = FALSE,
                           fig1 = c(0,0.8,0,0.8),
                           fig2 = c(0,0.8,0.55,1),
                           fig3 = c(0.7,1,0,0.8),
                           mai1 = c(0.2, 0.82, 0.82, 0.42),
                           mai2 = c(1.3, 0.82, 0.82, 0.42),
                           mai3 = c(0.7, 0.72, 0.82, 0.42),
                           ...) {
  oldpar <- par(no.readonly = TRUE)       # code line i
  on.exit(par(oldpar))                    # code line i + 1
  # e <- x
  #
  if(ncol(x) > 1) {
    graphics::par(fig=fig1, #new=TRUE,
                  mai = mai1,
                  ...)

    fields::image.plot(
      x = 1:ncol(x),
      xaxt = "n",
      z =t(as.matrix(x))[, nrow(x):1],
      xlab = "",
      ylab = paste0("Emissions by streets [",as.character(units(x[[1]])), "]"),
      col = cptcity::cpt(pal = pal, rev = rev), horizontal = TRUE)

    graphics::par(fig=fig2,
                  mai = mai2,
                  new=TRUE,
                  ...)
    avage <- sum(seq(1,ncol(x)) * colSums(x)/sum(x))
    graphics::plot(colSums(x, na.rm = T),
                   type="l",
                   ylab = paste0("Emissions [",as.character(units(x[[1]])), "]"),
                   xlab = "",
                   frame = FALSE,
                   xaxt = 'n')
    graphics::axis(3)

    graphics::abline(v = avage, col="red")
    cat("Weighted mean = ",round(avage,2), "\n")

    graphics::par(fig=fig3, new=TRUE,
                  mai = mai3,
                  ...)
    graphics::plot(x = rowSums(x, na.rm = T), y = nrow(x):1,
                   type = "l", frame = FALSE, yaxt = "n", xlab = '',
                   ylab = paste0("Emissions [",as.character(units(x[[1]])), "]")
    )
    graphics::abline(v = avage, col="red")

  } else {
    graphics::plot(unlist(x), type = "l", main = "1 column data")
  }
}
