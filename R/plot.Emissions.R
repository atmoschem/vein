#' Plot of Emissions
#'
#' @description The object with class "Emissions" is esentially a data.frame
#' with columns as type of vehicles and rows as streets. Therefore the plot
#' seeks to inform the total number by type of vehicles
#'
#' @param e An object with class "Emissions"
#' @param by Character that determines the type of plot. It accept the values:
#' "col", "streets" and "default". When by is "col" it is shown a plot of the
#' sum of the columns (See \code{\link{colSums}}). When by is "streets" performs
#' sum of the streets via \code{\link{rowSums}}. When by is "default" it plots
#' the default method for plot (See \code{\link{plot}}).
#' @param mean a logical value. When mean is TRUE and by is "col" it adds
#' the weighted mean with and when by is "streets" the mean.
#' @param xlab xlab
#' @param ... ignored
#'
#' @name plot
#' @rdname plot.Emissions
#' @name plot.Emissions
#' @title plot
#' @aliases NULL
NULL
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
#' pcw <- vein::temp_fact(net$ldv+net$hdv, pc_profile)
#' speed <- vein::netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1,
#'                         isList = T)
#' pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)
#' cod1 <- emis_det(po = "CO", cc = 1000, eu = "III", km = pckma[1:11])
#' cod2 <- emis_det(po = "CO", cc = 1000, eu = "I", km = pckma[12:24])
#' #vehicles newer than pre-euro
#' co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
#' cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
#' lef <- ef_ldv_scaled(co1, cod, v = "PC", t = "ALL", cc = "ALL",
#'                      f = "G",p = "CO", eu=co1$Euro_LDV)
#' lef <- c(lef,lef[length(lef)],lef[length(lef)],lef[length(lef)],
#'          lef[length(lef)],lef[length(lef)])
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed, agemax = 41,
#'              profile = pc_profile, hour = 24, day = 7, array = T)
#' plot(as.Emissions(apply(E_CO, c(3,2), sum)), by="col", mean = T)
#' plot(as.Emissions(apply(E_CO, c(2,3), sum)), by="col")
#' plot(as.Emissions(apply(E_CO, c(1,3), sum)), by="streets", mean = T)
#' plot(as.Emissions(apply(E_CO, c(1,4), sum)), by="streets", mean = T)
#' }
#' @export
plot.Emissions <- function(e, by = "col", mean = F, xlab = "Index", ...) {
  if ( by == "default" ) {
    graphics::plot(e, xlab = xlab, ...)
  } else if ( by=="col" && mean == FALSE ){
    Emission <- Emissions(colSums(e))
    graphics::plot(Emission, type="l",xlab = xlab, ...)
  } else if ( by=="col" && mean == TRUE ){
    avage <- sum(seq(1,ncol(e)) * colSums(e)/sum(e))
    units(avage) <- with(units::ud_units, g/h)
    Emission <- Emissions(colSums(e))
    graphics::plot(Emission,xlab = xlab, type="l",
                   main=paste(deparse(substitute(e))), ...)
    graphics::abline(v = avage, col="red")
    cat("\nAverage = ",round(avage,2))
  } else if (by=="streets" && mean == FALSE){
    Emission <- Emissions(rowSums(e))
    graphics::plot(Emission, type="l", xlab = xlab, ...)
  } else if (by=="streets" && mean == TRUE){
    avveh <- mean(rowSums(e), na.rm=T)
    Emission <- Emissions(rowSums(e))
    graphics::plot(Emission, xlab = xlab, type="l",
                   main=paste(deparse(substitute(e))), ...)
    graphics::abline(h =  avveh, col="red")
    cat("\nAverage = ",round(avveh,2))
  }
}
