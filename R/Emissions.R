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
#' @param ... ignored
#' @importFrom units as_units
#'
#' @rdname Emissions
#' @aliases Emissions print.Emissions summary.Emissions plot.Emissions
#' @examples {
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
#' pckm <- units::set_units(fkm[[1]](1:24), "km"); pckma <- cumsum(pckm)
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
#' }
#' @export
Emissions <- function(x, ...) {
  if ( is.matrix(x) ) {
    e <- as.data.frame(x)
    for(i in 1:ncol(e)){
      e[,i] <- e[,i]*units::as_units("g")
    }
    class(e) <- c("Emissions", class(e))
  } else if ( is.data.frame(x) ) {
    e <- x
    for(i in 1:ncol(x)){
      e[,i] <- e[,i]*units::as_units("g")
    }
    class(e) <- c("Emissions",class(x))
  } else if ( class(x) == "units" ) {
    e <- x
    if(units(x)$numerator != "g") stop("units are not 'g'")
  } else if( class(x) == "numeric" | class(x) == "integer") {
    e <- x*units::as_units("g")
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
plot.Emissions <- function(x,  ...) {
  e <- x
  avage <- sum(seq(1,ncol(e)) * colSums(e)/sum(e))
  Emission <- Emissions(colSums(e))
  graphics::plot(Emission, type="l", ...)
  graphics::abline(v = avage, col="red")
  cat("\nAverage = ",round(avage,2))
}
