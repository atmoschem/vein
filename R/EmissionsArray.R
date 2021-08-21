#' Construction function for class "EmissionsArray"
#'
#' @description \code{EmissionsArray} returns a tranformed object with class
#' "EmissionsArray" with 4 dimensios.
#'
#' @return Objects of class "EmissionsArray"
#'
#' @param x Object with class "data.frame", "matrix" or "numeric"
#' @param object object with class "EmissionsArray'
#' @param pal Palette of colors available or the number of the position
#' @param rev Logical; to internally revert order of rgb color vectors.
#' @param ... ignored
#' @importFrom  stats median quantile sd
#' @rdname EmissionsArray
#' @aliases EmissionsArray print.EmissionsArray summary.EmissionsArray
#' plot.EmissionsArray
#' @note Future version of this function will return an Array of 3 dimensions.
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
#' pckm <- units::set_units(fkm[[1]](1:24), "km"); pckma <- cumsum(pckm)
#' cod1 <- emis_det(po = "CO", cc = 1000, eu = "III", km = pckma[1:11])
#' cod2 <- emis_det(po = "CO", cc = 1000, eu = "I", km = pckma[12:24])
#' #vehicles newer than pre-euro
#' co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
#' cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
#' lef <- ef_ldv_scaled(co1, cod, v = "PC", cc = "<=1400",
#'                      f = "G",p = "CO", eu=co1$Euro_LDV)
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed, agemax = 41,
#'              profile = pc_profile, simplify = TRUE)
#' class(E_CO)
#' summary(E_CO)
#' E_CO
#' plot(E_CO)
#' lpc <- list(pc1, pc1)
#' E_COv2 <- emis(veh = lpc,lkm = net$lkm, ef = lef, speed = speed, agemax = 41,
#'              profile = pc_profile, hour = 2, day = 1)
#' }
#' @export
EmissionsArray <- function(x, ...) {
  e <- x
  if ( !is.array(e) ) {
    stop("Class of e must be 'array'")
  } else if ( is.array(e) ) {
    class(e) <- c("EmissionsArray",class(e))
  }
  return(e)
}

#' @rdname EmissionsArray
#' @method print EmissionsArray
#' @export
print.EmissionsArray <- function(x,  ...) {
  e <- x
  if (length(dim(e)) == 3) {
    cat("This EmissionsArray has\n", dim(e)[1], "streets\n",
        dim(e)[2], "vehicle categories\n", dim(e)[3], "hours\n")
    print(utils::head(e))
  } else {
    cat("This EmissionsArray has\n", dim(e)[1], "streets\n",
        dim(e)[2], "vehicle categories\n", dim(e)[3], "hours\n",
        dim(e)[4], "days\n")
    print(utils::head(e))

  }
}

#' @rdname EmissionsArray
#' @method summary EmissionsArray
#' @export
summary.EmissionsArray <- function(object, ...) {
  e <- object
  mine <- round(min(e, na.rm = T), 3)
  q1 <- round(stats::quantile(e, .25, na.rm = T), 3)
  mede <- round(stats::median(e, na.rm = T), 3)
  avge <- round(mean(e, na.rm = T), 3)
  q3 <- round(stats::quantile(e, .75, na.rm = T), 3)
  maxe <- round(max(e, na.rm = T), 3)
  sde <- round(stats::sd(e, na.rm = T), 3)
  a <- data.frame(Min = mine,
                  `Qu.1` = q1,
                  Median = mede,
                  Mean = avge,
                  `Qu.3` = q3,
                  Max = maxe,
                  sd = sde)
  row.names(a) <- NULL
  print(a)
}
  #' @rdname EmissionsArray
  #' @method plot EmissionsArray
  #' @export
  plot.EmissionsArray <- function(x, pal = "imagej_split_blrd_warmmetal", rev = TRUE, ...) {
    e <- x
    if (length(dim(e)) == 4 ) {
      df <- Emissions(t(apply(e, c(3, 4), sum, na.rm=T)))
      Mean <- colMeans(df)
      SD <- unlist(lapply(df, stats::sd))
      Min <- unlist(lapply(df, min))
      Max <- unlist(lapply(df, max))
      dfx <- data.frame(Mean, SD, Min, Max)
      graphics::plot(y = dfx$Mean, x = 1:nrow(dfx),
                     ylim = c(min(dfx$Min), max(dfx$Max)),
                     col = "red", type = "l",
                     main = "average emissions",
                     ...)
      graphics::lines(dfx$Mean+dfx$SD, ylim = c(min(dfx$Min), max(dfx$Max)))
      graphics::lines(dfx$Mean-dfx$SD, ylim = c(min(dfx$Min), max(dfx$Max)))
      graphics::points(dfx$Max, ylim = c(min(dfx$Min), max(dfx$Max)))
      graphics::points(dfx$Min, ylim = c(min(dfx$Min), max(dfx$Max)))
    } else {
      df <- Emissions(apply(e, c(1, 3), mean, na.rm=T))
      graphics::plot(df,
                     main = "average emissions",
                     ...)
    }
  }
