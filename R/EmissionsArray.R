#' Construction function for class "EmissionsArray"
#'
#' @description Returns a tranformed object with class "EmissionsArray".
#'
#' @return Objects of class "EmissionsArray"
#'
#' @param x Object with class "data.frame", "matrix" or "numeric"
#' @param object object with class "EmissionsArray'
#' @param ... ignored
#' @rdname EmissionsArray
#' @aliases EmissionsArray print.EmissionsArray summary.EmissionsArray
#' plot.EmissionsArray
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
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1,
#' isList = T)
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
#' class(E_CO)
#' summary(E_CO)
#' E_CO
#' plot(E_CO)
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
if (is.array(e)) {
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
    if (length(dim(e)) == 4 ) {
      df <- Emissions(t(apply(e, c(3, 4), sum, na.rm=T)))
      names(df) <- unlist(lapply(1:ncol(df), function(i) paste0("h",i)))
      # Total <- rowSums(df)
      Mean <- colMeans(df)
      SD <- unlist(lapply(df, stats::sd, na.rm = T))
      Min <- unlist(lapply(df, min))
      Max <- unlist(lapply(df, max))
      dfx <- data.frame(Mean, SD, Min, Max)
      cat("Hourly emissions\n")
      return(dfx)
    } else if (length(dim(e)) == 3 ){
      #TODO: improve
      apply(e, c(1, 3), sum, na.rm=T)
    }
}

#' @rdname EmissionsArray
#' @method plot EmissionsArray
#' @export
plot.EmissionsArray <- function(x, ...) {
  e <- x
  if (length(dim(e)) == 4 ) {
    df <- Emissions(t(apply(e, c(3, 4), sum, na.rm=T)))
    names(df) <- unlist(lapply(1:ncol(df), function(i) paste0("h",i)))
  Mean <- colMeans(df)
  SD <- unlist(lapply(df, stats::sd))
  Min <- unlist(lapply(df, min))
  Max <- unlist(lapply(df, max))
  dfx <- data.frame(Mean, SD, Min, Max)
  graphics::plot(y = dfx$Mean, x = 1:nrow(dfx),
                 ylim = c(min(dfx$Min), max(dfx$Max)),
                 col = "red", type = "l",
                 ...)
  graphics::lines(dfx$Mean+dfx$SD, ylim = c(min(dfx$Min), max(dfx$Max)))
  graphics::lines(dfx$Mean-dfx$SD, ylim = c(min(dfx$Min), max(dfx$Max)))
  graphics::points(dfx$Max, ylim = c(min(dfx$Min), max(dfx$Max)))
  graphics::points(dfx$Min, ylim = c(min(dfx$Min), max(dfx$Max)))
  } else {
  df <- Emissions(apply(e, c(1, 3), sum, na.rm=T))
  graphics::plot(df)
}
}
