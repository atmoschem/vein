#' Summary of EmissionsArray
#'
#' @description Summary of the object with class "EmissionsArray".
#' The summary method consists in plotting a message with the structure of
#' the list and indicating the summary of the first numeric element. The
#' dimensions of must be number of streets, number of vehicle categories,
#' number of hours and number of days.  This functions first performs an
#' \code{\link{apply}} over the array
#'
#' @return Summary for class "EmissionsArray". It returns a dataframe with
#' the mean values, standard deviation, min and max according the specification
#' in the argument 'by'
#' @param by Character value. When by is "day" it returns a dataframe with
#' columns mean, standard deviation (sd), min and max emissions based on hoyly
#' total emissions and rows as days of the week. When by is "hour" it returns
#' hourly mean, sd, min and max considering all day sof the week. When by is
#' "col" it returns a data.frame with mean, sd, min and max hourly emissions
#' considering each type of vehicle
#' @param ... ignored
#' @seealso \code{\link{apply}}
#' @rdname summary.EmissionsArray
#' @export
EmissionsArray <- function(e, ...) {
  UseMethod("EmissionsArray", e)
}
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
#' summary(as.Emissions(E_CO))
#' summary(as.Emissions(E_CO), by="hour")
#' summary(as.Emissions(E_CO), by="col")
#'}
summary.EmissionsArray <- function(e, by = "day") {
  if ( class(e) != "EmissionsArray" && !is.array(e) ) {
    stop("Not an EmissionsArray")
  } else if ( by == "day" ) {
    df <- as.data.frame(apply(e, c(3, 4), sum, na.rm=T))
    names(df) <- c("Mon", "Tue","Wed", "Thu", "Fri", "Sat", "Sun")
    # Total <- colSums(df)
    Mean <- colMeans(df)
    SD <- unlist(lapply(df, stats::sd))
    Min <- unlist(lapply(df, min))
    Max <- unlist(lapply(df, max))
    dfx <- data.frame(Mean, SD, Min, Max)
    cat("Daily emissions calculated from hourly totals\n")
    return(dfx)
  } else if ( by == "hour" ) {
    df <- as.Emissions(t(apply(e, c(3, 4), sum, na.rm=T)))
    names(df) <- unlist(lapply(1:ncol(df), function(i) paste0("h",i)))
    # Total <- rowSums(df)
    Mean <- colMeans(df)
    SD <- unlist(lapply(df, stats::sd))
    Min <- unlist(lapply(df, min))
    Max <- unlist(lapply(df, max))
    dfx <- data.frame(Mean, SD, Min, Max)
    cat("Hourly emissions\n")
    return(dfx)
  } else if ( by == "col") {
    df <- as.Emissions(t(apply(e, c(2, 3), sum, na.rm=T)))
    names(df) <- unlist(lapply(1:ncol(df), function(i) paste0("col",i)))
    Mean <- colMeans(df)
    SD <- unlist(lapply(df, stats::sd))
    Min <- unlist(lapply(df, min))
    Max <- unlist(lapply(df, max))
    dfx <- data.frame(Mean, SD, Min, Max)
    cat("Hourly emissions by column\n")
    return(dfx)
  }
}
