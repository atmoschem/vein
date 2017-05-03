#' Summary of Emissions
#'
#' @description The object with class "Emissions" is esentially a
#' data.frame with columns as type of vehicles and rows as streets. The summary
#' method considers this with the argument by. When the argument by is "col"
#' it returns the summary of the sum of the columns via \code{\link{colSums}}.
#' When the by is "streets" it returns the summary of the sum of the row via
#' \code{\link{rowSums}}. When by is "all" it returns a summary of the object
#' as it was an vector of length 1L. When by is "default" it performs a
#' summary as data.frame vua \code{\link{summary.data.frame}}
#'
#' @return Summary for class "Emissions"
#' @param by Character to indicate the summary
#' @seealso \code{\link{summary}}
#' @export
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
#'              profile = pc_profile, hour = 24, day = 7, array = F)
#'}
summary.Emissions <- function(e, by = "col", ...) {
  if(by =="col") {
    avemi <- sum(seq(1,ncol(e))*colSums(e)/sum(e))
    cat("Total emissions by column in study area = \n")
    print(summary(colSums(e), ...))#
    cat("\nAverage = ", round(avemi,2))#,"  sd = ",round(sdage,2),"\n\n")
  } else if (by=="streets") {
    cat("Emissions by street in study area = \n")
    print(summary(rowSums(e)))
  } else if (by == "all") {
    cat("Emissions by column and street in study area = \n")
    print(summary(unlist(e), ...))
  } else if (by == "default") {
    cat("Summary for column by street = \n")
    print(summary.data.frame(e), ...)
  }
}
