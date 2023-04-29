#' Scaling constant with speed emission factors of Heavy Duty Vehicles
#'
#' @description \code{\link{ef_hdv_scaled}} creates a list of scaled functions of emission factors. A scaled
#' emission factor which at a speed of the dricing cycle (SDC) gives a desired value.
#' This function needs a dataframe with local emission factors with a columns with
#' the name "Euro_HDV" indicating the Euro equivalence standard, assuming that there are
#' available local emission factors for several consecutive years.
#'
#' @param dfcol Column of the dataframe with the local emission factors eg df$dfcol
#' @param SDC Speed of the driving cycle
#' @param v Category vehicle: "Coach", "Trucks" or "Ubus"
#' @param t Sub-category of of vehicle: "3Axes", "Artic", "Midi", "RT, "Std" and "TT"
#' @param g Gross weight of each category: "<=18", ">18", "<=15", ">15 & <=18", "<=7.5",
#' ">7.5 & <=12", ">12 & <=14", ">14 & <=20", ">20 & <=26", ">26 & <=28", ">28 & <=32",
#' ">32", ">20 & <=28", ">28 & <=34", ">34 & <=40", ">40 & <=50" or ">50 & <=60"
#' @param eu Euro emission standard: "PRE", "I", "II", "III", "IV" and "V"
#' @param gr Gradient or slope of road: -0.06, -0.04, -0.02, 0.00, 0.02. 0.04 or 0.06
#' @param l Load of the vehicle: 0.0, 0.5 or 1.0
#' @param p Pollutant: "CO", "FC", "NOx" or "HC"
#' @param df deprecated
#' @return A list of scaled emission factors g/km
#' @keywords speed emission factors
#' @note The length of the list should be equal to the name of the age categories of
#' a specific type of vehicle
#' @export
#' @examples {
#' # Do not run
#' CO <- ef_cetesb(p = "CO", veh = "TRUCKS_SL_D", full = TRUE)
#' lef <- ef_hdv_scaled(dfcol = CO$CO,
#'                      v = "Trucks",
#'                      t = "RT",
#'                      g = "<=7.5",
#'                      eu = CO$Euro_EqHDV,
#'                      gr = 0,
#'                      l = 0.5,
#'                      p = "CO")
#' length(lef)
#' ages <- c(1, 10, 20, 30, 40)
#' EmissionFactors(do.call("cbind",
#'    lapply(ages, function(i) {
#'        data.frame(i = lef[[i]](1:100))
#' }))) -> df
#' names(df) <- ages
#' colplot(df)
#' }
ef_hdv_scaled <- function(df,
                          dfcol,
                          SDC  = 34.12,
                          v,
                          t,
                          g,
                          eu,
                          gr = 0,
                          l = 0.5,
                          p) {
  if(length(dfcol) != length(eu)) stop("Length of dfcol must be the same as length of eu")
  dfcol <- as.numeric(dfcol)
  la <-  lapply(1:length(dfcol), function(i)  {
    funIN <- ef_hdv_speed(v = v,
                          t = t,
                          g = g,
                          eu = as.character(eu[i]),
                          gr = gr,
                          l = l,
                          p = p,
                          k=1,
                          show.equation = FALSE)
    k <- dfcol[i]/ funIN(SDC)
    ef_hdv_speed(v = v,
                 t = t,
                 g = g,
                 eu = as.character(eu[i]),
                 gr = gr,
                 l = l,
                 p = p,
                 k = k,
                 show.equation = FALSE)
  })
  class(la) <- c("EmissionFactorsList",class(la))
return(la)
}

