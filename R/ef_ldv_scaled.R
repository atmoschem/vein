#' Scaling constant with speed emission factors of Light Duty Vehicles
#'
#' This function creates a list of scaled functions of emission factors. A scaled
#' emission factor which at a speed of the driving cycle (SDC) gives a desired value.
#'
#' This function calls "ef_ldv_speed" and calculate the specific k value, dividing the
#' local emission factor by the respective speed emissions factor at the speed representative
#' of the local emission factor, e.g. If the local emission factors were tested with the
#' FTP-75 test procedure, SDC = 34.12 km/h.
#'
#' @param df Dataframe with local emission factor
#' @param dfcol Column of the dataframe with the local emission factors eg df$dfcol
#' @param SDC Speed of the driving cycle
#' @param v Category vehicle: "PC", "LCV", "Motorcycle" or "Moped
#' @param t Sub-category of of vehicle: "PRE_ECE", "ECE_1501", "ECE_1502",
#' "ECE_1503", "ECE_1504" , "IMPROVED_CONVENTIONAL", "OPEN_LOOP", "ALL",
#' "2S"  or "4S"
#' @param cc Size of engine in cc: "ALL", "<=1400", ">1400", "1400_2000", ">2000",
#' "<=800", "800_1400", "<=2000", "2S", "<=50", ">=50", "<=250", "250_750", ">=750",
#' or ">50"
#' @param f Type of fuel: "G", "D", "LPG" or "FH" (Full Hybrid: starts by electric motor)
#' @param eu Euro standard: "PRE", "I", "II", "III", "III+DPF", "IV", "V", "VI", "VIc"
#' or "ALL"
#' @param p Pollutant: "CO", "FC", "NOx", "HC" or "PM"
#' @return A list of scaled emission factors  g/km
#' @keywords speed emission factors
#' @note The length of the list should be equal to the name of the age categories of
#' a specific type of vehicle. Thanks to Glauber Camponogara by the help.
#' @seealso ef_ldv_seed
#' @export
#' @examples \dontrun{
#' # Do not run
#' data(fe2015)
#' co1 <- fe2015[fe2015$Pollutant=="CO" & fe2015$Age<25, ] #24 obs!!!
#' l1 <- ef_ldv_scaled(co1, co1$PC_G, v = "PC", t = "ALL", cc = "ALL", f = "G",
#' eu = co1$Euro_LDV, p = "CO")
#' co2 <- fe2015[fe2015$Pollutant=="CO" & fe2015$Age>24,] #22 obs!!!
#' l2 <- ef_ldv_scaled(co2$PC_G, v = "PC", t = "PRE_ECE", cc = "ALL", f = "G",
#' eu = co2$Euro_LDV, p = "CO")
#' FE_PC_E25_1400_CO <- c(l1,l2,l2[[12]],l2[[12]],l2[[12]],l2[[12]])
#' FE_PC_E25_1400_CO[[1]](34.12) #first element
#' length(FE_PC_E25_1400_CO)
#' }
ef_ldv_scaled <- function(df,dfcol ,SDC  = 34.12, v, t, cc, f, eu, p) {
   lapply(1:length(df[,1]), function(i)  {
    funIN <- ef_ldv_speed(v = v, t = t, cc = cc, f = f,
                         eu = as.character(eu[i]), p = p, k = 1, show.equation = FALSE)
    k <- dfcol[i]/ funIN(SDC)
    funOUT <- ef_ldv_speed(v = v, t = t, cc = cc, f = f,
                         eu = as.character(eu[i]), p = p, k = k,show.equation = FALSE)
    return(as.EmissionsFactors(funOUT))
   })
}
