#' Base emissions factors from International Vehicle Emissions (IVE) model
#'
#' \code{\link{ef_ive}} returns the base emission factors from the the IVE
#' model. This function depend on vectorized mileage, which means your can
#' enter with the mileage by age of use and the name of the pollutant.
#'
#' @param description Character; "Auto/Sml Truck" "Truck/Bus" or"Sml Engine".
#' @param fuel Character; "Petrol", "NG Retrofit", "Natural Gas", "Prop Retro.",
#' "Propane", "EthOH Retrofit", "OEM Ethanol", "Diesel", "Ethanol" or "CNG/LPG".
#' @param weight Character; "Light", "Medium", "Heavy", "Lt", "Med" or "Hvy"
#' @param air_fuel_control Character; One of the following characters: "Carburetor",
#' "Single-Pt FI", "Multi-Pt FI", "Carb/Mixer", "FI", "Pre-Chamber Inject.",
#' "Direct Injection", "2-Cycle", "2-Cycle, FI", "4-Cycle, Carb", "4-Cycle, FI"
#' "4-Cycle"
#' @param exhaust Character: "None", "2-Way", "2-Way/EGR", "3-Way", "3-Way/EGR",
#' "None/EGR", "LEV", "ULEV", "SULEV", "EuroI", "EuroII", "EuroIII", "EuroIV",
#' "Hybrid", "Improved", "EGR+Improv", "Particulate", "Particulate/NOx",
#' "EuroV", "High Tech" or "Catalyst"
#' @param evaporative Character: "PCV", "PCV/Tank" or"None".
#' @param mileage Numeric; mileage of vehicle by age of use km.
#' @param pol Character; One of the following characters: "Carburetor",
#' "Single-Pt FI", "Multi-Pt FI", "Carb/Mixer", "FI", "Pre-Chamber Inject.",
#' "Direct Injection", "2-Cycle", "2-Cycle, FI", "4-Cycle, Carb", "4-Cycle, FI"
#' "4-Cycle"
#' #' \tabular{cccc}{
#'   "VOC_gkm" \tab "CO_gkm" \tab "NOx_gkm" \tab "PM_gkm" \cr
#'   "Pb_gkm" \tab "SO2_gkm" \tab "NH3_gkm" \tab "1,3_butadiene_gkm" \cr
#'   "formaldehyde_gkm" \tab "acetaldehyde_gkm" \tab "benzene_gkm" \tab "EVAP_gkm" \cr
#'   "CO2_gkm" \tab "N20_gkm" \tab "CH4_gkm" \tab "VOC_gstart" \cr
#'   "CO_gstart" \tab "NOx_gstart" \tab "PM_gstart" \tab "Pb_gstart" \cr
#'   "SO2_gstart" \tab "NH3_gstart" \tab "1,3_butadiene_gstart" \tab "formaldehyde_gstart" \cr
#'   "acetaldehyde_gstart" \tab "benzene_gstart" \tab "EVAP_gstart" \tab "CO2_gstart" \cr
#'   "N20_gstart" \tab "CH4_gstart" \tab   \tab  \cr
#' }
#' @param details Logical; option to see or not more information about vehicle.
#' @return An emission factor by annual mileage.
#' @keywords speed emission factors ive
#' @references Nicole Davis, James Lents, Mauricio Osses, Nick Nikkila,
#'  Matthew Barth. 2005. Development and Application of an International
#'  Vehicle Emissions Model. Transportation Research Board, 81st Annual Meeting,
#'  January 2005, Washington, D.C.
#' @export
#' @examples {
#' # Do not run
#' # Passenger Cars PC
#' data(fkm)
#' # cumulative mileage from 1 to 50 years of use, 40:50
#' mil <- cumsum(fkm$KM_PC_E25(1:50))
#' ef_ive("Truck/Bus", mileage = mil, pol = "CO_gkm")
#' ef_ive(mileage = mil, pol = "CO_gkm", details = TRUE)
#' }
ef_ive <- function(description = "Auto/Sml Truck",
                   fuel = "Petrol",
                   weight = "Light",
                   air_fuel_control = "Carburetor",
                   exhaust = "None",
                   evaporative = "PCV",
                   mileage,
                   pol,
                   details = FALSE){
  ive <- sysdata$ive
  a <- ive[ive$description == description &
             ive$fuel == fuel &
             ive$weight == weight &
             ive$air_fuel_control == air_fuel_control &
             ive$exhaust == exhaust &
             ive$evaporative == evaporative, ]
  if(nrow(a) == 0) stop("No data. Improve your selection")
  if(details) print(a$veh[1])
  FF <- a$f[[1]]
  if(pol %in% names(ive)[10:24]){
    ef <- vein::EmissionFactors(FF(mileage = mileage/1000, pol = pol, a = a))
    return(ef)
  } else if(pol %in% names(ive)[25:39]){
    cat("units in g/start\n")
    ef <- FF(mileage = mileage/1000, pol = pol, a = a)
    return(ef)
  }
}
