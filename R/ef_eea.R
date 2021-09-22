#' Emissions factors from European European Environment Agency
#'
#' \code{\link{ef_cetesb}} returns a vector or data.frame of Brazilian emission factors.
#' @param category String: "Passenger Cars", "Light Commercial Vehicles", "Heavy Duty Trucks",
#' "Buses" or "L-Category".
#' @param fuel String;  "Petrol", "Petrol Hybrid", "Petrol PHEV ~ Petrol",
#' "Petrol PHEV ~ Electricity",  "Diesel", "Diesel PHEV ~ Diesel",
#' "Diesel PHEV ~ Electricity", "LPG Bifuel ~ LPG", "LPG Bifuel ~ Petrol",
#' "CNG Bifuel ~ CNG", "CNG Bifuel ~ Petrol", "Diesel Hybrid ~ Diesel",
#' "Diesel Hybrid ~ Electricity", "CNG", "Biodiesel"
#' @param segment String for type of vehicle.
#' @param euro String; euro standard.
#' @param tech String; technology.
#' @param pol String; "CO", "NOx", "VOC", "PM Exhaust", "EC", "CH4", "NH3", "N2O"
#' @param mode String; "Urban Peak", "Urban Off Peak", "Rural", "Highway", NA.
#' @param slope Numeric; 0.00, -0.06, -0.04, -0.02,  0.02,  0.04,  0.06, or NA
#' @param load Numeric; 0.0,0.5, 1.0 or NA
#' @param speed Numeric; optional numeric in km/h.
#' @return Return a function depending of speed or numeric (g/km)
#' @importFrom data.table setDT
#' @keywords  emission factors
#' @export
#' @examples \dontrun{
#' # ef_eea(category = "I DONT KNOW")
#' ef_eea(category = "Passenger Cars",
#' fuel = "Petrol",
#' segment = "Small",
#' euro = "Euro 1",
#' tech = NA,
#' pol = "CO",
#' mode = NA,
#' slope = 0,
#' load = 0)(10)
#' }
ef_eea <- function(
  category,
  fuel,
  segment,
  euro,
  tech,
  pol,
  mode,
  slope,
  load,
  speed
) {

  eea <- data.table::setDT(sysdata$eea)

  eea$RoadSlope[is.na(eea$RoadSlope)] <- 0
  eea$Load[is.na(eea$Load)] <- 0

  Category <- Fuel <- Segment <- EuroStandard <- Technology <- NULL
  Pollutant <- Mode <- RoadSlope <- Load <-  NULL

  # category
  u_cat <- unique(eea$Category)
  if(any(!category %in% u_cat)) {
    stop("Select categories from:\n", paste(u_cat, collapse = "\n"))
  }
  ef <- eea[Category %in% category]

  # fuel
  u_fuel <- unique(ef$Fuel)
  if(any(!fuel %in% u_fuel)) {
    stop("Select categories from:\n", paste(u_fuel, collapse = "\n"))
  }
  ef <- ef[Fuel %in% fuel]

  # segment
  u_segment <- unique(ef$Segment)
  if(any(!segment %in% u_segment)) {
    stop("Select categories from:\n", paste(u_segment, collapse = "\n"))
  }
  ef <- ef[Segment %in% segment]

  # euro
  u_euro <- unique(ef$EuroStandard)
  if(any(!euro %in% u_euro)) {
    stop("Select categories from:\n", paste(u_euro, collapse = "\n"))
  }
  ef <- ef[EuroStandard %in% euro]

  # tech
  u_tech <- unique(ef$Technology)
  if(any(!tech %in% u_tech)) {
    stop("Select categories from:\n", paste(u_tech, collapse = "\n"))
  }
  ef <- ef[Technology %in% tech]

  # pollutant
  u_pol <- unique(ef$Pollutant)
  if(any(!pol %in% u_pol)) {
    stop("Select categories from:\n", paste(u_pol, collapse = "\n"))
  }
  ef <- ef[Pollutant %in% pol]

  # mode
  u_mode <- unique(ef$Mode)
  if(any(!mode %in% u_mode)) {
    stop("Select categories from:\n", paste(u_mode, collapse = "\n"))
  }
  ef <- ef[Mode %in% mode]

  # slope
  u_slope <- unique(ef$RoadSlope)
  if(any(!slope %in% u_slope)) {
    stop("Select categories from:\n", paste(u_slope, collapse = "\n"))
  }
  ef <- ef[RoadSlope %in% slope]

  # load
  u_load <- unique(ef$Load)
  if(any(!slope %in% u_slope)) {
    stop("Select categories from:\n", paste(u_load, collapse = "\n"))
  }
  ef <- ef[Load %in% load]

  names(ef)

  # EF = (Alpha x V2 + Beta x V + Gamma + Delta / V) / (Epsilon x V2 + Zeta x V + Eta) x (1 - RF)

  alpha <- ef$Alpha
  beta <- ef$Beta
  gamma <- ef$Gamma
  delta <- ef$Delta
  epsilon <- ef$Epsilon
  zeta <- ef$Zita
  eta <- ef$Hta
  RF <- ef$ReductionFactor_perc
  minv <- ef$MinSpeed_kmh
  maxv <- ef$MaxSpeed_kmh

  fv <- function(V){
    V <- ifelse(V < minv, minv, ifelse(V > maxv, maxv, V))

    return((alpha*V*V + beta*V + gamma + delta / V) / (epsilon*V*V + zeta*V + eta)*(1-RF))

  }

  if(missing(speed)) {
    return(fv)
  } else {
    return(EmissionFactors(fv(speed)))
  }
}
