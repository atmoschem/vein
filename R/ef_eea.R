#' Emissions factors from European European Environment Agency
#'
#' \code{\link{ef_cetesb}} returns a vector or data.frame of Brazilian emission factors.
#' @param category String: "PC" (Passenger Cars), "LCV" (Light Commercial Vehicles),
#' "TRUCKS" (Heavy Duty Trucks),
#' "BUS" (Buses) or "MC" (Motorcycles or L-Category as in EEA 2019).
#' @param fuel String;  "G", "G HY", "G PHEV G",
#' "G PHEV ELEC",  "D", "D PHEV D",
#' "D PHEV ELEC", "LPG BIFUEL LPG", "LPG BIFUEL G",
#' "CNG BIFUEL CNG", "CNG BIFUEL G", "D HY D",
#' "D HY ELEC", "CNG", "BIO D"
#' @param segment String for type of vehicle (try different, the function will show values).
#' @param euro String; euro standard:
#' "PRE", "IMPROVED CONVENTIONAL", "OPEN LOOP",
#' "ECE 15/00-01", "ECE 15/02", "ECE 15/03", "ECE 15/04".
#' "I", "II", "III", "IV", "V",
#' "VI A/B/C", "VI D", "VI D-TEMP", "VI D/E",
#' "EEV".
#' @param tech String; technology:
#' "DPF", "DPF With S/W Update", "DPF+SCR"
#' "EGR", "GDI", "GDI+GPF", "LNT+DPF", "PFI", "SCR".
#' @param pol String; "CO", "NOx", "VOC", "PM Exhaust", "EC", "CH4", "NH3", "N2O"
#' @param mode String; "Urban Peak", "Urban Off Peak", "Rural", "Highway", NA.
#' @param slope Numeric; 0.00, -0.06, -0.04, -0.02,  0.02,  0.04,  0.06, or NA
#' @param load Numeric; 0.0,0.5, 1.0 or NA
#' @param speed Numeric; optional numeric in km/h.
#' @return Return a function depending of speed or numeric (g/km)
#' @importFrom data.table setDT
#' @keywords  emission factors
#' @export
#' @examples {
#' # ef_eea(category = "I DONT KNOW")
#' ef_eea(category = "PC",
#' fuel = "G",
#' segment = "Small",
#' euro = "I",
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
