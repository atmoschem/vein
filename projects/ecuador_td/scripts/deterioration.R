# Deterioration factors (EMEP/EEA guidebook, vein::emis_det) ####
# Sourced by scripts/hot_exhaust_eea.R and scripts/cold_start_eea.R.
#
# As in the EEA guidebook, deterioration applies only to petrol catalyst
# light vehicles: fuel "G" (metadata$fuel_eea_old), family PC (taxis
# included) or LCV, Euro I and later. Every other vehicle, Euro class and
# pollutant gets factor 1.
# emis_det covers CO, HC and NOx: NMHC uses the HC factor and NO/NO2 use the
# NOx factor. CO2, FC and PM are not deteriorated.

det_pollutant <- function(p) {
  switch(
    p,
    "CO" = "CO",
    "HC" = "HC",
    "NMHC" = "HC",
    "NOx" = "NOx",
    "NO" = "NOx",
    "NO2" = "NOx",
    NA_character_
  )
}

# engine-size class for emis_det: passenger cars use their own class
# (metadata$cc_eea_old); LCV (N1, "<3.5" or "N1-I/II/III") use the PC >2000
# curves, the same substitution used for the cold-start ratios
det_cc <- function(cc) {
  if (cc %in% c("<=1400", "1400_2000", ">2000")) cc else ">2000"
}

# one factor per age; km = cumulative mileage by age (units km)
det_factor <- function(p, family, fuel, cc, eu, km) {
  out <- rep(1, length(eu))
  po <- det_pollutant(p)
  if (is.na(po) || !family %in% c("PC", "LCV") || !fuel %in% "G") {
    return(out)
  }
  tab <- tryCatch(
    unique(vein:::sysdata$ldv_det$EURO),
    error = function(e) c("I", "II", "III", "IV", "V", "VI", "VIc")
  )
  # Euro 6d/e and Euro 7 use the latest curve available
  eu <- ifelse(eu %in% c("VId", "VII"), "VIc", eu)
  eu_tab <- tab[match(eu, trimws(tab))]
  ok <- !is.na(eu_tab) & eu != "PRE" # PRE: no catalyst, no deterioration
  if (!any(ok)) {
    return(out)
  }
  f <- emis_det(po = po, cc = det_cc(cc), eu = eu_tab[ok], km = km[ok])
  out[ok] <- pmax(as.numeric(f), 1)
  out
}

# multiply each age column k of `veh` by efdet[k]; returns a copy, so `veh`
# itself is never modified inside the pollutant loop
apply_det <- function(veh, efdet) {
  if (all(efdet == 1)) {
    return(veh)
  }
  if (length(efdet) != ncol(veh)) {
    stop("efdet needs one value per age column of veh")
  }
  out <- veh
  for (k in seq_len(ncol(veh))) out[[k]] <- veh[[k]] * efdet[k]
  out
}
