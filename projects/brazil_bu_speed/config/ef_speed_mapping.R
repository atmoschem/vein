# Mapping of CETESB vehicle categories to EMEP/EEA driving-cycle categories
# used to scale the local (CETESB) emission factors by speed with
# ef_ldv_scaled() / ef_hdv_scaled().
#
# engine : "ldv" uses ef_ldv_scaled(), "hdv" uses ef_hdv_scaled()
# v,t,cc,f,g : EMEP/EEA categories (see ?ef_ldv_speed and ?ef_hdv_speed)
# eu_col : column of ef_cetesb(full = TRUE) with the Euro equivalence

ef_speed_mapping <- data.frame(
  veh = c(
    "PC_G", "PC_E", "PC_FG", "PC_FE",
    "LCV_G", "LCV_E", "LCV_FG", "LCV_FE", "LCV_D",
    "TRUCKS_SL_D", "TRUCKS_L_D", "TRUCKS_M_D", "TRUCKS_SH_D", "TRUCKS_H_D",
    "BUS_URBAN_D", "BUS_MICRO_D", "BUS_COACH_D",
    "MC_150_G", "MC_150_500_G", "MC_500_G",
    "MC_150_FG", "MC_150_500_FG", "MC_500_FG",
    "MC_150_FE", "MC_150_500_FE", "MC_500_FE"
  ),
  engine = c(rep("ldv", 9), rep("hdv", 8), rep("ldv", 9)),
  v = c(rep("PC", 4), rep("LCV", 5), rep("Trucks", 5),
        "Ubus", "Ubus", "Coach", rep("Motorcycle", 9)),
  t = c(rep("4S", 4), rep("4S", 5), rep("RT", 5),
        "Std", "Midi", "3Axes", rep("4S", 9)),
  cc = c(rep("1400_2000", 4), rep("<3.5", 5), rep(NA, 8),
         rep("<=250", 3), rep("250_750", 3), rep(">=750", 3)),
  f = c(rep("G", 4), "G", "G", "G", "G", "D",
        rep("D", 8), rep("G", 9)),
  g = c(rep(NA, 9),
        "<=7.5", ">7.5 & <=12", ">12 & <=14", ">14 & <=20", ">32",
        ">15 & <=18", "<=15", ">18", rep(NA, 9)),
  eu_col = c(rep("EqEuro_PC", 4), rep("EqEuro_LCV", 5), rep("Euro_EqHDV", 8),
             rep("Euro_EqMoto", 9)),
  stringsAsFactors = FALSE
)

# Pollutants with speed-dependent curves in EMEP/EEA (others fall back to a
# constant CETESB emission factor)
pol_speed_ldv <- c("CO", "HC", "NMHC", "NOx", "CO2", "PM",
                   "NO2", "NO", "CH4", "SO2")
pol_speed_hdv <- c("CO", "HC", "NMHC", "NOx", "CO2", "PM",
                   "NO2", "NO", "CH4", "SO2")

# Build a scaled speed emission factor list for one CETESB category + pollutant
ef_cetesb_speed <- function(veh, pol, year = 2018, agemax = 40,
                            scale = "default", SDC = 34.12, sppm) {
  m <- ef_speed_mapping[ef_speed_mapping$veh == veh, ]
  if (nrow(m) == 0) stop("No speed mapping for ", veh)
  full <- ef_cetesb(p = pol, veh = veh, year = year, agemax = agemax,
                    scale = scale, full = TRUE, sppm = sppm)
  eu <- full[[m$eu_col]]
  dfcol <- as.numeric(full[[pol]])
  if (m$engine == "ldv") {
    ef_ldv_scaled(dfcol = dfcol, SDC = SDC, v = m$v, t = m$t,
                  cc = m$cc, f = m$f, eu = eu, p = pol)
  } else {
    ef_hdv_scaled(dfcol = dfcol, SDC = SDC, v = m$v, t = m$t,
                  g = m$g, eu = eu, gr = 0, l = 0.5, p = pol)
  }
}
