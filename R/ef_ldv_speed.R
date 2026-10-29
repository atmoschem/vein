#' Emissions factors for Light Duty Vehicles and Motorcycles
#'
#' \code{\link{ef_ldv_speed}} returns speed dependent emission factors, data.frames or
#' list of emission factors. The hot emission factors come from the
#' EMEP/EEA air pollutant emission inventory guidebook 2023 - update 2026,
#' chapter 1.A.3.b.i-iv, Appendix 4 (July 2026)
#' https://www.eea.europa.eu/en/analysis/publications/emep-eea-guidebook-2023
#'
#' The argument of this functions have several options which results in
#' different combinations that returns emission factors. If a combination is
#' not available the function stops with an error that shows the combination.
#'
#' @param v Character; category vehicle: "PC", "LCV", "Motorcycle" or "Moped
#' @param t Character; sub-category of of vehicle.
#' PC petrol: "4S" (port fuel injection from Euro 3), "GDI" (gasoline direct
#' injection, Euro 3 and later), "GDI_GPF" (GDI with particle filter, "VIc"
#' and later), "2S" (2-stroke, only conventional technology exists) and the
#' pre-Euro sub-technologies "ECE_1501", "ECE_1502", "ECE_1503", "ECE_1504",
#' "IMPROVED_CONVENTIONAL", "OPEN_LOOP" (with eu = "PRE").
#' PC diesel: "4S" (Euro 3 to 5 with DPF, Euro 6 and later DPF+SCR),
#' "DPF_SWU" (Euro 5 with software update), "DPF", "LNT_DPF", "DPF_SCR"
#' (Euro 6 and later). LCV: "4S". Motorcycle: "2S" or "4S". Moped: "2S" or "4S"
#' @param cc Character; size of engine in cc: PC: "<=800" (Mini, Euro 4 and
#' later), "<=1400" (Small), "1400_2000" (Medium), ">2000" (Large-SUV-Executive).
#' Motorcycle: ">=50" (for "2S"), "<=250", "250_750", ">=750".
#' Moped: "<=50". LCV: "<3.5" (equal to N1-III), "N1-I", "N1-II" or "N1-III".
#' @param f Character; type of fuel: "G", "D", "LPG" (bifuel in LPG mode),
#' "CNG" (bifuel in CNG mode, Euro 4 and later), "FH" (petrol full hybrid,
#' Euro 4 and later), "PHEV_G" and "PHEV_D" (plug-in hybrids in fuel mode,
#' Euro 6 and later). Full hybrid vehicles cannot be charged from the grid;
#' only its own engine recharges the batteries.
#' @param eu Character or data.frame of characters; euro standard:
#'  "PRE", "I", "II", "III", "III+DPF", "IV", "V", "VI" (Euro 6 a/b/c),
#'  "VIc" (Euro 6 d-temp), "VId" (Euro 6 d/e) or "VII" (Euro 7).
#'  Motorcycles and mopeds: "PRE" and "I" to "V".
#' @param p Character; pollutant: "CO", "FC", "EC" (energy consumption, MJ/km),
#' "NOx", "NO", "NO2", "HC", "PM", "NMHC", "CH4", "CO2", "SO2", "Pb",
#' "BC", "OM", "SPN23" (solid particle number > 23 nm, #/km, where available),
#' "N2O" and "NH3" (L-category only), metals ("As", "Cd", "Cr", "Cu", "Hg",
#' "Ni", "Se", "Zn"), NMHC species, polycyclic aromatic hydrocarbons (PAHs),
#' persistent organic pollutants (POPs), number of particles and active surface.
#' Only when p is "SO2" or "Pb" x is needed.
#' @param x Numeric; if pollutant is "SO2", it is sulphur in fuel in ppm, if is
#' "Pb", Lead in fuel in ppm.
#' @param k Numeric; multiplication factor
#' @param show.equation Logical; option to see or not the equation parameters
#' and the EEA row used.
#' @param speed Speed in km/h with class "Speed" or "units", to return the
#' emission factor at that speed instead of a function.
#' @param fcorr Numeric; Correction by fuel properties by euro technology.
#' See \code{\link{fuel_corr}}. The order from first to last is
#' "PRE", "I", "II", "III", "IV", "V", "VI" and any later standard. Default is 1
#' @return An emission factor function which depends of the average speed V  g/km
#' @keywords speed emission factors
#' @note Every path returns non-negative values: the speed is limited to the
#' validity range of the equation and negative values are set to 0.
#'
#' Emission factors given by EEA per driving mode (e.g. CH4 and PM of recent
#' petrol cars) use Urban for V < 50 km/h, Rural for 50 <= V < 90 and Highway
#' for V >= 90 km/h.
#'
#' FC comes from the energy consumption factors and the calorific values of
#' Table 3-28 of the guidebook. CO2 = FC * 44.011 / (12.011 + 1.008 rH:C +
#' 16 rO:C) with the ratios of Table 3-29, so CO2/FC is constant. NO2 and NO
#' use the f-NO2 fractions of Table 3-87, with NO expressed as NO2-equivalent
#' mass (NO = NOx * (1 - f-NO2)); use 46 g/mol for NO when converting to moles.
#'
#' \strong{Pollutants (g/km)}: "CO", "NOx", "HC", "PM", "CH4", "NMHC", "CO2", "SO2",
#' "Pb", "FC".
#'
#' \strong{Black Carbon and Organic Matter (g/km)}: "BC", "OM"
#'
#' \strong{PAH and POP (g/km)}:  \code{\link{speciate}}
#' \strong{Dioxins and furans(g equivalent toxicity / km)}:  \code{\link{speciate}}
#' \strong{Metals (g/km)}:  \code{\link{speciate}}
#'
#' \strong{NMHC (g/km)}: \code{\link{speciate}}
#'
#' \emph{Active Surface (cm2/km)}:  \code{\link{speciate}}"AS_urban", "AS_rural", "AS_highway"
#'
#' \emph{Total Number of particles (N/km)}: \code{\link{speciate}} "N_urban", "N_rural", "N_highway",
#' "N_50nm_urban", "N_50_100nm_rural", "N_100_1000nm_highway".
#'
#' The available standards for Active Surface or number of particles are Euro I,
#' II, III, III+DPF dor diesle and III+DISI for gasoline. Pre euro vehicles
#' has the value of Euro I and  euro IV, V, VI and VIc the value of euro III.
#' @seealso \code{\link{fuel_corr}} \code{\link{emis}} \code{\link{ef_ldv_cold}}
#' @export
#' @examples \dontrun{
#' # Passenger Cars PC
#' # Emission factor function
#' V <- 0:150
#' ef1 <- ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
#' p = "CO")
#' efs <- EmissionFactors(ef1(1:150))
#' plot(Speed(1:150), efs, xlab = "speed[km/h]", type = "b", pch = 16, col = "blue")
#'
#' # Quick view
#' pol <- c("CO", "NOx", "HC", "NMHC", "CH4", "FC", "PM", "CO2", "SO2",
#' "1-butyne", "propyne")
#' f <- sapply(1:length(pol), function(i){
#' ef_ldv_speed("PC", "4S", "<=1400", "G", "PRE", pol[i], x = 10)(30)
#' })
#' f
#' # PM Characteristics
#' pol <- c("AS_urban", "AS_rural", "AS_highway",
#' "N_urban", "N_rural", "N_highway",
#' "N_50nm_urban", "N_50_100nm_rural", "N_100_1000nm_highway")
#' f <- sapply(1:length(pol), function(i){
#' ef_ldv_speed("PC", "4S", "<=1400", "D", "PRE", pol[i], x = 10)(30)
#' })
#' f
#' # PAH POP
#' ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
#' p = "indeno(1,2,3-cd)pyrene")(10)
#' ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
#' p = "napthalene")(10)
#'
#' # Dioxins and Furans
#' ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
#' p = "PCB")(10)
#'
#' # NMHC
#' ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
#' p = "hexane")(10)
#'
#' # List of Copert emission factors for 40 years fleet of Passenger Cars.
#' # Assuming a euro distribution of euro V, IV, III, II, and I of
#' # 5 years each and the rest 15 as PRE euro:
#' euro <- c(rep("V", 5), rep("IV", 5), rep("III", 5), rep("II", 5),
#'           rep("I", 5), rep("PRE", 15))
#' speed <- 25
#' lef <- lapply(1:40, function(i) {
#' ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
#'           eu = euro[i], p = "CO")
#' ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
#'           eu = euro[i], p = "CO", show.equation = FALSE)(25) })
#' # to check the emission factor with a plot
#' efs <- EmissionFactors(unlist(lef)) #returns 'units'
#' plot(efs, xlab = "age")
#' lines(efs, type = "l")
#' euros <- c("VI", "V", "IV", "III", "II")
#' ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
#'           eu = euros, p = "CO")
#' a <- ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
#'           eu = euros, p = "CO", speed = Speed(0:120))
#' head(a)
#' filled.contour(as.matrix(a)[1:10, 1:length(euros)], col = cptcity::cpt(n = 18))
#' filled.contour(as.matrix(a)[110:120, 1:length(euros)], col = cptcity::cpt(n = 16))
#' filled.contour(as.matrix(a)[, 1:length(euros)], col = cptcity::cpt(n = 21))
#' filled.contour(as.matrix(a)[, 1:length(euros)],
#' col = cptcity::cpt("mpl_viridis", n = 21))
#' filled.contour(as.matrix(a)[, 1:length(euros)],
#' col = cptcity::cpt("mpl_magma", n = 21))
#' persp(as.matrix(a)[, 1:length(euros)], phi = 0, theta = 0)
#' persp(as.matrix(a)[, 1:length(euros)], phi = 25, theta = 45)
#' persp(as.matrix(a)[, 1:length(euros)], phi = 0, theta = 90)
#' persp(as.matrix(a)[, 1:length(euros)], phi = 25, theta = 90+45)
#' persp(as.matrix(a)[, 1:length(euros)], phi = 0, theta = 180)
#' new_euro <- c("VI", "VI", "V", "V", "V")
#' euro <- c("V", "V", "IV", "III", "II")
#' old_euro <- c("III", "II", "I", "PRE", "PRE")
#' meuros <- rbind(new_euro, euro, old_euro)
#' aa <- ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
#'           eu = meuros, p = "CO", speed = Speed(10:11))
#' # Light Commercial Vehicles
#' V <- 0:150
#' ef1 <- ef_ldv_speed(v = "LCV",t = "4S", cc = "<3.5", f = "G", eu = "PRE",
#' p = "CO")
#' efs <- EmissionFactors(ef1(1:150))
#' plot(Speed(1:150), efs, xlab = "speed[km/h]")
#' lef <- lapply(1:5, function(i) {
#' ef_ldv_speed(v = "LCV", t = "4S", cc = "<3.5", f = "G",
#'           eu = euro[i], p = "CO", show.equation = FALSE)(25) })
#' # to check the emission factor with a plot
#' efs <- EmissionFactors(unlist(lef)) #returns 'units'
#' plot(efs, xlab = "age")
#' lines(efs, type = "l")
#'
#' # Motorcycles
#' V <- 0:150
#' ef1 <- ef_ldv_speed(v = "Motorcycle",t = "4S", cc = "<=250", f = "G",
#' eu = "PRE", p = "CO",show.equation = TRUE)
#' efs <- EmissionFactors(ef1(1:150))
#' plot(Speed(1:150), efs, xlab = "speed[km/h]")
#' # euro for motorcycles
#' eurom <- c(rep("III", 5), rep("II", 5), rep("I", 5), rep("PRE", 25))
#' lef <- lapply(1:30, function(i) {
#' ef_ldv_speed(v = "Motorcycle", t = "4S", cc = "<=250", f = "G",
#' eu = eurom[i], p = "CO",
#' show.equation = FALSE)(25) })
#' efs <- EmissionFactors(unlist(lef)) #returns 'units'
#' plot(efs, xlab = "age")
#' lines(efs, type = "l")
#' a <- ef_ldv_speed(v = "Motorcycle", t = "4S", cc = "<=250", f = "G",
#' eu = eurom, p = "CO", speed = Speed(0:125))
#' a$speed <- NULL
#' filled.contour(as.matrix(a), col = cptcity::lucky(),
#' xlab = "Speed", ylab = "Age")
#' persp(x = as.matrix(a), theta = 35, xlab = "Speed", ylab = "Euros",
#' zlab = "CO [g/km]", col = cptcity::lucky(), phi = 25)
#'
#' ef <- ef_ldv_speed(v = "LCV",
#'                    t = "4S",
#'                    cc = "<3.5",
#'                    f = "G",
#'                    p = "FC",
#'                    eu = c("I", "II"),
#'                    speed = Speed(10))
#'
#' # plot speed data.frame by hour
#'
#' data(net)
#' data(pc_profile)
#' pc_week <- temp_fact(net$ldv + net$hdv, pc_profile)
#' df <- netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm)
#' lapply(1:ncol(df), function(i) {
#'  a <- ef_ldv_speed(
#'  v = "PC",
#'  t = "4S",
#'  cc = "<=1400",
#'  f = "G",
#'  eu = "III",
#'  p = "CO",
#'  speed = df[[i]]
#'  )
#' }) -> lef
#'  def <- EmissionFactors(remove_units(do.call("cbind", lef)))
#'  plot(def)
#' }
ef_ldv_speed <- function(
                         v,
                         t = "4S",
                         cc,
                         f,
                         eu,
                         p,
                         x,
                         k = 1,
                         speed,
                         show.equation = FALSE,
                         fcorr = rep(1, 8)
) {
  ef_ldv <- sysdata$ldv
  xas <- c("AS_urban", "AS_rural", "AS_highway")
  npm <- c(
    "N_urban",
    "N_rural",
    "N_highway",
    "N_50nm_urban",
    "N_50_100nm_rural",
    "N_100_1000nm_highway"
  )
  xx <- if (missing(x)) NULL else x

  # Check eu
  if (is.matrix(eu) | is.data.frame(eu)) {
    eu <- as.data.frame(eu)
    for (i in 1:ncol(eu)) {
      eu[, i] <- as.character(eu[, i])
    }
  } else {
    eu <- as.character(eu)
  }

  # Check speed
  if (!missing(speed)) {
    speed <- .speed_kmh(speed)
  }

  # Message for units
  if (!missing(speed)) {
    if (p %in% xas) {
      cat("Units of Active Surface: cm^2/km\n")
    } else if (p %in% npm) {
      cat("Units of Number of Particles: n/km\n")
    }
  }

  # EF function for one euro standard
  fun_eu <- function(e) {
    what <- paste(v, t, cc, f, e, p)
    df <- .ef_row(ef_ldv,
      list(VEH = v, TYPE = t, CC = cc, FUEL = f, EURO = e,
        POLLUTANT = p),
      what)
    .ef_speed_fun(df, k = k, k2 = .fcorr_k2(e, fcorr), x = xx, what = what)
  }

  # fun starts
  if (!is.data.frame(eu)) {
    if (length(eu) == 1) {
      f1 <- fun_eu(eu)
      if (show.equation == TRUE) {
        df <- .ef_row(ef_ldv,
          list(VEH = v, TYPE = t, CC = cc, FUEL = f, EURO = eu,
            POLLUTANT = p),
          paste(v, t, cc, f, eu, p))
        cat(paste0(
          "a = ", df$a, ", b = ", df$b, ", c = ", df$c, ", d = ", df$d,
          ", e = ", df$e, ", f = ", df$f, "\n"
        ))
        cat(paste0(
          "Equation = (", as.character(df$Y), ")*", k, "*",
          .fcorr_k2(eu, fcorr), "\n"
        ))
        if (!is.null(df$REF)) cat(paste0("Reference = ", df$REF, "\n"))
      }

      # check for speed as numeric or data.frame
      if (!missing(speed)) {
        if (is.data.frame(speed)) {
          return(EmissionFactors(as.data.frame(lapply(speed, f1))))
        }
        return(EmissionFactors(f1(speed)))
      }
      return(f1)
    } else if (length(eu) > 1) {
      lf <- lapply(eu, fun_eu)
      if (!missing(speed)) {
        if (is.data.frame(speed)) {
          stop(
            "When length(eu) > 1, 'speed' must be a numeric vector. ",
            "For speeds by street and hour use 'eu' as data.frame or ",
            "one call per euro standard."
          )
        }
        dff <- do.call("cbind", lapply(lf, function(fi) fi(speed)))
        dff <- EmissionFactors(dff)
        names(dff) <- paste0(eu, 1:length(eu))
        dff$speed <- speed
        return(dff)
      }
      names(lf) <- paste0(eu, 1:length(eu))
      return(EmissionFactorsList(lf))
    }
  } else if (is.data.frame(eu)) {
    if (missing(speed)) {
      stop("Add 'speed' please")
    }
    if (is.data.frame(speed)) {
      stop("When 'eu' is a data.frame, 'speed' must be a numeric vector")
    }
    dff <- do.call(
      "rbind",
      lapply(1:nrow(eu), function(j) {
        do.call(
          "cbind",
          lapply(1:ncol(eu), function(i) {
            fun_eu(eu[j, i][[1]])(speed)
          })
        )
      })
    )
    dff <- EmissionFactors(dff)
    dff$speed <- speed
    dff$row_eu <- rep(1:nrow(eu), each = length(speed))
    return(dff)
  }
}
