# HBEFA-based instantaneous emission models of Eclipse SUMO, ported to R.
#
# The formulas below and the coefficient table sysdata$hbefa are ported from
# Eclipse SUMO, src/utils/emissions (HelpersHBEFA.h/.cpp [HBEFA v2.1],
# HelpersHBEFA3.h/.cpp [HBEFA v3.1], HelpersHBEFA4.h/.cpp [HBEFA v4.2] and
# PollutantsInterface.cpp [coasting deceleration]), commit
# 25bdc3b9f7b1ee05e9dda40f3b8f6266736f005b (2026-10-07),
# https://github.com/eclipse-sumo/sumo/tree/25bdc3b9f7b1ee05e9dda40f3b8f6266736f005b/src/utils/emissions
#
# Eclipse SUMO, Simulation of Urban MObility; see https://eclipse.dev/sumo
# Copyright (C) 2001-2026 German Aerospace Center (DLR) and others.
# This program and the accompanying materials are made available under the
# terms of the Eclipse Public License 2.0 which is available at
# https://www.eclipse.org/legal/epl-2.0/
# This Source Code may also be made available under the following Secondary
# Licenses when the conditions for such availability set forth in the Eclipse
# Public License 2.0 are satisfied: GNU General Public License, version 2
# or later which is available at
# https://www.gnu.org/licenses/old-licenses/gpl-2.0-standalone.html
# SPDX-License-Identifier: EPL-2.0 OR GPL-2.0-or-later
#
# The coefficients are continuous reformulations (polynomial fits) by DLR of the
# Handbook Emission Factors for Road Transport (HBEFA, https://www.hbefa.net/,
# coordinated by INFRAS) versions 2.1, 3.1 and 4.2. Unlike the rest of vein
# (MIT), this file and sysdata$hbefa are distributed under
# EPL-2.0 OR GPL-2.0-or-later; see inst/COPYRIGHTS.
#
# R port: validated bit-for-bit against SUMO's C++ code (8.7 million points) and
# against SUMO's emissionsDrivingCycle NEDC regression results for all classes.

# constants exactly as in SUMO
.HBEFA_GRAVITY <- 9.80665            # utils/geom/GeomHelper.h
.HBEFA_GRAVITY_CONST <- 9.81         # foreign/PHEMlight/V5/cpp/Constants.cpp
.HBEFA_SPEED_DCEL_MIN <- 10 / 3.6    # foreign/PHEMlight/V5/cpp/Constants.cpp
.HBEFA_ZERO_SPEED_ACCURACY <- 0.5    # PollutantsInterface.cpp
.HBEFA_POL <- c("CO2", "CO", "HC", "FC", "NOx", "PM", "EC")

.hbefa_tab <- function() {
  tab <- sysdata$hbefa
  if (is.null(tab)) stop("sysdata$hbefa is not available in this build of vein")
  tab
}

# numeric value of x, converting 'units' objects to unit u
.hbefa_num <- function(x, u) {
  if (inherits(x, "units")) {
    as.numeric(units::set_units(x, u, mode = "standard"))
  } else {
    as.numeric(x)
  }
}

# vein pollutant name (also accepts SUMO names)
.hbefa_pol <- function(p) {
  if (length(p) != 1) stop("'pollutant' must be of length 1")
  map <- c(co2 = "CO2", co = "CO", hc = "HC", fc = "FC", fuel = "FC",
    nox = "NOx", pm = "PM", pmx = "PM", ec = "EC", electricity = "EC")
  q <- map[tolower(p)]
  if (is.na(q)) {
    stop("Unknown pollutant '", p, "'. Use one of: ",
      paste(.HBEFA_POL, collapse = ", "))
  }
  unname(q)
}

# resolve a SUMO emission class as SUMO does (PollutantsInterface::getClassByName)
.hbefa_resolve <- function(class, model) {
  class <- as.character(class)
  if (grepl("/", class, fixed = TRUE)) {
    model <- sub("/.*$", "", class)
    name <- sub("^[^/]*/", "", class)
  } else {
    name <- class
  }
  if (identical(name, "zero") || identical(model, "Zero")) {
    return(list(model = "Zero", name = "default", row = NA_integer_))
  }
  if (!model %in% c("HBEFA2", "HBEFA3", "HBEFA4")) {
    stop("Unknown model '", model, "'. Use 'HBEFA2', 'HBEFA3' or 'HBEFA4'")
  }
  tab <- .hbefa_tab()
  im <- which(tab$model == model)
  nm <- tab$name[im]
  i <- match(name, nm)
  if (is.na(i) && tolower(name) %in% c("default", "unknown")) {
    def <- c(HBEFA2 = "P_7_7", HBEFA3 = "PC_G_EU4", HBEFA4 = "PC_petrol_Euro-4")
    i <- match(def[[model]], nm)
  }
  if (is.na(i) && model != "HBEFA4") {
    # HBEFA2/3 register lower case aliases of every class
    i <- match(tolower(name), tolower(nm))
  }
  if (is.na(i)) {
    stop("Unknown emission class '", class, "' for model ", model,
      ". See hbefa_classes(\"", model, "\")")
  }
  list(model = model, name = nm[i], row = im[i])
}

# fuel label of SUMO's getFuel() (HelpersHBEFA3.cpp, HelpersHBEFA4.cpp)
.hbefa_sumo_fuel <- function(model, name) {
  if (model == "HBEFA3") {
    return(ifelse(grepl("_D_", name, fixed = TRUE), "Diesel", "Gasoline"))
  }
  if (model == "HBEFA4") {
    f <- ifelse(grepl("_diesel_", name, fixed = TRUE), "Diesel", "Gasoline")
    f <- ifelse(grepl("_BEV", name, fixed = TRUE), "Electricity", f)
    f <- ifelse(grepl("_PHEV", name, fixed = TRUE), paste0("Hybrid", f), f)
    return(f)
  }
  rep("Gasoline", length(name))
}

# PollutantsInterface::Helper::getCoastingDecel
.hbefa_coasting_decel <- function(v, slope, mass, frontal_area) {
  dec <- function(v) {
    incl <- frontal_area / mass * -9.05337017 + -0.00017774
    grad <- ifelse(slope == 0, 0, .HBEFA_GRAVITY_CONST * sin(slope * pi / 180))
    x <- incl * v + 0.00001066 * mass + -0.38347107 - 20.0 * incl - grad
    ifelse(0 < x, 0, x)
  }
  res <- dec(v)
  low <- which(v < .HBEFA_SPEED_DCEL_MIN)
  if (length(low)) {
    res[low] <- (v / .HBEFA_SPEED_DCEL_MIN *
      dec(rep_len(.HBEFA_SPEED_DCEL_MIN, length(v))))[low]
  }
  res
}

# emission rate as SUMO returns it: mg/s (ml/s volumetric fuel, Wh/s electricity)
.hbefa_rate <- function(res, pol, v, a, slope, mass, frontal_area,
                        coasting, volumetric_fuel) {
  n <- length(v)
  if (res$model == "Zero") return(rep(0, n))
  if (res$model != "HBEFA4" && pol == "EC") return(rep(0, n))
  tab <- .hbefa_tab()
  f <- as.numeric(unlist(tab[res$row, paste0(pol, "_c", 0:6)], use.names = FALSE))
  sfuel <- .hbefa_sumo_fuel(res$model, res$name)
  if (res$model == "HBEFA2") {
    # HelpersHBEFA.h: V in km/h, alpha = acceleration as gradient angle [deg]
    kmh <- v * 3.6
    scale <- if (pol == "FC" && volumetric_fuel) 3.6 * 790. else 3.6
    if (grepl("_A0_", res$name, fixed = TRUE)) {
      x <- (f[1] + f[4] * kmh + f[5] * kmh * kmh + f[6] * kmh * kmh * kmh) / scale
    } else {
      alpha <- asin(a / .HBEFA_GRAVITY) * 180. / pi
      x <- (f[1] + f[2] * alpha * kmh + f[3] * alpha * alpha * kmh + f[4] * kmh +
        f[5] * kmh * kmh + f[6] * kmh * kmh * kmh) / scale
    }
    out <- ifelse(x > 0, x, 0)
  } else if (res$model == "HBEFA3") {
    # HelpersHBEFA3.h
    scale <- 3.6
    if (pol == "FC" && volumetric_fuel) {
      scale <- scale * (if (sfuel == "Diesel") 836. else 742.)
    }
    a2 <- a + .HBEFA_GRAVITY * sin(slope * pi / 180.)
    x <- (f[1] + f[2] * a2 * v + f[3] * a2 * a2 * v + f[4] * v + f[5] * v * v +
      f[6] * v * v * v) / scale
    out <- ifelse(x > 0, x, 0)
  } else {
    # HelpersHBEFA4.h
    scale <- 1.
    if (pol == "FC" && volumetric_fuel) {
      if (sfuel == "Diesel") scale <- 836. else if (sfuel == "Gasoline") scale <- 742.
    }
    a2 <- a + .HBEFA_GRAVITY * sin(slope * pi / 180.)
    x <- (f[1] + f[2] * v + f[3] * a2 + f[4] * v * v + f[5] * v * v * v +
      f[6] * a2 * v + f[7] * a2 * v * v) / scale
    out <- if (pol != "EC") ifelse(0.0 > x, 0.0, x) else x
  }
  if (coasting && !(res$model == "HBEFA4" && sfuel == "Electricity")) {
    coast <- v > .HBEFA_ZERO_SPEED_ACCURACY &
      a < .hbefa_coasting_decel(v, slope, mass, frontal_area)
    out[which(coast)] <- 0
  }
  out
}

# units of the outputs
.hbefa_mass_unit <- function(pol, volumetric_fuel) {
  if (pol == "EC") "W*h" else if (pol == "FC" && volumetric_fuel) "L" else "g"
}
# factor from SUMO units (mg, ml, Wh) to the output mass unit (g, L, Wh)
.hbefa_conv <- function(pol) if (pol == "EC") 1 else 1 / 1000

#' HBEFA emission classes available in the SUMO-based models
#'
#' @description \code{\link{hbefa_classes}} lists the emission classes of the
#' HBEFA-based emission models of Eclipse SUMO, which are available in
#' \code{\link{emis_hbefa}}, \code{\link{ef_hbefa}} and
#' \code{\link{ef_hbefa_cycle}}, with attributes parsed from the class names
#' (and, for HBEFA4, from the HBEFA subsegment names and ids) to help mapping
#' vein vehicle categories to HBEFA classes.
#'
#' @param model Character; one or more of "HBEFA4" (default, HBEFA v4.2,
#' 833 classes), "HBEFA3" (HBEFA v3.1, 45 classes) and "HBEFA2" (HBEFA v2.1
#' clusters, 84 classes).
#' @return data.frame with columns \code{class} (full SUMO name, e.g.
#' "HBEFA4/PC_petrol_Euro-4", accepted by the other functions), \code{model},
#' \code{name}, \code{heavy}, \code{category} (HBEFA4: "PC", "LCV", "Coach",
#' "UBus", "MC", "HGV"; HBEFA3: "PC", "LDV", "Bus", "Coach", "HDV"; HBEFA2:
#' "PC/LDV", "HDV"), \code{fuel}, \code{euro} (integer, roman numerals
#' converted; NA when not applicable), \code{euro_label}, \code{technology},
#' \code{size}, \code{hbefa_subsegment}, \code{hbefa_id} and \code{sumo_fuel}
#' (the fuel label SUMO derives from the name; it decides the coasting
#' exception of battery electric vehicles and the volumetric fuel density).
#' @note The attributes \code{fuel}, \code{euro}, \code{technology} and
#' \code{size} are parsed heuristically; the authoritative identifiers are
#' \code{class} and, for HBEFA4, \code{hbefa_id}. HBEFA2 classes are clusters
#' of HBEFA 2.1 classes ("P_A0_*" and "HDV_A0_*" ignore acceleration).
#' @references SUMO documentation, HBEFA4-based model:
#' \url{https://sumo.dlr.de/docs/Models/Emissions/HBEFA4-based.html};
#' HBEFA3-based: \url{https://sumo.dlr.de/docs/Models/Emissions/HBEFA3-based.html};
#' HBEFA2-based: \url{https://sumo.dlr.de/docs/Models/Emissions/HBEFA-based.html}
#'
#' Krajzewicz, D., Behrisch, M., Wagner, P., Luz, R., Krumnow, M. (2015).
#' Second Generation of Pollutant Emission Models for SUMO. In: Behrisch, M.,
#' Weber, M. (eds) Modeling Mobility with Open Data. Lecture Notes in Mobility.
#' Springer, Cham. \doi{10.1007/978-3-319-15024-6_12}
#'
#' INFRAS (2022). Handbook Emission Factors for Road Transport (HBEFA) 4.2.
#' \url{https://www.hbefa.net/}
#' @seealso \code{\link{emis_hbefa}} \code{\link{ef_hbefa}}
#' \code{\link{ef_hbefa_cycle}}
#' @export
#' @examples
#' cl <- hbefa_classes()
#' table(cl$category)
#' # Euro 4 petrol passenger cars
#' cl[cl$category == "PC" & cl$fuel %in% "petrol" & cl$euro %in% 4, 1:8]
#' head(hbefa_classes("HBEFA3"))
hbefa_classes <- function(model = "HBEFA4") {
  bad <- setdiff(model, c("HBEFA2", "HBEFA3", "HBEFA4"))
  if (length(bad)) stop("Unknown model: ", paste(bad, collapse = ", "))
  tab <- .hbefa_tab()
  tab <- tab[tab$model %in% model, ]
  out <- data.frame(class = paste0(tab$model, "/", tab$name),
    tab[, c("model", "name", "heavy", "category", "fuel", "euro",
      "euro_label", "technology", "size",
      "hbefa_subsegment", "hbefa_id")],
    stringsAsFactors = FALSE)
  out$sumo_fuel <- unlist(lapply(seq_len(nrow(tab)), function(i) {
    .hbefa_sumo_fuel(tab$model[i], tab$name[i])
  }))
  rownames(out) <- NULL
  out
}

#' Instantaneous emissions with the HBEFA-based models of SUMO
#'
#' @description \code{\link{emis_hbefa}} returns instantaneous emission rates
#' (g/s) as a function of speed, acceleration and road slope, using the
#' HBEFA-based emission models of the traffic simulator Eclipse SUMO
#' (HBEFA v4.2, v3.1 and v2.1 reformulated as continuous functions by DLR).
#' The implementation reproduces SUMO exactly, including its rule that the
#' engine does not emit while coasting.
#'
#' HBEFA4: E = c0 + c1 v + c2 a2 + c3 v^2 + c4 v^3 + c5 a2 v + c6 a2 v^2 (mg/s),
#' with a2 = a + g sin(slope).
#'
#' HBEFA3: E = (c0 + c1 a2 v + c2 a2^2 v + c3 v + c4 v^2 + c5 v^3) / 3.6 (mg/s).
#'
#' HBEFA2: E = (c0 + c1 alpha V + c2 alpha^2 V + c3 V + c4 V^2 + c5 V^3) / 3.6
#' (mg/s), with V = 3.6 v in km/h and alpha = asin(a / g) in degrees; the slope
#' is not used except in the coasting rule. Classes "*_A0_*" ignore alpha.
#'
#' Negative values are set to 0 (except electricity). If v > 0.5 m/s and
#' a is lower than the coasting deceleration, all emissions are 0 (not applied
#' to HBEFA4 battery electric classes "*_BEV*"). The coasting deceleration is
#' min(0, i v + 1.066e-5 m - 0.38347107 - 20 i - 9.81 sin(slope)), with
#' i = -9.05337017 A / m - 0.00017774, m the mass and A the front surface,
#' linearly scaled to 0 for v < 10/3.6 m/s.
#'
#' @param class Character; SUMO emission class, e.g. "HBEFA4/PC_petrol_Euro-4",
#' "HBEFA3/PC_G_EU4", "HBEFA2/P_7_7", or the name without the prefix together
#' with \code{model}. "zero" gives no emissions; "default" is the default
#' class of the model. See \code{\link{hbefa_classes}}. Recycled.
#' @param pollutant Character, one of "CO2", "CO", "HC", "FC" (fuel mass),
#' "NOx", "PM", "EC" (electricity, Wh). SUMO names "fuel", "PMx" and
#' "electricity" are also accepted. In HBEFA4, "PM" is PM10 exhaust plus
#' non-exhaust and "EC" is the HBEFA FC_MJ (energy consumption) of battery
#' electric, plug-in hybrid electric mode and electric buses (0 for the other
#' classes, and always 0 in HBEFA2 and HBEFA3).
#' @param v Numeric speed in m/s, or 'units' (e.g. km/h), converted to m/s.
#' @param a Numeric acceleration in m/s^2, or 'units', converted to m/s^2.
#' @param slope Numeric road slope in degrees, or 'units' angle.
#' @param model Character; model used when \code{class} has no prefix:
#' "HBEFA4" (default), "HBEFA3" or "HBEFA2".
#' @param mass Numeric vehicle mass in kg (or 'units'), only used for the
#' coasting rule. SUMO default without vehicle type: 1500 kg (SUMO
#' simulations use the vehicle type mass, e.g. 4500 kg truck, 12000 kg bus,
#' 25000 kg coach, 5000 kg delivery).
#' @param frontal_area Numeric front surface in m^2 (or 'units'), only used for
#' the coasting rule. SUMO default 1.8 * 1.5 * pi / 4 = 2.1206 m^2.
#' @param coasting Logical; apply the coasting rule of SUMO (default TRUE).
#' @param volumetric_fuel Logical; if TRUE, "FC" is returned in L/s using the
#' densities of SUMO's option --emissions.volumetric-fuel (790 g/L in HBEFA2;
#' 836 g/L for classes SUMO labels as diesel and 742 g/L otherwise; in HBEFA4
#' SUMO only labels names containing "_diesel_" as diesel and does not convert
#' hybrids, which stay in g/s). Default FALSE: fuel mass in g/s.
#' @return 'units' vector: g/s ("FC": g/s or L/s; "EC": W*h/s).
#' @seealso \code{\link{hbefa_classes}} \code{\link{ef_hbefa}}
#' \code{\link{ef_hbefa_cycle}}
#' @references SUMO documentation:
#' \url{https://sumo.dlr.de/docs/Models/Emissions.html},
#' \url{https://sumo.dlr.de/docs/Models/Emissions/HBEFA4-based.html};
#' source code \url{https://github.com/eclipse-sumo/sumo/tree/main/src/utils/emissions}
#'
#' Krajzewicz, D., Behrisch, M., Wagner, P., Luz, R., Krumnow, M. (2015).
#' Second Generation of Pollutant Emission Models for SUMO. In: Behrisch, M.,
#' Weber, M. (eds) Modeling Mobility with Open Data. Lecture Notes in Mobility.
#' Springer, Cham. \doi{10.1007/978-3-319-15024-6_12}
#'
#' INFRAS (2022). Handbook Emission Factors for Road Transport (HBEFA) 4.2.
#' \url{https://www.hbefa.net/}
#' @note The formulas and coefficients are ported from Eclipse SUMO
#' (Copyright (C) 2001-2026 German Aerospace Center (DLR) and others,
#' EPL-2.0 OR GPL-2.0-or-later). See \code{system.file("COPYRIGHTS",
#' package = "vein")}. SUMO documents the fit error of every HBEFA4 class and
#' pollutant; be careful with errors above 40\%.
#' SUMO's switch-off of the engine during long stops is not modelled.
#' @export
#' @examples
#' # CO2 of a Euro 4 petrol car at 50 km/h, accelerating 0.5 m/s2
#' emis_hbefa("HBEFA4/PC_petrol_Euro-4", "CO2", v = 50 / 3.6, a = 0.5)
#' # units are converted
#' emis_hbefa("HBEFA4/PC_petrol_Euro-4", "NOx",
#'            v = units::set_units(50, "km/h"), a = 0)
#' # idle and coasting (zero)
#' emis_hbefa("HBEFA3/PC_G_EU4", "CO2", v = c(0, 15), a = c(0, -1))
#' # electricity of a battery electric car, negative when braking
#' emis_hbefa("HBEFA4/PC_BEV", "EC", v = 15, a = c(-1, 0, 1))
emis_hbefa <- function(class, pollutant, v, a, slope = 0, model = "HBEFA4",
                       mass = 1500, frontal_area = 1.8 * 1.5 * pi / 4,
                       coasting = TRUE, volumetric_fuel = FALSE) {
  pol <- .hbefa_pol(pollutant)
  v <- .hbefa_num(v, "m/s")
  a <- .hbefa_num(a, "m/s^2")
  slope <- .hbefa_num(slope, "degree")
  mass <- .hbefa_num(mass, "kg")
  frontal_area <- .hbefa_num(frontal_area, "m^2")
  n <- max(length(class), length(v), length(a), length(slope),
    length(mass), length(frontal_area))
  mu <- .hbefa_mass_unit(pol, volumetric_fuel)
  if (n == 0 || length(class) == 0 || length(v) == 0 || length(a) == 0) {
    return(units::as_units(numeric(0), paste0(mu, " s-1")))
  }
  class <- rep_len(as.character(class), n)
  v <- rep_len(v, n)
  a <- rep_len(a, n)
  slope <- rep_len(slope, n)
  mass <- rep_len(mass, n)
  frontal_area <- rep_len(frontal_area, n)
  out <- numeric(n)
  for (k in unique(class)) {
    i <- which(class == k)
    res <- .hbefa_resolve(k, model)
    out[i] <- .hbefa_rate(res, pol, v[i], a[i], slope[i], mass[i],
      frontal_area[i], coasting, volumetric_fuel)
  }
  units::as_units(out * .hbefa_conv(pol), paste0(mu, " s-1"))
}

#' Constant speed emission factors from the HBEFA-based models of SUMO
#'
#' @description \code{\link{ef_hbefa}} returns speed dependent emission factors
#' (g/km) from the instantaneous HBEFA-based models of SUMO
#' (\code{\link{emis_hbefa}}) evaluated at constant speed (a = 0):
#' EF(V) = E(v = V / 3.6, a = 0) / (V / 3600), with V in km/h.
#'
#' @param class Character; SUMO emission class(es), see
#' \code{\link{hbefa_classes}}. With more than one class, a list of functions
#' is returned, or a data.frame with one column per class when \code{speed} is
#' given.
#' @param pollutant Character; "CO2", "CO", "HC", "FC", "NOx", "PM" or "EC",
#' see \code{\link{emis_hbefa}}.
#' @param model Character; model used when \code{class} has no prefix.
#' @param speed Optional speed in km/h, numeric, 'units' or 'Speed' (vector or
#' data.frame, e.g. from \code{\link{netspeed}}), to return emission factors
#' instead of a function.
#' @param slope Numeric road slope in degrees.
#' @param mass,frontal_area Vehicle mass (kg) and front surface (m^2). They
#' only enter SUMO's coasting rule, which never applies at constant speed, so
#' they have no effect here; kept for consistency with \code{\link{emis_hbefa}}.
#' @param vmin Numeric; minimum speed in km/h (default 5). Speeds below
#' \code{vmin} are set to \code{vmin}.
#' @param volumetric_fuel Logical; "FC" in L/km instead of g/km, see
#' \code{\link{emis_hbefa}}.
#' @return A function of V (km/h) returning numeric g/km ("EC": Wh/km,
#' "FC" with \code{volumetric_fuel}: L/km), or 'EmissionFactors' when
#' \code{speed} is given.
#' @note These are \strong{cruise} (steady speed) emission factors: they do
#' not include accelerations, stop & go, idling, traffic situations, cold
#' starts or evaporation, so they are not equal to the HBEFA
#' traffic-situation or EMEP/EEA average-speed emission factors.
#' The idle term makes the factor diverge as V -> 0, therefore V is limited
#' to \code{vmin} (5 km/h by default). For driving patterns use
#' \code{\link{ef_hbefa_cycle}} or \code{\link{emis_hbefa}} with speed traces.
#' @seealso \code{\link{emis_hbefa}} \code{\link{ef_hbefa_cycle}}
#' \code{\link{hbefa_classes}} \code{\link{ef_ldv_speed}}
#' @references SUMO documentation:
#' \url{https://sumo.dlr.de/docs/Models/Emissions/HBEFA4-based.html}
#'
#' Krajzewicz, D., Behrisch, M., Wagner, P., Luz, R., Krumnow, M. (2015).
#' Second Generation of Pollutant Emission Models for SUMO. In: Behrisch, M.,
#' Weber, M. (eds) Modeling Mobility with Open Data. Lecture Notes in Mobility.
#' Springer, Cham. \doi{10.1007/978-3-319-15024-6_12}
#'
#' INFRAS (2022). Handbook Emission Factors for Road Transport (HBEFA) 4.2.
#' \url{https://www.hbefa.net/}
#' @export
#' @examples
#' ef <- ef_hbefa("HBEFA4/PC_petrol_Euro-4", "CO2")
#' ef(c(20, 50, 90, 120))
#' ef_hbefa("HBEFA4/PC_diesel_Euro-6ab", "NOx", speed = Speed(c(30, 60, 90)))
#' ef_hbefa(c("HBEFA4/PC_petrol_Euro-4", "HBEFA4/PC_petrol_Euro-6d"), "CO",
#'          speed = Speed(c(30, 60, 90)))
#' ef_hbefa("HBEFA4/PC_BEV", "EC")(50) # Wh/km
ef_hbefa <- function(class, pollutant, model = "HBEFA4", speed, slope = 0,
                     mass = 1500, frontal_area = 1.8 * 1.5 * pi / 4,
                     vmin = 5, volumetric_fuel = FALSE) {
  pol <- .hbefa_pol(pollutant)
  slope <- .hbefa_num(slope, "degree")
  mu <- .hbefa_mass_unit(pol, volumetric_fuel)
  fun1 <- function(cl) {
    res <- .hbefa_resolve(cl, model)
    function(V) {
      V <- .hbefa_num(V, "km/h")
      V <- ifelse(V < vmin, vmin, V)
      r <- .hbefa_rate(res, pol, v = V / 3.6, a = rep_len(0, length(V)),
        slope = rep_len(slope, length(V)), mass = mass,
        frontal_area = frontal_area, coasting = TRUE,
        volumetric_fuel = volumetric_fuel)
      r / (V / 3600) * .hbefa_conv(pol)
    }
  }
  spd <- function(s) {
    if (is.data.frame(s) || is.matrix(s)) {
      s <- as.data.frame(s)
      out <- as.data.frame(lapply(s, function(z) .hbefa_num(z, "km/h")))
      names(out) <- names(s)
      return(out)
    }
    .hbefa_num(s, "km/h")
  }
  if (length(class) == 1) {
    f1 <- fun1(class)
    if (missing(speed)) return(f1)
    speed <- spd(speed)
    if (is.data.frame(speed)) {
      return(EmissionFactors(as.data.frame(lapply(speed, f1)), mass = mu))
    }
    return(EmissionFactors(f1(speed), mass = mu))
  }
  lf <- lapply(class, fun1)
  names(lf) <- class
  if (missing(speed)) return(lf)
  speed <- spd(speed)
  if (is.data.frame(speed)) {
    stop("When length(class) > 1, 'speed' must be a vector")
  }
  dff <- as.data.frame(lapply(lf, function(fi) fi(speed)))
  names(dff) <- class
  EmissionFactors(dff, mass = mu)
}

#' Emission factors over a driving cycle from the HBEFA-based models of SUMO
#'
#' @description \code{\link{ef_hbefa_cycle}} integrates the instantaneous
#' emissions of \code{\link{emis_hbefa}} over a speed trace and divides by the
#' distance travelled, returning g/km. It follows SUMO's tool
#' emissionsDrivingCycle with --compute-a: the first sample only initialises;
#' for every following sample t, a_t = (v_t - v_(t-1)) / dt and the emission is
#' evaluated at (v_t, a_t) ("backward", default) or at (v_(t-1), a_t)
#' ("forward", --compute-a.forward). Mass = sum(E_t dt) and
#' distance = sum(v dt) over the same samples.
#'
#' @param class Character; SUMO emission class(es), see
#' \code{\link{hbefa_classes}}. One emission factor is returned per class.
#' @param pollutant Character; "CO2", "CO", "HC", "FC", "NOx", "PM" or "EC".
#' @param speed Speed trace, numeric in km/h or 'units'/'Speed'.
#' @param dt Numeric time step in seconds (or 'units'), default 1 (1 Hz).
#' @param model Character; model used when \code{class} has no prefix.
#' @param slope Numeric slope in degrees, length 1 or the length of
#' \code{speed}.
#' @param accel Character; "backward" (default) or "forward", see description.
#' @param full Logical; if TRUE returns a list with the emission factor, total
#' mass (g; Wh for "EC"), distance (km) and the per-step speed (m/s),
#' acceleration (m/s^2) and emission rate.
#' @param ... Further arguments for \code{\link{emis_hbefa}}: \code{mass},
#' \code{frontal_area}, \code{coasting}, \code{volumetric_fuel}.
#' @return 'EmissionFactors' (units g/km; "EC": Wh/km), one value per class,
#' or a list when \code{full = TRUE}.
#' @note Validated against the NEDC results of SUMO's emissionsDrivingCycle
#' for all classes of the three models. The speed trace should be at the
#' resolution used to derive the coefficients (1 Hz); coarser traces
#' underestimate accelerations.
#' @seealso \code{\link{emis_hbefa}} \code{\link{ef_hbefa}}
#' @references SUMO documentation:
#' \url{https://sumo.dlr.de/docs/Tools/Emissions.html}
#'
#' Krajzewicz, D., Behrisch, M., Wagner, P., Luz, R., Krumnow, M. (2015).
#' Second Generation of Pollutant Emission Models for SUMO. In: Behrisch, M.,
#' Weber, M. (eds) Modeling Mobility with Open Data. Lecture Notes in Mobility.
#' Springer, Cham. \doi{10.1007/978-3-319-15024-6_12}
#' @export
#' @examples
#' # synthetic urban trip: accelerate, cruise at 50 km/h, brake, idle
#' v <- c(seq(0, 50, by = 5), rep(50, 30), seq(50, 0, by = -10), rep(0, 10))
#' ef_hbefa_cycle("HBEFA4/PC_petrol_Euro-4", "CO2", speed = v)
#' ef_hbefa_cycle(c("HBEFA4/PC_petrol_Euro-4", "HBEFA4/PC_diesel_Euro-6d"),
#'                "NOx", speed = v)
ef_hbefa_cycle <- function(class, pollutant, speed, dt = 1, model = "HBEFA4",
                           slope = 0, accel = c("backward", "forward"),
                           full = FALSE, ...) {
  accel <- match.arg(accel)
  pol <- .hbefa_pol(pollutant)
  vol <- isTRUE(list(...)$volumetric_fuel)
  v <- .hbefa_num(speed, "km/h") / 3.6
  dt <- .hbefa_num(dt, "s")
  n <- length(v)
  if (n < 2) stop("'speed' needs at least two samples")
  if (anyNA(v)) stop("'speed' has NA values")
  slope <- rep_len(.hbefa_num(slope, "degree"), n)
  a <- (v[-1] - v[-n]) / dt
  if (accel == "backward") {
    vv <- v[-1]
    ss <- slope[-1]
  } else {
    vv <- v[-n]
    ss <- slope[-n]
  }
  dist <- sum(vv * dt) / 1000
  res <- lapply(class, function(cl) {
    r <- as.numeric(emis_hbefa(cl, pol, v = vv, a = a, slope = ss,
      model = model, ...))
    tot <- sum(r * dt)
    list(ef = tot / dist, total = tot, rate = r)
  })
  ef <- vapply(res, function(z) z$ef, numeric(1))
  names(ef) <- class
  mu <- .hbefa_mass_unit(pol, vol)
  ef <- EmissionFactors(unname(ef), mass = mu)
  if (!full) return(ef)
  list(ef = ef,
    total = units::as_units(vapply(res, function(z) z$total, numeric(1)), mu),
    distance = units::as_units(dist, "km"),
    v = units::as_units(vv, "m/s"),
    a = units::as_units(a, "m/s^2"),
    rate = units::as_units(do.call(cbind, lapply(res, function(z) z$rate)),
      paste0(mu, " s-1")),
    class = class)
}
