# Equivalence between the EMEP/EEA (COPERT) vehicle keys used by
# ef_ldv_speed() / ef_hdv_speed() and the HBEFA4 classes of the SUMO models
# (ef_hbefa.R), and an EEA-key interface to the HBEFA speed functions.

.hbefa_eu_ldv <- c(I = "Euro-1", II = "Euro-2", III = "Euro-3", IV = "Euro-4",
                   V = "Euro-5", VI = "Euro-6ab", VIc = "Euro-6d-temp",
                   VId = "Euro-6d", VII = "Euro-7")
.hbefa_eu_hdv <- c(PRE = "80ties", I = "Euro-I", II = "Euro-II", III = "Euro-III",
                   IV = "Euro-IV_SCR", V = "Euro-V_SCR", VI = "Euro-VI_A-C",
                   VIc = "Euro-VI_D-E", VId = "Euro-VI_D-E")
.hbefa_hdv_veh <- c("Trucks", "Ubus", "Coach")

# One EEA key -> c(class, note); note is "" when the equivalence is direct
.hbefa_eea1 <- function(v, t, cc, f, eu) {
  nt <- character()
  note <- function(...) nt <<- c(nt, paste0(...))
  no <- function(why = "") {
    stop("No HBEFA4 equivalent for v = '", v, "', t = '", t, "', cc = '", cc,
         "', f = '", f, "', eu = '", eu, "'", if (nzchar(why)) paste0(": ", why),
         ". See hbefa_eea()", call. = FALSE)
  }
  eu_l <- function(e, last = "Euro-7") {
    x <- .hbefa_eu_ldv[e]
    if (is.na(x)) no("unknown euro standard")
    if (e == "VII" && last != "Euro-7") {
      note("Euro 7 as ", last)
      x <- last
    }
    unname(x)
  }
  # euro digit classes of LPG/CNG/FFV/PHEV ("Euro-2" ... "Euro-6")
  eu_d <- function(e, from, to) {
    k <- match(e, c("PRE", "I", "II", "III", "IV", "V", "VI", "VIc", "VId", "VII"))
    if (is.na(k)) no("unknown euro standard")
    d <- min(max(k - 1L, 0L), 6L)
    dd <- min(max(d, from), to)
    if (dd != d || k > 7L) note("Euro ", e, " as Euro-", dd)
    paste0("Euro-", dd)
  }
  cls <- if (v == "PC") {
    if (!t %in% c("4S", "2S", "GDI", "GDI_GPF", "ECE_1501", "ECE_1502", "ECE_1503",
                  "ECE_1504", "IMPROVED_CONVENTIONAL", "OPEN_LOOP", "DPF",
                  "DPF_SWU", "LNT_DPF", "DPF_SCR")) no("unknown type")
    if (f %in% c("G", "FH")) {
      if (f == "FH") note("no full hybrid class in HBEFA4: conventional petrol")
      if (t %in% c("GDI", "GDI_GPF")) note("no GDI class in HBEFA4: petrol")
      if (t == "2S") {
        note("2-stroke cars: one HBEFA4 class for every euro")
        "PC_2S_EE"
      } else if (eu == "PRE") {
        switch(t,
          "4S" = "PC_petrol_ltECE",
          ECE_1501 = {
            note("EEA ECE 15/00-01 as HBEFA ECE-15'00")
            "PC_petrol_ECE-15_00"
          },
          ECE_1502 = "PC_petrol_ECE-15_01_02",
          ECE_1503 = "PC_petrol_ECE-15_03",
          ECE_1504 = "PC_petrol_ECE-15_04",
          IMPROVED_CONVENTIONAL = "PC_petrol_conv_other_concepts",
          OPEN_LOOP = "PC_petrol_Ucat",
          no())
      } else {
        paste0("PC_petrol_", eu_l(eu))
      }
    } else if (f == "D") {
      if (t %in% c("DPF", "LNT_DPF", "DPF_SCR")) {
        note("after-treatment variant ", t, " not distinguished in HBEFA4")
      }
      if (eu == "PRE") {
        "PC_diesel_conv"
      } else if (eu == "III+DPF") {
        "PC_diesel_Euro-3_(DPF)"
      } else if (t == "DPF_SWU" && eu == "V") {
        note("EEA DPF with S/W update as HBEFA 'other SU after software update'")
        "PC_diesel_Euro-5_other_SU_after_software_update"
      } else {
        paste0("PC_diesel_", eu_l(eu))
      }
    } else if (f == "LPG") {
      paste0("PC_LPG_petrol_", eu_d(eu, 2L, 6L), "_(LPG)")
    } else if (f == "CNG") {
      paste0("PC_CNG_petrol_", eu_d(eu, 2L, 6L), "_(CNG)")
    } else if (f %in% c("PHEV_G", "PHEV_D")) {
      fu <- if (f == "PHEV_G") "petrol" else "diesel"
      mode <- if (f == "PHEV_G") "(P)" else "(D)"
      e <- switch(eu, IV = "Euro-4", V = "Euro-5", VI = "Euro-6ab",
                  VIc = {
                    note("Euro 6 d-temp PHEV as Euro-6d")
                    "Euro-6d"
                  },
                  VId = "Euro-6d",
                  VII = {
                    note("Euro 7 PHEV as Euro-6d")
                    "Euro-6d"
                  },
                  no())
      paste0("PC_PHEV_", fu, "_", e, "_", mode)
    } else {
      no("unknown fuel")
    }
  } else if (v == "LCV") {
    sz <- switch(cc, "<3.5" = "N1-III", "N1-I" = "M+N1-I", "N1-II" = "N1-II",
                 "N1-III" = "N1-III", no("unknown LCV class"))
    if (!f %in% c("G", "D")) no("LCV fuels are G and D")
    fu <- if (f == "G") "petrol" else "diesel"
    if (eu == "PRE") {
      if (f == "G") {
        note("conventional petrol LCV as HBEFA 'Conv >1981'")
        paste0("LCV_petrol_", sz, "_Conv_gt1981")
      } else {
        paste0("LCV_diesel_", sz, "_convlt_1986")
      }
    } else if (eu == "III+DPF") {
      if (f != "D") no()
      paste0("LCV_diesel_", sz, "_Euro-3_(DPF)")
    } else {
      paste0("LCV_", fu, "_", sz, "_", eu_l(eu, last = "Euro-6d"))
    }
  } else if (v %in% c("Motorcycle", "Moped")) {
    if (f != "G") no("L-category fuel is G")
    e <- if (eu == "PRE") "preEuro" else {
      x <- c(I = "Euro-1", II = "Euro-2", III = "Euro-3", IV = "Euro-4",
             V = "Euro-5", VI = "Euro-6")[eu]
      if (is.na(x)) no("unknown euro standard")
      unname(x)
    }
    if (v == "Moped") {
      if (cc != "<=50") no("moped cc is '<=50'")
      note("2- and 4-stroke mopeds share one HBEFA4 class")
      if (e == "Euro-6") no()
      paste0("Moped_le50cc_", e)
    } else if (t == "4S") {
      sz <- switch(cc, "<=250" = "le250cc", "250_750" = "gt250cc",
                   ">=750" = "gt250cc", no("unknown motorcycle cc"))
      paste0("MC_4S_", sz, "_", e)
    } else if (t == "2S") {
      note("2-stroke motorcycles >50 cc as HBEFA <=250 cc")
      paste0("MC_2S_le250cc_", e)
    } else {
      no("unknown type")
    }
  } else if (v %in% .hbefa_hdv_veh) {
    if (!f %in% c("D", "CNG")) no("heavy vehicle fuels are D and CNG")
    if (f == "CNG") {
      e <- c(II = "Euro-II", III = "Euro-III", IV = "Euro-IV", V = "Euro-V",
             VI = "Euro-VI")[eu]
      if (is.na(e)) no()
      if (v == "Ubus") {
        sz <- switch(t, Midi = "Midi_lt15t", Std = "Std_gt15-18t",
                     Artic = "Artic_gt18t", no("unknown bus type"))
        paste0("UBus_", sz, "_CNG_", e)
      } else if (v == "Trucks" && t == "RT") {
        if (!eu %in% c("IV", "V", "VI")) no()
        sz <- switch(cc, "<=7.5" = "le7_5t", ">7.5 & <=12" = "gt7_5-12t", "gt12t")
        if (sz == "gt12t") note("CNG trucks >12 t share one HBEFA4 class")
        paste0("HGV_CNG_", sz, "_", e)
      } else {
        no()
      }
    } else {
      e <- .hbefa_eu_hdv[eu]
      if (is.na(e)) no("unknown euro standard")
      if (eu == "PRE") note("pre-Euro heavy vehicles as HBEFA 80ties")
      if (eu %in% c("IV", "V")) note("Euro ", eu, " as the SCR variant")
      if (v == "Trucks") {
        if (t == "RT") {
          sz <- switch(cc, "<=7.5" = "le7.5t", ">7.5 & <=12" = "gt7_5-12t",
                       ">12 & <=14" = "gt12-14t", ">14 & <=20" = "gt14-20t",
                       ">20 & <=26" = "gt20-26t", ">26 & <=28" = "gt26-28t",
                       ">28 & <=32" = "gt28-32t", ">32" = "gt32t",
                       no("unknown rigid truck weight"))
          paste0("RT_", sz, "_", e)
        } else if (t == "TT") {
          sz <- switch(cc, ">14 & <=20" = "gt14-20t", ">20 & <=28" = "gt20-28t",
                       ">28 & <=34" = "gt28-34t", ">34 & <=40" = "gt34-40t",
                       ">40 & <=50" = "gt40-50t", ">50 & <=60" = "gt50-60t",
                       no("unknown articulated truck weight"))
          paste0("TT_AT_", sz, "_", e)
        } else {
          no("truck types are RT and TT")
        }
      } else if (v == "Ubus") {
        sz <- switch(t, Midi = "Midi_le15t", Std = "Std_gt15-18t",
                     Artic = "Artic_gt18t", no("unknown bus type"))
        paste0("UBus_", sz, "_", e)
      } else {
        sz <- switch(t, Std = "Std_le18t", "3Axes" = "3-Axes_gt18t",
                     Midi = "Midi_le15t", no("unknown coach type"))
        paste0("Coach_", sz, "_", e)
      }
    }
  } else {
    no("unknown vehicle")
  }
  if (!cls %in% .hbefa_tab()$name[.hbefa_tab()$model == "HBEFA4"]) no()
  c(class = paste0("HBEFA4/", cls), note = paste(unique(nt), collapse = "; "))
}

#' Equivalence between EMEP/EEA vehicle categories and HBEFA4 classes
#'
#' @description \code{\link{hbefa_eea}} returns the HBEFA4 class (SUMO name)
#' equivalent to the EMEP/EEA (COPERT) keys used by \code{\link{ef_ldv_speed}}
#' and \code{\link{ef_hdv_speed}}. Without arguments it returns the full
#' equivalence table for every key of vein's EEA tables.
#'
#' @param v Character; category: "PC", "LCV", "Motorcycle", "Moped" (as in
#' \code{\link{ef_ldv_speed}}) or "Trucks", "Ubus", "Coach" (as in
#' \code{\link{ef_hdv_speed}}).
#' @param t Character; type, e.g. "4S", "ECE_1504", "DPF_SWU" (light vehicles)
#' or "RT", "TT", "Midi", "Std", "Artic", "3Axes" (heavy vehicles).
#' @param cc Character; size: engine size for PC and motorcycles (ignored for
#' PC, HBEFA4 has no size classes), "<3.5", "N1-I", "N1-II", "N1-III" for LCV,
#' gross weight (argument \code{g} of \code{\link{ef_hdv_speed}}) for trucks.
#' @param f Character; fuel: "G", "D", "LPG", "CNG", "FH", "PHEV_G", "PHEV_D".
#' Heavy vehicles: "D" (default) or "CNG" (urban buses and rigid trucks).
#' @param eu Character; euro standard: "PRE", "I", "II", "III", "III+DPF",
#' "IV", "V", "VI", "VIc" (Euro 6 d-temp), "VId" (Euro 6 d/e), "VII".
#' @param details Logical; if TRUE returns a data.frame with the keys, the
#' class and a note describing the approximations.
#' @return Character vector of HBEFA4 classes (e.g. "HBEFA4/PC_petrol_Euro-4"),
#' usable in \code{\link{emis_hbefa}}, \code{\link{ef_hbefa}} and
#' \code{\link{ef_hbefa_cycle}}; or a data.frame (columns VEH, TYPE, CC, FUEL,
#' EURO, class, exact, note) when \code{details = TRUE} or no key is given.
#' Arguments are recycled to the longest one.
#' @note Rules, in addition to the direct matches (same vehicle, fuel, size
#' class and euro standard):
#' \itemize{
#' \item PC: no engine size classes in HBEFA4; full hybrids and GDI as
#' conventional petrol; EEA after-treatment variants of Euro 6 diesel (DPF,
#' LNT+DPF, DPF+SCR) share one class; "VIc" is Euro-6d-temp and "VId"
#' Euro-6d. LPG/CNG classes exist for Euro 2 to 6; other standards use the
#' nearest one.
#' \item LCV: "<3.5" is N1-III; Euro 7 as Euro-6d.
#' \item Motorcycles: 250_750 and >=750 cc are HBEFA ">250cc"; 2-stroke >50
#' cc are "<=250cc"; mopeds of 2 and 4 strokes share one class.
#' \item Heavy vehicles: pre-Euro as "80ties"; Euro IV and V as the SCR
#' variants; Euro VI as "A-C" ("VIc"/"VId" as "D-E").
#' }
#' The column \code{exact} is FALSE when a rule other than a direct match was
#' used.
#' @seealso \code{\link{ef_hbefa_speed}} \code{\link{hbefa_classes}}
#' @export
#' @examples
#' hbefa_eea("PC", "4S", "1400_2000", "G", c("PRE", "I", "IV", "VI", "VId"))
#' hbefa_eea("LCV", "4S", "<3.5", "D", "V")
#' hbefa_eea("Trucks", "RT", ">14 & <=20", "D", "V", details = TRUE)
#' tab <- hbefa_eea()
#' head(tab)
#' table(tab$exact)
hbefa_eea <- function(v, t = "4S", cc, f, eu, details = FALSE) {
  if (missing(v) && missing(cc) && missing(eu)) {
    ldv <- unique(sysdata$ldv[, c("VEH", "TYPE", "CC", "FUEL", "EURO")])
    hdv <- unique(sysdata$hdv_criteria[, c("VEH", "TYPE", "GW", "EURO")])
    hdv <- data.frame(VEH = hdv$VEH, TYPE = hdv$TYPE, CC = hdv$GW,
                      FUEL = "D", EURO = hdv$EURO, stringsAsFactors = FALSE)
    keys <- rbind(ldv, hdv)
    return(.hbefa_eea_keys(keys, strict = FALSE))
  }
  if (missing(cc)) cc <- NA_character_
  if (missing(f)) {
    if (all(v %in% .hbefa_hdv_veh)) f <- "D" else stop("Add 'f' (fuel)")
  }
  if (missing(eu)) stop("Add 'eu'")
  n <- max(length(v), length(t), length(cc), length(f), length(eu))
  k <- data.frame(VEH = rep_len(as.character(v), n),
                  TYPE = rep_len(as.character(t), n),
                  CC = rep_len(as.character(cc), n),
                  FUEL = rep_len(as.character(f), n),
                  EURO = rep_len(as.character(eu), n),
                  stringsAsFactors = FALSE)
  k <- .hbefa_eea_keys(k, strict = TRUE)
  if (!details) return(k$class)
  k
}

# keys: data.frame VEH TYPE CC FUEL EURO; strict = FALSE gives NA classes
# (with the reason in note) instead of an error
.hbefa_eea_keys <- function(k, strict) {
  id <- do.call(paste, c(k, sep = "\r"))
  u <- !duplicated(id)
  r <- lapply(which(u), function(i) {
    one <- function() .hbefa_eea1(k$VEH[i], k$TYPE[i], k$CC[i], k$FUEL[i], k$EURO[i])
    if (strict) return(one())
    tryCatch(one(), error = function(e) {
      c(class = NA_character_, note = conditionMessage(e))
    })
  })
  r <- do.call(rbind, r)[match(id, id[u]), , drop = FALSE]
  k$class <- unname(r[, "class"])
  k$exact <- !is.na(k$class) & !nzchar(r[, "note"])
  k$note <- unname(r[, "note"])
  rownames(k) <- NULL
  k
}

#' Speed functions of the HBEFA4 models of SUMO with EMEP/EEA categories
#'
#' @description \code{\link{ef_hbefa_speed}} takes the same vehicle keys as
#' \code{\link{ef_ldv_speed}} and \code{\link{ef_hdv_speed}} and returns
#' HBEFA4 emission factors (g/km) as functions of speed, with the same shapes
#' as those functions, so it can replace them in a project. The class is
#' chosen with \code{\link{hbefa_eea}} and the factors are the constant speed
#' factors of \code{\link{ef_hbefa}}.
#'
#' @param v,t,cc,f,eu Vehicle keys as in \code{\link{ef_ldv_speed}} (heavy
#' vehicles: \code{v} "Trucks", "Ubus" or "Coach", \code{t} as in
#' \code{\link{ef_hdv_speed}} and the gross weight in \code{cc} or \code{g}).
#' \code{eu} can be a vector or a data.frame, as in \code{\link{ef_ldv_speed}}.
#' See \code{\link{hbefa_eea}}.
#' @param p Character; pollutant: "CO", "HC", "NOx", "PM", "FC", "CO2", "EC"
#' (HBEFA4), or "NO2" and "NO", computed as NOx times the NO2 (NO) share of
#' the EMEP/EEA tables of vein for the same key.
#' @param k Numeric; multiplication factor.
#' @param speed Optional speed with class "Speed" or "units" (vector or
#' data.frame), to return emission factors instead of functions.
#' @param show.equation Logical; print the HBEFA4 class used.
#' @param fcorr Numeric; fuel correction by euro standard, as in
#' \code{\link{ef_ldv_speed}}.
#' @param g Gross weight of heavy vehicles, alternative to \code{cc}, as in
#' \code{\link{ef_hdv_speed}}.
#' @param gr Numeric; road gradient as fraction (e.g. 0.02 for 2\%), as in
#' \code{\link{ef_hdv_speed}}; converted to the slope angle of the HBEFA
#' models.
#' @param l Load; HBEFA4 classes of SUMO have no load parameter, ignored.
#' @param vmin Numeric; minimum speed in km/h, see \code{\link{ef_hbefa}}.
#' @return A function of speed (km/h) for one euro standard, an
#' 'EmissionFactorsList' for several, or 'EmissionFactors' when \code{speed}
#' is given, as in \code{\link{ef_ldv_speed}}.
#' @note These are \strong{constant speed} (cruise) factors from the
#' instantaneous SUMO models, not average-speed factors like the EMEP/EEA
#' curves, which include the accelerations of real driving. Below about
#' 30 km/h the idle term dominates and the factors are higher than the
#' EMEP/EEA ones; CO and HC fits of SUMO have large errors. In HBEFA4 "PM" is
#' exhaust plus non-exhaust PM10, so do not add wear emissions on top. Hot
#' emissions only: no cold start or evaporative emissions.
#' @seealso \code{\link{hbefa_eea}} \code{\link{ef_hbefa}}
#' \code{\link{ef_ldv_speed}} \code{\link{ef_hdv_speed}}
#' @export
#' @examples
#' ef <- ef_hbefa_speed("PC", "4S", "1400_2000", "G", "IV", "CO2")
#' ef(c(20, 50, 90))
#' ef_hbefa_speed("PC", "4S", "1400_2000", "G", c("I", "IV", "VI"), "NOx",
#'                speed = Speed(c(30, 60, 90)))
#' ef_hbefa_speed(v = "Trucks", t = "RT", g = ">14 & <=20", eu = "V",
#'                p = "NO2", speed = Speed(50), show.equation = TRUE)
ef_hbefa_speed <- function(v, t = "4S", cc, f, eu, p, k = 1, speed,
                           show.equation = FALSE, fcorr = rep(1, 8), g,
                           gr = 0, l = 0.5, vmin = 5) {
  if (missing(cc)) {
    if (missing(g)) stop("Add 'cc' (or 'g' for heavy vehicles)")
    cc <- g
  }
  hdv <- v %in% .hbefa_hdv_veh
  if (missing(f)) {
    if (hdv) f <- "D" else stop("Add 'f' (fuel)")
  }
  slope <- atan(as.numeric(gr)) * 180 / pi
  hp <- c("CO", "HC", "NOx", "PM", "FC", "CO2", "EC")
  if (!p %in% c(hp, "NO2", "NO")) {
    stop("Pollutant '", p, "' is not available in HBEFA4. Use one of: ",
         paste(c(hp, "NO2", "NO"), collapse = ", "))
  }
  if (is.matrix(eu) | is.data.frame(eu)) {
    eu <- as.data.frame(eu)
    for (i in seq_len(ncol(eu))) eu[, i] <- as.character(eu[, i])
  } else {
    eu <- as.character(eu)
  }
  if (!missing(speed)) speed <- .speed_kmh(speed)

  # NO2 or NO share of NOx from vein's EEA tables, constant or f(V)
  share <- function(e) {
    if (!p %in% c("NO2", "NO")) return(function(V) 1)
    if (hdv) {
      if (f != "D") stop("NO2 and NO shares are only available for diesel heavy vehicles")
      fp <- ef_hdv_speed(v = v, t = t, g = cc, eu = e, p = p, gr = 0, l = 0.5)
      fx <- ef_hdv_speed(v = v, t = t, g = cc, eu = e, p = "NOx", gr = 0, l = 0.5)
      return(function(V) {
        x <- fx(V)
        ifelse(x > 0, fp(V) / x, 0)
      })
    }
    row <- .ef_row(sysdata$ldv,
                   list(VEH = v, TYPE = t, CC = cc, FUEL = f, EURO = e,
                        POLLUTANT = p),
                   paste(v, t, cc, f, e, p))
    x <- as.numeric(row$x)
    function(V) x
  }
  fun_eu <- function(e) {
    cl <- hbefa_eea(v, t, cc, f, e)
    f1 <- ef_hbefa(cl, if (p %in% c("NO2", "NO")) "NOx" else p,
                   slope = slope, vmin = vmin)
    sh <- share(e)
    kk <- as.numeric(k) * .fcorr_k2(e, fcorr)
    function(V) {
      V <- as.numeric(V)
      as.numeric(f1(V)) * sh(V) * kk
    }
  }
  mu <- if (p == "EC") "W*h" else "g"

  if (is.data.frame(eu)) {
    if (missing(speed)) stop("Add 'speed' please")
    if (is.data.frame(speed)) {
      stop("When 'eu' is a data.frame, 'speed' must be a numeric vector")
    }
    lf <- list()
    fe <- function(e) {
      if (is.null(lf[[e]])) lf[[e]] <<- fun_eu(e)
      lf[[e]]
    }
    dff <- do.call("rbind", lapply(seq_len(nrow(eu)), function(j) {
      do.call("cbind", lapply(seq_len(ncol(eu)), function(i) {
        fe(eu[j, i][[1]])(speed)
      }))
    }))
    dff <- EmissionFactors(dff, mass = mu)
    dff$speed <- speed
    dff$row_eu <- rep(seq_len(nrow(eu)), each = length(speed))
    return(dff)
  }
  if (show.equation) {
    d <- hbefa_eea(v, t, cc, f, eu, details = TRUE)
    for (i in seq_len(nrow(d))) {
      cat(paste0(d$EURO[i], " -> ", d$class[i],
                 if (nzchar(d$note[i])) paste0(" (", d$note[i], ")"), "\n"))
    }
  }
  if (length(eu) == 1) {
    f1 <- fun_eu(eu)
    if (missing(speed)) return(f1)
    if (is.data.frame(speed)) {
      return(EmissionFactors(as.data.frame(lapply(speed, f1)), mass = mu))
    }
    return(EmissionFactors(f1(speed), mass = mu))
  }
  lf <- lapply(eu, fun_eu)
  if (!missing(speed)) {
    if (is.data.frame(speed)) {
      stop("When length(eu) > 1, 'speed' must be a numeric vector. ",
           "For speeds by street and hour use 'eu' as data.frame or ",
           "one call per euro standard.")
    }
    dff <- do.call("cbind", lapply(lf, function(fi) fi(speed)))
    dff <- EmissionFactors(dff, mass = mu)
    names(dff) <- paste0(eu, seq_along(eu))
    dff$speed <- speed
    return(dff)
  }
  names(lf) <- paste0(eu, seq_along(eu))
  EmissionFactorsList(lf)
}
