# Internal helpers shared by ef_ldv_speed() and ef_hdv_speed() ---------------
#
# The EMEP/EEA tables store each emission factor as an expression string Y in
# the speed V (km/h) with coefficients a..f (and x for SO2/Pb). These helpers
# build the R closure for one row so every branch of the speed functions
# applies the same rules: speed clamped to [MINV, MAXV], the factor k and the
# fuel correction k2, and negative values set to zero (the compiled engine in
# src/e_speed.c follows the same rules).

# Speed in km/h as plain numbers (vector, matrix or data.frame)
.speed_kmh <- function(speed) {
  conv <- function(z) {
    if (inherits(z, "units")) {
      as.numeric(units::set_units(z, "km/h", mode = "standard"))
    } else {
      as.numeric(z)
    }
  }
  if (is.data.frame(speed)) {
    out <- as.data.frame(lapply(speed, conv))
    names(out) <- names(speed)
    return(out)
  }
  if (is.matrix(speed)) {
    return(as.data.frame(lapply(as.data.frame(speed), conv)))
  }
  if (!inherits(speed, "units")) {
    stop("speed needs class 'Speed' or 'units' in 'km/h'. See ?Speed and ?units::set_units")
  }
  conv(speed)
}

# Row of an EF table selected by exact match of the named key values
.ef_row <- function(tab, keys, what) {
  sel <- rep(TRUE, nrow(tab))
  for (nm in names(keys)) sel <- sel & tab[[nm]] == keys[[nm]]
  sel[is.na(sel)] <- FALSE
  row <- tab[sel, , drop = FALSE]
  if (nrow(row) == 0) {
    stop("No emission factor for ", what,
         ". Check the combination of arguments.", call. = FALSE)
  }
  row[1, , drop = FALSE]
}

# Closure EF(V) for one table row
.ef_speed_fun <- function(row, k = 1, k2 = 1, x = NULL, what = "") {
  Y <- as.character(row$Y)
  if (is.na(Y) || !nzchar(trimws(Y))) {
    stop("Emission factor not available for ", what, call. = FALSE)
  }
  ex <- parse(text = Y)[[1]]
  needs_x <- "x" %in% all.vars(ex)
  if (needs_x && is.null(x)) {
    stop("'x' (sulphur or lead content of the fuel, ppm) is needed for ",
         what, call. = FALSE)
  }
  cf <- function(nm) {
    z <- row[[nm]]
    if (is.null(z)) 0 else as.numeric(z)
  }
  env <- list(a = cf("a"), b = cf("b"), c = cf("c"), d = cf("d"),
              e = cf("e"), f = cf("f"), x = if (needs_x) as.numeric(x) else NA)
  minv <- as.numeric(row$MINV)
  maxv <- as.numeric(row$MAXV)
  kk <- as.numeric(k) * as.numeric(k2)
  function(V) {
    V <- as.numeric(V)
    Vc <- pmin(pmax(V, minv), maxv)
    out <- eval(ex, c(env, list(V = Vc)), baseenv())
    out <- rep_len(as.numeric(out), length(V)) * kk
    out[!is.na(out) & out < 0] <- 0
    out
  }
}

# Fuel correction by euro standard, order "PRE", "I", "II", "III", "IV", "V",
# "VI" and any later standard
.fcorr_k2 <- function(eu, fcorr) {
  idx <- match(as.character(eu), c("PRE", "I", "II", "III", "IV", "V", "VI"))
  idx[is.na(idx)] <- 8L
  rep_len(fcorr, 8)[idx]
}
