# Portable MOVES rate tables. All evaluation is in R; the database is used only
# by the external data-production workflow, never by these functions.

.moves_columns <- c("MOVESScenarioID", "MOVESRunID", "yearID", "monthID",
                    "dayID", "hourID", "linkID", "pollutantID", "processID",
                    "sourceTypeID", "regClassID", "SCC", "fuelTypeID",
                    "modelYearID", "roadTypeID", "avgSpeedBinID", "temperature",
                    "relHumidity", "ratePerDistance", "massUnits",
                    "distanceUnits", "energyUnits", "masterVersion",
                    "defaultDatabaseUsed")

.moves_table <- function(x, label) {
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    if (!file.exists(x)) stop(label, " file does not exist: ", x)
    if (grepl("\\.gz$", x, ignore.case = TRUE)) {
      con <- gzfile(x, "rt")
      on.exit(close(con))
      x <- utils::read.table(con, header = TRUE, stringsAsFactors = FALSE,
                             sep = if (grepl("\\.tsv\\.gz$", x)) "\t" else ",",
                             check.names = FALSE, comment.char = "")
    } else {
      x <- as.data.frame(data.table::fread(file = x))
    }
  }
  if (!is.data.frame(x) || !nrow(x)) stop(label, " must be a non-empty table")
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (anyDuplicated(tolower(names(x)))) stop(label, " has duplicate column names")
  i <- match(tolower(names(x)), tolower(.moves_columns))
  names(x)[!is.na(i)] <- .moves_columns[i[!is.na(i)]]
  x[] <- lapply(x, function(z) if (is.factor(z)) as.character(z) else z)
  x
}

.moves_required <- function(x, columns, label) {
  absent <- setdiff(columns, names(x))
  if (length(absent)) stop(label, " is missing: ", paste(absent, collapse = ", "))
}

.moves_numeric <- function(x, name, integer = FALSE) {
  if (!is.numeric(x) || inherits(x, "units") || any(!is.finite(x)) ||
      (integer && any(x != floor(x)))) {
    stop(name, " must contain finite ", if (integer) "integer " else "numeric ",
         "values without units")
  }
}

# MOVESRun records abbreviated units (e.g. g, mi), while user exports may use
# the full RunSpec labels. ton denotes the U.S. short ton in MOVES.
.moves_unit_scale <- function(x, type) {
  key <- tolower(gsub("[ .]", "", as.character(x)))
  scale <- if (type == "mass") {
    c(g = 1, gram = 1, grams = 1, kg = 1000, kilogram = 1000,
      kilograms = 1000, lb = 453.59237, pound = 453.59237, pounds = 453.59237,
      ton = 907184.74, uston = 907184.74)
  } else {
    c(mi = 1.609344, mile = 1.609344, miles = 1.609344,
      km = 1, kilometer = 1, kilometers = 1)
  }
  out <- unname(scale[key])
  if (anyNA(out)) stop("Unknown MOVES ", type, " units: ",
                       paste(unique(x[is.na(out)]), collapse = ", "))
  out
}

#' Portable MOVES running-exhaust factor data
#'
#' @description Imports exported MOVES RatePerDistance and MOVESRun tables or
#' loads the small reference dataset distributed with vein. All subsequent use
#' is local R computation, with no SQL server, Java or MOVES installation.
#' @param rates A data.frame or path to a CSV/TSV (optionally gzip compressed)
#' export of RatePerDistance. Omit both arguments to load the reference data.
#' @param runs A data.frame or file containing the corresponding MOVESRun rows,
#' including MOVESRunID, massUnits, distanceUnits, masterVersion and
#' defaultDatabaseUsed. Run IDs must uniquely identify runs within this input.
#' @return A \code{MOVESFactors} list with \code{rates}, \code{runs} and
#' \code{schema_version}. The rate table retains the original columns and adds
#' \code{gkm} (grams per vehicle-kilometre) and \code{temperature_C}. When linkID
#' is present, \code{locationID} preserves location while decoding the virtual
#' speed-bin links of Default/County scale runs. Save/load
#' the object with \code{saveRDS}/\code{readRDS} on any operating system.
#' @details This first interface supports pollutant mass for running exhaust
#' (processID 1) on road types 2--5. Export these processes separately from
#' starts, idle, evaporation and energy consumption. Zero rates are retained;
#' missing, negative or duplicate rates are rejected. No fleet or scenario
#' averaging is performed. MOVES temperature values are degrees Fahrenheit.
#'
#' The bundled reference data describe a limited U.S. scenario; inspect
#' \code{x$runs} and \code{x$rates} before selecting factors. They are not a
#' calibrated Ecuador fleet. Additional datasets can use the same format.
#' @references EPA MOVES6 technical guidance, section 4.2.2:
#' \url{https://www.epa.gov/moves/latest-version-motor-vehicle-emission-simulator-moves}
#' @seealso \code{\link{ef_moves}}
#' @export
#' @examples
#' x <- moves_factors()
#' x$runs[, c("masterVersion", "defaultDatabaseUsed")]
#' head(x$rates)
moves_factors <- function(rates, runs) {
  if (missing(rates) && missing(runs)) {
    directory <- system.file("extdata", "moves", package = "vein")
    rates <- file.path(directory, "rateperdistance.csv")
    runs <- file.path(directory, "movesrun.csv")
  } else if (missing(rates) || missing(runs)) {
    stop("Supply both rates and runs, or omit both for the reference dataset")
  }
  rates <- .moves_table(rates, "rates")
  runs <- .moves_table(runs, "runs")
  required <- c("MOVESRunID", "yearID", "pollutantID", "processID",
                "sourceTypeID", "fuelTypeID", "modelYearID", "roadTypeID",
                "avgSpeedBinID", "temperature", "relHumidity", "ratePerDistance")
  .moves_required(rates, required, "rates")
  .moves_required(runs, c("MOVESRunID", "massUnits", "distanceUnits",
                          "masterVersion", "defaultDatabaseUsed"), "runs")
  for (name in required) {
    .moves_numeric(rates[[name]], name,
                   integer = !name %in% c("temperature", "relHumidity", "ratePerDistance"))
  }
  .moves_numeric(runs$MOVESRunID, "runs$MOVESRunID", integer = TRUE)
  if (anyDuplicated(runs$MOVESRunID)) stop("runs has duplicate MOVESRunID values")
  run <- match(rates$MOVESRunID, runs$MOVESRunID)
  if (anyNA(run)) stop("Every rate must match a MOVESRunID in runs")
  if (any(rates$processID != 1L) || any(!rates$roadTypeID %in% 2:5)) {
    stop("Select running exhaust (processID 1) on roadTypeID 2--5")
  }
  if (any(rates$pollutantID %in% 91:94)) {
    stop("Energy consumption rates are not mass factors; export pollutants separately")
  }
  if (any(rates$sourceTypeID <= 0 | rates$fuelTypeID <= 0 |
          rates$modelYearID <= 0 | rates$yearID < rates$modelYearID)) {
    stop("Export disaggregated source, fuel and model year; modelYearID must not exceed yearID")
  }
  if (any(!rates$avgSpeedBinID %in% 1:16)) stop("avgSpeedBinID must be in 1:16")
  if (any(rates$relHumidity < 0 | rates$relHumidity > 100)) {
    stop("relHumidity must be in 0--100 percent")
  }
  if (any(rates$ratePerDistance < 0)) stop("ratePerDistance cannot be negative")
  for (name in c("masterVersion", "defaultDatabaseUsed")) {
    if (anyNA(runs[[name]]) || any(!nzchar(trimws(runs[[name]])))) {
      stop("runs must record ", name)
    }
  }
  if (any(c("gkm", "temperature_C", "locationID") %in% names(rates))) {
    stop("Supply original MOVES rates, without derived gkm/temperature_C/locationID columns")
  }
  keys <- setdiff(names(rates), "ratePerDistance")
  if (anyDuplicated(rates[keys])) stop("Duplicate rate keys; keep runs/scenarios distinct")
  rates$gkm <- rates$ratePerDistance *
    .moves_unit_scale(runs$massUnits[run], "mass") /
    .moves_unit_scale(runs$distanceUnits[run], "distance")
  rates$temperature_C <- (rates$temperature - 32) * 5 / 9
  if ("linkID" %in% names(rates)) {
    .moves_numeric(rates$linkID[!is.na(rates$linkID)], "linkID", integer = TRUE)
    rates$locationID <- ifelse(is.na(rates$linkID), NA_character_,
                               paste0("link:", rates$linkID))
    # ExecutionRunSpec.java: virtual link = (zoneID * 10 + roadTypeID) * 100 + bin.
    # Keep the zone separate from speed so two counties can never be conflated.
    if ("domain" %in% names(runs)) {
      virtual <- which(runs$domain[run] %in% c("DEFAULT", "SINGLE") & rates$linkID > 0)
      link <- rates$linkID[virtual]
      if (any(link %% 100 != rates$avgSpeedBinID[virtual] |
              floor(link / 100) %% 10 != rates$roadTypeID[virtual])) {
        stop("Virtual linkID does not agree with the road type and speed bin")
      }
      rates$locationID[virtual] <- paste0("zone:", floor(link / 1000))
    }
  }
  structure(list(rates = rates, runs = runs, schema_version = 1L),
            class = "MOVESFactors")
}

.moves_pollutant <- function(x) {
  if (length(x) != 1L || anyNA(x)) stop("pollutant must be one name or MOVES ID")
  if (is.numeric(x) && is.finite(x) && x > 0 && x == floor(x)) return(x)
  map <- c(CO = 2L, NOX = 3L, HC = 1L, THC = 1L, CO2 = 90L,
           `PM2.5` = 110L, PM10 = 100L)
  out <- unname(map[toupper(as.character(x))])
  if (is.na(out)) stop("Unknown pollutant; use CO, NOx, THC, CO2, PM2.5, PM10 or a numeric MOVES ID")
  out
}

#' Evaluate MOVES emission factors without a database
#'
#' @description Selects one running-exhaust scenario from portable MOVES data
#' and returns factors in g/km or a speed-dependent function for vein inventories.
#' @param pollutant MOVES numeric pollutant ID or CO, NOx, THC (also HC), CO2,
#' PM2.5 or PM10. PM names denote primary exhaust particulate matter.
#' @param model_year One vehicle model year, selected exactly (no age filling).
#' @param speed Numeric speed vector in km/h, or a vector with speed units.
#' Omit to return a function of speed yielding numeric g/km, suitable for
#' \code{\link{EmissionFactorsList}} and \code{\link{emis}}.
#' @param factors A \code{MOVESFactors} object from \code{\link{moves_factors}}.
#' Defaults to the bundled reference dataset.
#' @param source_type_id MOVES source type ID; default 21 (passenger car).
#' @param fuel_type_id MOVES fuel type ID; default 1 (gasoline).
#' @param road_type_id MOVES road type ID; default 5 (urban unrestricted).
#' @param temperature Optional scalar temperature in degrees Celsius, matched
#' to an existing temperature point. No meteorological interpolation is performed.
#' @param rel_humidity Optional scalar relative humidity in percent.
#' @param select Named list of additional scalar selections in the rate table,
#' e.g. \code{list(yearID = 2025, hourID = 1, MOVESRunID = 1)}. Every remaining
#' dimension must be constant; ambiguous scenarios are rejected.
#' @param method "bin" (default) uses MOVES speed bins with lower-inclusive
#' boundaries at 2.5, 7.5, ..., 72.5 mph. Positive speeds through 75 mph are
#' supported if their bin exists. "linear" interpolates between nominal bin
#' speeds (2.5, 5, 10, ..., 75 mph) and requires both adjacent native bins.
#' Interpolation is an approximation to exported MOVES output, not an EPA model
#' equation. No extrapolation or missing-bin filling is performed.
#' @return With speed, a units vector in g/km. Without speed, a function returning
#' numeric g/km, with \code{moves_metadata} recording the selected scenario,
#' source run, original rates, interpolation method and model version.
#' @details Zero speed is rejected because distance-based factors cannot describe
#' stationary idle. NA speeds propagate. Categorical attributes and model years
#' are selected exactly. Valid zero emission rates remain zero.
#' @seealso \code{\link{moves_factors}}, \code{\link{emis}}
#' @export
#' @examples
#' x <- moves_factors()
#' # Inspect available conditions and choose one exported hour.
#' unique(x$rates[, c("yearID", "hourID", "temperature_C", "relHumidity")])
#' ef_moves("NOx", model_year = 2020, speed = c(20, 40, 60),
#'          select = list(hourID = 1))
#' f <- ef_moves("NOx", model_year = 2020, select = list(hourID = 1))
#' f(c(20, 40, 60))
ef_moves <- function(pollutant, model_year, speed, factors = moves_factors(),
                      source_type_id = 21, fuel_type_id = 1, road_type_id = 5,
                      temperature, rel_humidity, select = list(),
                      method = c("bin", "linear")) {
  if (!inherits(factors, "MOVESFactors") || !identical(factors$schema_version, 1L)) {
    stop("factors must be created by moves_factors()")
  }
  method <- match.arg(method)
  filters <- list(pollutantID = .moves_pollutant(pollutant),
                  modelYearID = model_year, sourceTypeID = source_type_id,
                  fuelTypeID = fuel_type_id, roadTypeID = road_type_id, processID = 1L)
  if (!missing(temperature)) filters$temperature_C <- temperature
  if (!missing(rel_humidity)) filters$relHumidity <- rel_humidity
  if (!is.list(select) || (length(select) &&
      (is.null(names(select)) || anyNA(names(select)) || any(!nzchar(names(select))) ||
       anyDuplicated(names(select))))) stop("select must be a named list of scalar values")
  if (length(intersect(names(filters), names(select)))) {
    stop("select duplicates a named argument: ",
         paste(intersect(names(filters), names(select)), collapse = ", "))
  }
  filters <- c(filters, select)
  rates <- factors$rates
  forbidden <- c("avgSpeedBinID", "ratePerDistance", "gkm")
  bad <- union(setdiff(names(filters), names(rates)), intersect(names(filters), forbidden))
  if (length(bad)) stop("Invalid selection columns: ", paste(bad, collapse = ", "))
  for (name in names(filters)) {
    value <- filters[[name]]
    if (length(value) != 1L || anyNA(value) || inherits(value, "units") ||
        !is.atomic(value) || (is.numeric(value) && !is.finite(value))) {
      stop("Select one non-missing value without units for ", name)
    }
    keep <- if (name %in% c("temperature_C", "temperature", "relHumidity")) {
      if (!is.numeric(value)) stop(name, " must be numeric")
      abs(rates[[name]] - value) <= 1e-6
    } else {
      rates[[name]] == value
    }
    rates <- rates[!is.na(keep) & keep, , drop = FALSE]
  }
  if (!nrow(rates)) stop("No MOVES rates match the requested vehicle/scenario")
  dimensions <- setdiff(names(rates), c(forbidden,
                                       if ("locationID" %in% names(rates)) "linkID"))
  ambiguous <- dimensions[vapply(rates[dimensions], function(x) length(unique(x)) > 1L,
                                  logical(1))]
  if (length(ambiguous)) {
    stop("Select one scenario; ambiguous dimensions: ", paste(ambiguous, collapse = ", "))
  }
  if (anyDuplicated(rates$avgSpeedBinID)) stop("Duplicate rates per speed bin")
  rates <- rates[order(rates$avgSpeedBinID), , drop = FALSE]
  bins <- rates$avgSpeedBinID
  values <- rates$gkm
  nominal <- c(2.5, seq(5, 75, 5))
  # A deliberately small closure: it captures only this selected speed curve.
  fun <- .moves_curve(bins, values, method, nominal)
  attr(fun, "moves_metadata") <- list(
    scenario = rates[1L, dimensions, drop = FALSE],
    runs = factors$runs[factors$runs$MOVESRunID == rates$MOVESRunID[1L], , drop = FALSE],
    rates = rates, method = method, units = "g/km")
  if (missing(speed)) fun else EmissionFactors(fun(speed))
}

.moves_curve <- function(bins, values, method, nominal) {
  force(bins)
  force(values)
  force(method)
  force(nominal)
  function(V) {
    if (is.logical(V) && all(is.na(V))) V <- as.numeric(V)
    if (!is.numeric(V) || !is.null(dim(V))) stop("speed must be a numeric vector")
    if (inherits(V, "units")) V <- units::set_units(V, "km/h", mode = "standard")
    mph <- as.numeric(V) / 1.609344
    valid <- !is.na(mph)
    if (any(!is.finite(mph[valid])) || any(mph[valid] <= 0 | mph[valid] > 75 + 1e-10)) {
      stop("speed must be positive and no greater than 75 mph (120.7008 km/h)")
    }
    mph[valid] <- pmin(mph[valid], 75)
    # Snap round-off at native boundaries after a units conversion.
    for (b in unique(c(seq(2.5, 72.5, 5), nominal))) {
      mph[valid & abs(mph - b) < 1e-10] <- b
    }
    out <- rep(NA_real_, length(mph))
    if (method == "bin") {
      id <- findInterval(mph[valid], seq(2.5, 72.5, 5)) + 1L
      index <- match(id, bins)
      if (anyNA(index)) stop("Missing MOVES speed bins: ",
                             paste(unique(id[is.na(index)]), collapse = ", "))
      out[valid] <- values[index]
    } else {
      v <- mph[valid]
      if (any(v < nominal[1L])) stop("Linear interpolation starts at 2.5 mph")
      left <- findInterval(v, nominal)
      right <- pmin(left + 1L, length(nominal))
      exact <- abs(v - nominal[left]) < 1e-10
      right[exact] <- left[exact]
      il <- match(left, bins)
      ir <- match(right, bins)
      if (anyNA(il) || anyNA(ir)) stop("Missing adjacent MOVES speed bins for interpolation")
      weight <- rep(0, length(v))
      interpolate <- right != left
      weight[interpolate] <- (v[interpolate] - nominal[left[interpolate]]) /
        (nominal[right[interpolate]] - nominal[left[interpolate]])
      out[valid] <- values[il] * (1 - weight) + values[ir] * weight
    }
    out
  }
}
