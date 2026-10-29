# Portable MOVES rate tables. All evaluation is in R; the database is used only
# by the external data-production workflow, never by these functions.

.moves_cache <- new.env(parent = emptyenv())

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
  x[] <- lapply(x, function(z) {
    if (inherits(z, "integer64")) return(as.numeric(as.character(z)))
    if (is.factor(z)) as.character(z) else z
  })
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

#' Portable MOVES distance-based factor data
#'
#' @description Imports exported MOVES RatePerDistance and MOVESRun tables or
#' loads the small reference dataset distributed with vein. All subsequent use
#' is local R computation, with no SQL server, Java or MOVES installation.
#' The tables can come from MOVES itself or from the movesr package
#' (\url{https://github.com/atmoschem/movesr}), a standalone R port of the MOVES
#' rate engine whose \code{movesr_export_vein()} writes these files for any
#' county, year, month, weather and fuel.
#' @param rates A data.frame or path to a CSV/TSV (optionally gzip compressed)
#' export of RatePerDistance. Omit both arguments to load the reference data.
#' @param runs A data.frame or file containing the corresponding MOVESRun rows,
#' including MOVESRunID, massUnits, distanceUnits, masterVersion and
#' defaultDatabaseUsed. Run IDs must uniquely identify runs within this input.
#' @param vehicles Optional data.frame or file with sourceTypeID, sourceTypeName,
#' fuelTypeID and fuelTypeDesc. Omit for the bundled dataset or when names are
#' unavailable; \code{\link{moves_vehicles}} then lists numeric IDs.
#' @return A \code{MOVESFactors} list with \code{rates}, \code{runs},
#' \code{vehicles}, \code{basis} and \code{schema_version}. The rate table
#' retains the original columns and adds
#' \code{gkm} (grams per vehicle-kilometre) and \code{temperature_C}. When linkID
#' is present, \code{locationID} preserves location while decoding the virtual
#' speed-bin links of Default/County scale runs. Save/load
#' the object with \code{saveRDS}/\code{readRDS} on any operating system.
#' @details Supports pollutant mass per distance on road types 2--5 for running
#' exhaust (processID 1), brakewear (9), tirewear (10) and evaporative
#' permeation, vapor venting and fuel leaks while driving (11, 12, 13). Starts
#' and parked evaporative emissions are event/vehicle factors
#' (\code{\link{moves_start_factors}}, \code{\link{moves_evap_factors}});
#' energy consumption is not a mass factor. Zero rates are retained;
#' missing, negative or duplicate rates are rejected. No fleet or scenario
#' averaging is performed. MOVES temperature values are degrees Fahrenheit.
#'
#' The bundled reference data describe a U.S. scenario covering all 13 on-road
#' source types and their valid MOVES6 fuels. Inspect \code{\link{moves_vehicles}}
#' and \code{x$runs} before selecting factors. They are not a calibrated Ecuador
#' fleet. Additional datasets can use the same format.
#' @references EPA MOVES6 technical guidance, section 4.2.2:
#' \url{https://www.epa.gov/moves/latest-version-motor-vehicle-emission-simulator-moves}
#' @seealso \code{\link{ef_moves}}
#' @export
#' @examples
#' x <- moves_factors()
#' x$runs[, c("masterVersion", "defaultDatabaseUsed")]
#' head(x$rates)
moves_factors <- function(rates, runs, vehicles) {
  has_vehicles <- !missing(vehicles)
  cache <- missing(rates) && missing(runs) && missing(vehicles)
  if (cache && exists("distance", .moves_cache, inherits = FALSE)) {
    return(get("distance", .moves_cache, inherits = FALSE))
  }
  if (missing(rates) && missing(runs)) {
    directory <- system.file("extdata", "moves", package = "vein")
    rates <- file.path(directory, "rateperdistance.csv")
    runs <- file.path(directory, "movesrun.csv")
    if (!has_vehicles) vehicles <- file.path(directory, "vehicles.csv")
    has_vehicles <- TRUE
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
  if (any(!rates$processID %in% .moves_distance_processes) ||
      any(!rates$roadTypeID %in% 2:5)) {
    stop("Select distance-based processes (running exhaust 1, brakewear 9, ",
         "tirewear 10, evaporative 11--13) on roadTypeID 2--5")
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
  unavailable <- !has_vehicles ||
    (is.character(vehicles) && (length(vehicles) != 1L || !file.exists(vehicles))) ||
    (is.data.frame(vehicles) && !nrow(vehicles))
  if (unavailable) {
    if (has_vehicles) {
      warning("vehicles data unavailable; listing numeric source/fuel IDs only")
    }
    vehicles <- unique(rates[c("sourceTypeID", "fuelTypeID")])
    vehicles$sourceTypeName <- NA_character_
    vehicles$fuelTypeDesc <- NA_character_
    vehicles <- vehicles[c("sourceTypeID", "sourceTypeName", "fuelTypeID", "fuelTypeDesc")]
    vehicles <- vehicles[order(vehicles$sourceTypeID, vehicles$fuelTypeID), ]
    rownames(vehicles) <- NULL
  } else {
    vehicles <- .moves_table(vehicles, "vehicles")
    .moves_required(vehicles, c("sourceTypeID", "sourceTypeName",
                                "fuelTypeID", "fuelTypeDesc"), "vehicles")
    .moves_numeric(vehicles$sourceTypeID, "vehicles$sourceTypeID", integer = TRUE)
    .moves_numeric(vehicles$fuelTypeID, "vehicles$fuelTypeID", integer = TRUE)
    if (anyDuplicated(vehicles[c("sourceTypeID", "fuelTypeID")])) {
      stop("vehicles has duplicate source/fuel combinations")
    }
  }
  available <- paste(rates$sourceTypeID, rates$fuelTypeID)
  vehicles <- vehicles[paste(vehicles$sourceTypeID, vehicles$fuelTypeID) %in% available, ]
  rownames(vehicles) <- NULL
  if (!nrow(vehicles)) stop("No vehicle/fuel names match the rate data")
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
  result <- structure(list(rates = rates, runs = runs, vehicles = vehicles,
                           schema_version = 1L, basis = "distance"),
                      class = "MOVESFactors")
  if (cache) assign("distance", result, envir = .moves_cache)
  result
}

.moves_id <- function(value, vehicles, id, name, label) {
  if (is.numeric(value) && length(value) == 1L && !is.na(value)) return(value)
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop(label, " must be one numeric ID or one name")
  }
  if (is.null(vehicles) || !nrow(vehicles)) {
    stop("No vehicle names are available; supply a numeric ", label)
  }
  key <- tolower(trimws(value))
  found <- unique(vehicles[[id]][!is.na(vehicles[[name]]) &
                                    tolower(trimws(vehicles[[name]])) == key])
  if (!length(found)) stop("Unknown ", label, " '", value, "'; see moves_vehicles()")
  if (length(found) > 1L) stop("Ambiguous ", label, " '", value, "'; use its numeric ID")
  found
}

# Processes reported per distance on roads (RatePerDistance).
.moves_distance_processes <- c(1L, 9L, 10L, 11L, 12L, 13L)

.moves_process <- function(x) {
  if (length(x) != 1L || anyNA(x)) stop("process must be one name or MOVES ID")
  if (is.numeric(x)) {
    if (!x %in% .moves_distance_processes) {
      stop("process must be one of ", paste(.moves_distance_processes, collapse = ", "))
    }
    return(as.integer(x))
  }
  map <- c(running = 1L, brake = 9L, brakewear = 9L, tire = 10L, tirewear = 10L,
           permeation = 11L, venting = 12L, leaks = 13L)
  out <- unname(map[tolower(as.character(x))])
  if (is.na(out)) {
    stop("Unknown process; use running, brake, tire, permeation, venting, leaks ",
         "or a numeric MOVES ID")
  }
  out
}

.moves_pollutant <- function(x, process = 1L) {
  if (length(x) != 1L || anyNA(x)) stop("pollutant must be one name or MOVES ID")
  if (is.numeric(x) && is.finite(x) && x > 0 && x == floor(x)) return(x)
  map <- c(CO = 2L, NOX = 3L, HC = 1L, THC = 1L, CH4 = 5L, N2O = 6L, NH3 = 30L,
           SO2 = 31L, NMHC = 79L, NMOG = 80L, TOG = 86L, VOC = 87L, CO2 = 90L,
           CO2E = 98L, PM10 = 100L, `PM2.5` = 110L, OC = 111L, EC = 112L,
           SO4 = 115L)
  key <- toupper(as.character(x))
  out <- unname(map[key])
  # Brake and tire wear particulate matter have their own pollutant IDs.
  if (key %in% c("PM2.5", "PM10") && process %in% c(9L, 10L)) {
    out <- c(`PM2.5` = 116L, PM10 = 106L)[[key]] + (process == 10L)
  }
  if (is.na(out)) {
    stop("Unknown pollutant; use CO, NOx, THC, CH4, N2O, NH3, SO2, NMHC, NMOG, ",
         "TOG, VOC, CO2, CO2e, PM2.5, PM10, OC, EC, SO4 or a numeric MOVES ID")
  }
  out
}

#' List vehicle and fuel combinations in MOVES factors
#'
#' @description Returns the source type and fuel combinations present in a
#' portable MOVES dataset, with names when available. Use the IDs in
#' \code{\link{ef_moves}}.
#' @param factors A \code{MOVESFactors} object from \code{\link{moves_factors}}.
#' @return A data.frame with sourceTypeID, sourceTypeName, fuelTypeID and
#' fuelTypeDesc, ordered by source type and fuel.
#' @seealso \code{\link{ef_moves}}, \code{\link{moves_factors}}
#' @export
#' @examples
#' moves_vehicles()
moves_vehicles <- function(factors = moves_factors()) {
  if (!inherits(factors, "MOVESFactors")) {
    stop("factors must be created by moves_factors()")
  }
  vehicles <- factors$vehicles
  if (is.null(vehicles) || !nrow(vehicles)) {
    vehicles <- unique(factors$rates[c("sourceTypeID", "fuelTypeID")])
    vehicles$sourceTypeName <- NA_character_
    vehicles$fuelTypeDesc <- NA_character_
  }
  vehicles <- vehicles[c("sourceTypeID", "sourceTypeName",
                         "fuelTypeID", "fuelTypeDesc")]
  vehicles <- vehicles[order(vehicles$sourceTypeID, vehicles$fuelTypeID), ]
  rownames(vehicles) <- NULL
  vehicles
}

#' Evaluate MOVES emission factors without a database
#'
#' @description Selects one distance-based scenario (running exhaust by
#' default) from portable MOVES data and returns factors in g/km or a
#' speed-dependent function for vein inventories.
#' @param pollutant MOVES numeric pollutant ID or CO, NOx, THC (also HC), CH4,
#' N2O, NH3, SO2, NMHC, NMOG, TOG, VOC, CO2, CO2e, PM2.5, PM10, OC, EC or SO4.
#' PM2.5 and PM10 denote exhaust particulate matter, or brake/tire wear
#' particles (116/106, 117/107) when \code{process} is brake or tire. The
#' bundled reference data contain CO and NOx; other pollutants need extra data,
#' e.g. from movesr.
#' @param model_year One vehicle model year, selected exactly (no age filling).
#' @param speed Numeric speed vector in km/h, or a vector with speed units.
#' Omit to return a function of speed yielding numeric g/km, suitable for
#' \code{\link{EmissionFactorsList}} and \code{\link{emis}}.
#' @param factors A \code{MOVESFactors} object from \code{\link{moves_factors}}.
#' Defaults to the bundled reference dataset.
#' @param source_type_id MOVES source type ID or exact name, e.g. 21 or
#' "Passenger Car". Default 21. Use \code{\link{moves_vehicles}} to list all
#' combinations in the data.
#' @param fuel_type_id MOVES fuel type ID or exact name, e.g. 1 or "Gasoline".
#' Default 1. The bundled data include gasoline, diesel, E-85, CNG and
#' electricity where valid.
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
#' @param process MOVES process: 1 or "running" (default), 9 "brake",
#' 10 "tire", 11 "permeation", 12 "venting", 13 "leaks" (evaporative emissions
#' while driving).
#' @return With speed, a units vector in g/km. Without speed, a function returning
#' numeric g/km, with \code{moves_metadata} recording the selected scenario,
#' source run, original rates, interpolation method and model version.
#' @details Zero speed is rejected because distance-based factors cannot describe
#' stationary idle. NA speeds propagate. Categorical attributes and model years
#' are selected exactly. Valid zero emission rates remain zero.
#' @seealso \code{\link{moves_factors}}, \code{\link{moves_vehicles}},
#' \code{\link{emis}}
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
                      method = c("bin", "linear"), process = 1L) {
  if (!inherits(factors, "MOVESFactors") || !identical(factors$schema_version, 1L) ||
      !identical(factors$basis, "distance")) {
    stop("factors must be created by moves_factors()")
  }
  method <- match.arg(method)
  source_type_id <- .moves_id(source_type_id, factors$vehicles,
                              "sourceTypeID", "sourceTypeName", "source_type_id")
  fuel_type_id <- .moves_id(fuel_type_id, factors$vehicles,
                            "fuelTypeID", "fuelTypeDesc", "fuel_type_id")
  process <- .moves_process(process)
  filters <- list(pollutantID = .moves_pollutant(pollutant, process),
                  modelYearID = model_year, sourceTypeID = source_type_id,
                  fuelTypeID = fuel_type_id, roadTypeID = road_type_id,
                  processID = process)
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
