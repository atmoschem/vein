# Start-exhaust and evaporative MOVES rates. These are event/vehicle factors,
# not distance factors; evaluation remains local R with no database access.

.moves_event_table <- function(rates, runs, vehicles, basis, process_ids,
                               rate_column, value_column) {
  rates <- .moves_table(rates, "rates")
  runs <- .moves_table(runs, "runs")
  required <- c("MOVESRunID", "yearID", "pollutantID", "processID",
                "sourceTypeID", "fuelTypeID", "modelYearID",
                "temperature", "relHumidity", rate_column)
  .moves_required(rates, required, "rates")
  .moves_required(runs, c("MOVESRunID", "massUnits", "masterVersion",
                          "defaultDatabaseUsed"), "runs")
  for (name in required) {
    .moves_numeric(rates[[name]], name,
                   integer = !name %in% c("temperature", "relHumidity", rate_column))
  }
  .moves_numeric(runs$MOVESRunID, "runs$MOVESRunID", integer = TRUE)
  if (anyDuplicated(runs$MOVESRunID)) stop("runs has duplicate MOVESRunID values")
  run <- match(rates$MOVESRunID, runs$MOVESRunID)
  if (anyNA(run)) stop("Every rate must match a MOVESRunID in runs")
  if (any(!rates$processID %in% process_ids)) {
    stop("Unexpected processID for the ", basis, " rate table")
  }
  if (any(rates$sourceTypeID <= 0 | rates$fuelTypeID <= 0 |
          rates$modelYearID <= 0 | rates$yearID < rates$modelYearID)) {
    stop("Export disaggregated source, fuel and model year; modelYearID must not exceed yearID")
  }
  if (any(rates$relHumidity < 0 | rates$relHumidity > 100)) {
    stop("relHumidity must be in 0--100 percent")
  }
  if (any(rates[[rate_column]] < 0)) stop(rate_column, " cannot be negative")
  for (name in c("masterVersion", "defaultDatabaseUsed")) {
    if (anyNA(runs[[name]]) || any(!nzchar(trimws(runs[[name]])))) {
      stop("runs must record ", name)
    }
  }
  keys <- setdiff(names(rates), rate_column)
  if (anyDuplicated(rates[keys])) stop("Duplicate rate keys; keep runs/scenarios distinct")
  rates[[value_column]] <- rates[[rate_column]] *
    .moves_unit_scale(runs$massUnits[run], "mass")
  if ("temperature" %in% names(rates)) {
    rates$temperature_C <- (rates$temperature - 32) * 5 / 9
  }

  has_vehicles <- !missing(vehicles)
  unavailable <- !has_vehicles ||
    (is.character(vehicles) && (length(vehicles) != 1L || !file.exists(vehicles))) ||
    (is.data.frame(vehicles) && !nrow(vehicles))
  if (unavailable) {
    if (has_vehicles) warning("vehicles data unavailable; listing numeric source/fuel IDs only")
    vehicles <- unique(rates[c("sourceTypeID", "fuelTypeID")])
    vehicles$sourceTypeName <- NA_character_
    vehicles$fuelTypeDesc <- NA_character_
    vehicles <- vehicles[c("sourceTypeID", "sourceTypeName",
                           "fuelTypeID", "fuelTypeDesc")]
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
  vehicles <- vehicles[order(vehicles$sourceTypeID, vehicles$fuelTypeID), ]
  rownames(vehicles) <- NULL
  if (!nrow(vehicles)) stop("No vehicle/fuel names match the rate data")
  structure(list(rates = rates, runs = runs, vehicles = vehicles,
                 schema_version = 1L, basis = basis),
            class = "MOVESFactors")
}

#' Portable MOVES start-exhaust factors
#'
#' @description Loads local MOVES RatePerStart rates in grams per engine start.
#' No database, Java or MOVES runtime is used by these functions. Rates for
#' other scenarios can be generated with movesr
#' (\url{https://github.com/atmoschem/movesr}) and written with
#' \code{movesr_export_vein()}.
#' @param rates A data.frame or path to a RatePerStart CSV/TSV export. Omit
#' both arguments to load the bundled reference data.
#' @param runs MOVESRun rows with output units and model provenance.
#' @param vehicles Optional source/fuel name table.
#' @return A \code{MOVESFactors} object with \code{basis = "start"} and a
#' \code{gperstart} column.
#' @seealso \code{\link{ef_moves_start}}, \code{\link{moves_factors}}
#' @export
#' @examples
#' start <- moves_start_factors()
#' ef_moves_start("CO", 2020, factors = start,
#'                source_type_id = "Passenger Car",
#'                fuel_type_id = "Gasoline", select = list(hourID = 1))
moves_start_factors <- function(rates, runs, vehicles) {
  cache <- missing(rates) && missing(runs) && missing(vehicles)
  if (cache && exists("start", .moves_cache, inherits = FALSE)) {
    return(get("start", .moves_cache, inherits = FALSE))
  }
  if (missing(rates) && missing(runs)) {
    directory <- system.file("extdata", "moves", package = "vein")
    rates <- file.path(directory, "rateperstart.csv")
    runs <- file.path(directory, "movesrun_events.csv")
    if (missing(vehicles)) vehicles <- file.path(directory, "vehicles.csv")
  } else if (missing(rates) || missing(runs)) {
    stop("Supply both rates and runs, or omit both for bundled data")
  }
  result <- .moves_event_table(rates, runs, vehicles, "start", 2L,
                               "ratePerStart", "gperstart")
  if (cache) assign("start", result, envir = .moves_cache)
  result
}

#' Portable MOVES evaporative factors
#'
#' @description Loads local RatePerVehicle and RatePerProfile rates for
#' permeation, fuel leaks and fuel vapor venting of parked vehicles. All three
#' are grams per vehicle in the hour: MOVES divides the hourly emissions by the
#' vehicle population. Vapor venting is written to RatePerProfile only because
#' MOVES labels it with the temperature profile; its rate has the same basis.
#' They are not distance factors; evaporative emissions while driving are
#' distance factors (\code{\link{moves_factors}}, processes 11--13). Rates for
#' any county, month and weather can be generated with movesr
#' (\url{https://github.com/atmoschem/movesr}, \code{movesr_evap()} and
#' \code{movesr_export_vein()}).
#' @param rates A RatePerVehicle data.frame or file.
#' @param profile Optional RatePerProfile data.frame or file. Omit to load the
#' bundled reference or use only vehicle-based processes.
#' @param runs MOVESRun rows with output units and model provenance.
#' @param vehicles Optional source/fuel name table.
#' @return A \code{MOVESFactors} object with \code{basis = "evap"} and a
#' \code{gpervehicle} column (g per vehicle in the hour). The
#' \code{processID} identifies permeation (11), vapor venting (12) or fuel
#' leaks (13).
#' @seealso \code{\link{ef_moves_evap}}
#' @export
#' @examples
#' evap <- moves_evap_factors()
#' ef_moves_evap("THC", 2020, factors = evap,
#'               source_type_id = "Passenger Car",
#'               fuel_type_id = "Gasoline", select = list(hourID = 1))
moves_evap_factors <- function(rates, profile, runs, vehicles) {
  cache <- missing(rates) && missing(profile) && missing(runs) && missing(vehicles)
  if (cache && exists("evap", .moves_cache, inherits = FALSE)) {
    return(get("evap", .moves_cache, inherits = FALSE))
  }
  has_runs <- !missing(runs)
  has_profile <- !missing(profile)
  if (missing(rates) && !has_runs) {
    directory <- system.file("extdata", "moves", package = "vein")
    rates <- file.path(directory, "ratepervehicle.csv")
    profile <- file.path(directory, "rateperprofile.csv")
    runs <- file.path(directory, "movesrun_events.csv")
    if (missing(vehicles)) vehicles <- file.path(directory, "vehicles.csv")
    has_runs <- TRUE
    has_profile <- TRUE
  } else if (missing(rates) || !has_runs) {
    stop("Supply both rates and runs, or omit both for bundled data")
  }
  if (!is.character(rates) || !file.exists(rates)) {
    if (!is.data.frame(rates)) stop("rates must be a file or data.frame")
  }
  base <- .moves_event_table(rates, runs, vehicles, "evap", c(11L, 13L),
                             "ratePerVehicle", "gpervehicle")
  if (has_profile && !is.null(profile) &&
      (!is.character(profile) || file.exists(profile))) {
    extra <- .moves_event_table(profile, runs, vehicles, "evap", 12L,
                                "ratePerVehicle", "gpervehicle")
    for (name in setdiff(names(extra$rates), names(base$rates))) {
      base$rates[[name]] <- NA
    }
    for (name in setdiff(names(base$rates), names(extra$rates))) {
      extra$rates[[name]] <- NA
    }
    base$rates <- rbind(base$rates, extra$rates[names(base$rates)])
    base$rates <- base$rates[order(base$rates$processID, base$rates$pollutantID,
                                   base$rates$modelYearID, base$rates$hourID), ]
    rownames(base$rates) <- NULL
  }
  if (cache) assign("evap", base, envir = .moves_cache)
  base
}

.moves_event_filter <- function(rates, filters) {
  for (name in names(filters)) {
    value <- filters[[name]]
    multiple <- name %in% c("processID", "hourID")
    if ((!multiple && length(value) != 1L) || !length(value) || anyNA(value) ||
        inherits(value, "units") || !is.atomic(value) ||
        (is.numeric(value) && any(!is.finite(value)))) {
      stop("Select valid value(s) without units for ", name)
    }
    keep <- if (name %in% c("temperature_C", "temperature", "relHumidity")) {
      if (length(value) != 1L) stop(name, " must be scalar")
      abs(rates[[name]] - value) <= 1e-6
    } else if (multiple) {
      rates[[name]] %in% value
    } else {
      rates[[name]] == value
    }
    rates <- rates[!is.na(keep) & keep, , drop = FALSE]
  }
  rates
}

#' Evaluate MOVES start-exhaust factors
#'
#' @description Selects local MOVES start factors in grams per start. The
#' result may contain several hours or temperatures; filter with
#' \code{select}. This function has no speed argument because start emissions
#' are event-based, not distance-based.
#' @param pollutant MOVES pollutant ID or name; the bundled data contain CO and NOx.
#' @param model_year One manufacturing year.
#' @param factors A \code{MOVESFactors} object from \code{\link{moves_start_factors}}.
#' @param source_type_id MOVES source type ID or exact name.
#' @param fuel_type_id MOVES fuel type ID or exact name.
#' @param temperature Optional temperature in degrees C.
#' @param rel_humidity Optional relative humidity in percent.
#' @param select Named list of additional filters, e.g. \code{list(hourID = 1)}.
#' @return A data.frame with the selected dimensions and \code{gperstart}.
#' @seealso \code{\link{moves_start_factors}}, \code{\link{ef_moves}}
#' @export
#' @examples
#' ef_moves_start("NOx", 2020, source_type_id = "Passenger Car",
#'                fuel_type_id = "Gasoline", select = list(hourID = 1))
ef_moves_start <- function(pollutant = "CO", model_year,
                           factors = moves_start_factors(),
                           source_type_id = 21, fuel_type_id = 1,
                           temperature, rel_humidity, select = list()) {
  if (!inherits(factors, "MOVESFactors") || !identical(factors$basis, "start")) {
    stop("factors must be created by moves_start_factors()")
  }
  source_type_id <- .moves_id(source_type_id, factors$vehicles,
                              "sourceTypeID", "sourceTypeName", "source_type_id")
  fuel_type_id <- .moves_id(fuel_type_id, factors$vehicles,
                            "fuelTypeID", "fuelTypeDesc", "fuel_type_id")
  filters <- list(pollutantID = .moves_pollutant(pollutant),
                  modelYearID = model_year, sourceTypeID = source_type_id,
                  fuelTypeID = fuel_type_id, processID = 2L)
  if (!missing(temperature)) filters$temperature_C <- temperature
  if (!missing(rel_humidity)) filters$relHumidity <- rel_humidity
  if (length(intersect(names(filters), names(select)))) {
    stop("select duplicates a named argument")
  }
  filters <- c(filters, select)
  bad <- setdiff(names(filters), names(factors$rates))
  if (length(bad)) stop("Invalid selection columns: ", paste(bad, collapse = ", "))
  out <- .moves_event_filter(factors$rates, filters)
  if (!nrow(out)) stop("No MOVES start rates match the requested vehicle/scenario")
  out$basis <- "start"
  attr(out, "units") <- "g/start"
  out
}

#' Evaluate MOVES evaporative factors
#'
#' @description Selects local MOVES evaporative factors for permeation, fuel
#' leaks or vapor venting of parked vehicles. Rates are grams per vehicle in
#' the hour (hourly emissions divided by the vehicle population) for all three
#' processes; multiply by the number of vehicles for each hour. They are not
#' g/km.
#' @param pollutant MOVES pollutant ID or name; bundled data contain THC (1).
#' @param model_year One manufacturing year.
#' @param process Character vector: "permeation", "venting", "leaks" or "all".
#' @param factors A \code{MOVESFactors} object from \code{\link{moves_evap_factors}}.
#' @param source_type_id MOVES source type ID or exact name.
#' @param fuel_type_id MOVES fuel type ID or exact name.
#' @param temperature Optional temperature in degrees C.
#' @param rel_humidity Optional relative humidity in percent.
#' @param select Named list of additional filters.
#' @return A data.frame with the selected dimensions, \code{processID},
#' \code{basis} (source table: "vehicle" for RatePerVehicle, "profile" for
#' RatePerProfile) and \code{gpervehicle} (g/vehicle/h).
#' @seealso \code{\link{moves_evap_factors}}
#' @export
#' @examples
#' ef_moves_evap("THC", 2020, source_type_id = "Passenger Car",
#'               fuel_type_id = "Gasoline", select = list(hourID = 1))
ef_moves_evap <- function(pollutant = "THC", model_year,
                          process = c("all", "permeation", "venting", "leaks"),
                          factors = moves_evap_factors(),
                          source_type_id = 21, fuel_type_id = 1,
                          temperature, rel_humidity, select = list()) {
  if (!inherits(factors, "MOVESFactors") || !identical(factors$basis, "evap")) {
    stop("factors must be created by moves_evap_factors()")
  }
  process <- match.arg(process)
  process <- switch(process, all = 11:13, permeation = 11L,
                    venting = 12L, leaks = 13L)
  source_type_id <- .moves_id(source_type_id, factors$vehicles,
                              "sourceTypeID", "sourceTypeName", "source_type_id")
  fuel_type_id <- .moves_id(fuel_type_id, factors$vehicles,
                            "fuelTypeID", "fuelTypeDesc", "fuel_type_id")
  filters <- list(pollutantID = .moves_pollutant(pollutant),
                  modelYearID = model_year, sourceTypeID = source_type_id,
                  fuelTypeID = fuel_type_id, processID = process)
  if (!missing(temperature)) filters$temperature_C <- temperature
  if (!missing(rel_humidity)) filters$relHumidity <- rel_humidity
  if (length(intersect(names(filters), names(select)))) {
    stop("select duplicates a named argument")
  }
  filters <- c(filters, select)
  bad <- setdiff(names(filters), names(factors$rates))
  if (length(bad)) stop("Invalid selection columns: ", paste(bad, collapse = ", "))
  out <- .moves_event_filter(factors$rates, filters)
  if (!nrow(out)) stop("No MOVES evaporative rates match the requested vehicle/scenario")
  out$basis <- ifelse(out$processID == 12L, "profile", "vehicle")
  attr(out, "units") <- "g/vehicle/h"
  out
}
