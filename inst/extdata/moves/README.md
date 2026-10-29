# MOVES6 reference factors for portable R evaluation

These are **generated MOVES outputs**, not synthetic test values. They are
bundled so `moves_factors()` and `ef_moves()` work locally without MariaDB,
SQL, Java, a MOVES executable or a network connection.

## Coverage

| Attribute | Reference data |
| --- | --- |
| EPA release | MOVES6.0.0 |
| Source commit | `45805553e5afb575ae2d29daf89fe1bdbe4b8e56` |
| Default database | `movesdb20260729` |
| Inventory year | 2025 |
| Geography | Washtenaw County, Michigan, USA (26161), zone 261610 |
| Source / fuel | All 13 on-road types; 38 vehicle/fuel combinations |
| Pollutants | CO (2), NOx (3) |
| Process / road | Running exhaust (1), urban unrestricted access (5) |
| Vehicle model years | 2000--2025 where MOVES reports that combination |
| Average speed bins | All 16 MOVES positive-speed bins |
| Meteorology | 20, 10 and 30 degrees C for hours 1, 2 and 3 respectively; RH 50% |
| Time / fuel context | January weekend day (dayID 2), with EPA default fuel and other inputs |
| Rows | 86,304 (every group has 16 speed bins) |

Start exhaust is a separate event-based table: 5,394 rows of CO/NOx in g/start,
covering all 13 source types and five fuels. Evaporative THC has 2,124 rows
for permeation/leaks and 1,062 profile rows for vapor venting, all in grams per
vehicle in the hour (MOVES divides each hour's emissions by the population).
MOVES reports evaporative output for 12 source types and gasoline/E85
combinations in this scenario; combination long-haul trucks and diesel, CNG or
electric evaporative factors are not included.

The RUNSPEC lists 48 vehicle/fuel combinations because MOVES6 requires every
valid fuel for each selected source type. Ten combinations have no modelled
running-exhaust output in this scenario and are not in the distance table:
heavy electric buses/trucks, and CNG/electric motor homes.
`moves_vehicles()` lists the 38 combinations that have distance factors.

The three hours are artificial meteorological lookup points, not an observed
diurnal cycle. Fleet technology, fuel and other assumptions are EPA defaults
for this U.S. reference scenario. These factors do not constitute an Ecuador
calibration or coverage of all MOVES vehicles/pollutants/conditions.

## Files and provenance

- `rateperdistance.csv`: running-exhaust EPA output in g/mile.
- `rateperstart.csv`: start-exhaust EPA output in g/start.
- `ratepervehicle.csv`: evaporative permeation and fuel-leak output in grams
  per vehicle in the hour.
- `rateperprofile.csv`: evaporative vapor-venting output, including MOVES
  `temperatureProfileID`; same basis (grams per vehicle in the hour), not g/km.
- `movesrun.csv`: running-exhaust run record and output units.
- `movesrun_events.csv`: start/evaporative run record and output units.
- `moveserror.csv`, `moveserror_events.csv`: empty because the successful runs
  had no reported errors.
- `avgspeedbin.csv`: native speed-bin definitions exported from the default DB.
- `vehicles.csv`: all valid source/fuel names and IDs.
- `zonemonthhour.csv`: the exact meteorological overrides.
- `reference.mrs` and `events.mrs`: executed RunSpecs. All applicable fuel types
  were included as required by MOVES6.
- `SHA256.json`: SHA-256 hashes of the original CSV exports.

Start and evaporative factors are event-based. Do not pass them to `emis()` as
g/km factors. Start factors require start activity; evaporative factors require
the number of vehicles in each hour.

MOVES builds vapor venting only from the hours selected in the RunSpec (the
worker's `HourDay` holds just those hours), so a vehicle whose cold soak began
outside hours 1--3 contributes nothing here, and the three hours carry
artificial temperatures. The bundled venting values illustrate the format; they
are not full-day factors.

These running and start factors are reproduced by movesr
(https://github.com/atmoschem/movesr), a standalone R port of the MOVES rate
engine: all 86,304 running and 5,394 start rows within 7.2e-6 (MOVES prints 6
significant digits). Use `movesr_export_vein()` to write the same tables for
other counties, years, months, weather, fuels, pollutants and processes.

Generated on Linux on 2026-10-28 using the EPA source, Java 11, Go 1.26.10,
MariaDB 10.11.14 and one local worker. The successful run retrieved all 32
generated bundles and reported no MOVES warnings/errors. Its database was
shut down before the portable R evaluation checks. The production scripts
and detailed reproduction notes are in `data-raw/moves/` in the VEIN source
repository (excluded from the installed package).

The R importer preserves the original data and creates g/km values by dividing
g/mile by exactly 1.609344. It also converts the recorded Fahrenheit values to
Celsius. Default/County scale virtual link IDs include the speed-bin ID;
`locationID` decodes their zone while retaining the original link IDs. This
permits speed lookup without losing geographic identity.

The default bin evaluator uses the intervals in `avgspeedbin.csv`. In
particular, exactly 2.5 mph falls in bin 2, while bin 1's nominal speed for
linear interpolation is 2.5 mph. Reference tests evaluate bin 1 at 1.25 mph
and the other bins at their nominal speeds; linear interpolation is checked
at all nominal speeds. All 86,304 exported values are checked by the reference
test, which evaluates bins 1--16 for every pollutant, source/fuel combination
and one representative model year/hour.

## Sources

- https://github.com/USEPA/EPA_MOVES_Model/tree/MOVES6.0.0
- https://www.epa.gov/moves/latest-version-motor-vehicle-emission-simulator-moves
- EPA-403-B-26-002, MOVES6 Technical Guidance, section 4.2.2.

EPA is the source of the model and default data. The portable R lookup and
optional interpolation are VEIN functionality, not an EPA-endorsed model update.
