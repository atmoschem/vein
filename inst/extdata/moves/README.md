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
| Source / fuel | Passenger car (21), gasoline (1) |
| Pollutants | CO (2), NOx (3) |
| Process / road | Running exhaust (1), urban unrestricted access (5) |
| Vehicle model years | 2000--2025, inclusive |
| Average speed bins | All 16 MOVES positive-speed bins |
| Meteorology | 20, 10 and 30 degrees C for hours 1, 2 and 3 respectively; RH 50% |
| Time / fuel context | January weekday, with EPA default fuel and other inputs |
| Rows | 2 pollutants x 26 model years x 3 temperatures x 16 bins = 2,496 |

The three hours are artificial meteorological lookup points, not an observed
diurnal cycle. Fleet technology, fuel and other assumptions are EPA defaults
for this U.S. reference scenario. These factors do not constitute an Ecuador
calibration or coverage of all MOVES vehicles/pollutants/conditions.

## Files and provenance

- `rateperdistance.csv`: raw selected EPA output, in g/mile.
- `movesrun.csv`: the original run record, including output units, model and
  database versions, date and description. Run ID 3 is local to the producing
  output database, not a globally unique model identifier.
- `avgspeedbin.csv`: native speed-bin definitions exported from the default DB.
- `zonemonthhour.csv`: the exact meteorological overrides.
- `moveserror.csv`: empty because the successful run had no reported errors.
- `reference.mrs`: the executed RunSpec. All applicable passenger-car fuel types
  were included as required by MOVES6; the published table selects gasoline.
- `SHA256.json`: SHA-256 hashes of the original CSV exports.

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
at all nominal speeds. All 2,496 exported values are covered.

## Sources

- https://github.com/USEPA/EPA_MOVES_Model/tree/MOVES6.0.0
- https://www.epa.gov/moves/latest-version-motor-vehicle-emission-simulator-moves
- EPA-403-B-26-002, MOVES6 Technical Guidance, section 4.2.2.

EPA is the source of the model and default data. The portable R lookup and
optional interpolation are VEIN functionality, not an EPA-endorsed model update.
