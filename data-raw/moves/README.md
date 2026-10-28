# Producing the portable MOVES reference dataset

These are data-production tools. `ef_moves()` and `moves_factors()` never invoke
them. R users only need the installed CSV data or a saved `MOVESFactors` object.

## Source and environment

- EPA source: https://github.com/USEPA/EPA_MOVES_Model
- Release: MOVES6.0.0; commit `45805553e5afb575ae2d29daf89fe1bdbe4b8e56`.
- Bundled default database: `movesdb20260729.zip`.
- Source checkout expected at `../moves/` relative to the VEIN repository.
- Python 3.6+, Java JDK 11 (used here; EPA recommends JDK 17).
- `bootstrap.py` downloads SHA-256-verified MariaDB 10.11.14 and Go 1.26.10
  binaries into the private work directory and builds the model with bundled Ant.

Run from any working directory:

```sh
python3 data-raw/moves/bootstrap.py
python3 data-raw/moves/generate_reference.py
```

By default the private work directory is `../moves_work/` relative to the VEIN
repository. Set `MOVES_WORK_DIR` and `MOVES_SOURCE_DIR` to use other locations.
The scripts use a private database at 127.0.0.1:13316, small database buffers,
a 512 MiB Java heap, one local worker, and one Go CPU. The database process is
shut down in a `finally` block. Existing system services and R libraries are
not part of this workflow.

## Scenario

`reference.mrs` requests default-scale rates for Washtenaw County, Michigan
(26161), inventory year 2025, January weekday, passenger cars, road type 5
(urban unrestricted). MOVES6 requires all applicable fuel types in the RunSpec;
the published reference export selects gasoline (fuelTypeID 1).

Only running-exhaust CO and NOx are requested. Model years 2000--2025 are
exported, with all 16 positive speed bins. An optional input table overrides
temperature/humidity only: hour 1 = 20 C, hour 2 = 10 C, hour 3 = 30 C, all at
50% relative humidity. These hours represent meteorological grid points, not
a measured diurnal cycle. Other model inputs remain EPA U.S. defaults.

## Output and publication

Exports are saved in the private work directory's `exports/` folder:

- `rateperdistance.csv`: raw selected output, retaining every dimension.
- `movesrun.csv`: corresponding run record, units and model/database provenance.
- `moveserror.csv`: the error table for the current run (must be empty).
- `zonemonthhour.csv`: the actual meteorological input points.
- `avgspeedbin.csv`: the EPA speed-bin definitions.
- `SHA256.json`: hashes of the exported CSV files.

The generator rejects runs with reported errors or no distance rates. Before
publication, validate the exact expected coverage and compare R evaluation
with the exported rate, converted from g/mile to g/km. For bin lookup, use
1.25 mph inside the first bin and nominal speeds for bins 2--16; exactly
2.5 mph belongs to bin 2. Check linear interpolation at all nominal bin speeds.
The package's reference-data tests perform this comparison. Also inspect the
run log for warnings: MOVES may exit with status 0 after an invalid RunSpec.

Copy the verified CSV files to `inst/extdata/moves/`, together with the RunSpec
and a README documenting the actual run and scope. The data are ordinary local
files, so they can also be distributed separately for larger scenario collections.

## Debugging notes from the initial Linux run

- `<inputdatabase>` replaces the complete default database. Meteorology-only
  overrides belong in `<databaseselections>` instead.
- All 14 output-breakdown elements, including the deprecated
  `estimateuncertainty` element, are needed by the MOVES6 XML reader. Omitting
  one can discard the whole output-breakdown selection.
- MOVES opens more than 30 database connections even for one worker. The private
  server permits 128 connections while retaining small buffers.

The generated data are model output for a specified U.S. reference scenario.
The R implementation is a portable lookup/interpolation layer; it does not
reimplement MOVES base-rate algorithms or infer unmodeled local fleet conditions.
