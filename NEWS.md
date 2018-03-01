NEWS
===========

# vein 0.3.9 (Release date: 2018-03-01)

### vein 0.3.9 (Release date: 2018-03-01)

- included all examples with less than 5 secs.
- Preparing submission to CRAN.
- inventoyr prints directory with files.

# vein 0.3.8 (Release date: 2018-02-27)

### vein 0.3.8 (Release date: 2018-02-27)

- Improve inventory.
- Starts adding codecov.

# vein 0.3.7 (Release date: 2018-02-26)

### vein 0.3.7 (Release date: 2018-02-26)

- Improve inventory function.
- Change order of arguments on emis_merge.

# vein 0.3.6 (Release date: 2018-02-22)

### vein 0.3.6 (Release date: 2018-02-22)

- fix #67.
- make_grid now uses sf::st_make_grid

### vein 0.3.5 (Release date: 2018-02-22)

- Add emis_merge.
- improve inventory.

### vein 0.3.4 (Release date: 2018-02-21)

- improve inventory.

### vein 0.3.3 (Release date: 2018-02-08)

- Fix #62.

### vein 0.3.2 (Release date: 2018-02-08)

- Fix #60 and #61.

### vein 0.3.1 (Release date: 2018-02-07)

- Fix #57, #58 and #59.
- GriddedEmissionsInventory corrected orientation.

# vein 0.3.0 (Release date: 2018-02-05)

### vein 0.3.0 (Release date: 2018-02-05)

- Fix #49: Documentation in inventory est
- Fix #50: Fix repetition of x_DF
- Fix #51: deparse text
- Fix #52: separate objects in rm with ','
- Add class GriddedEmissionsArray
- Fix #28, data.table imported in emis_grd. Now very fast!
- Fix #55.
- Minor fix on demo(VEIN).
- emis and emis_cold adjust length of ef by length of columns of first
element of the list of data-frames.
- Revised all examples.

### vein 0.2.4 (Release date: 2018-01-31)

- new data set names profiles data(profiles)

### vein 0.2.3 (Release date: 2018-01-23) (Fernanda)

- sp now imported instead of Depend #47.
- Improved Documentation
- Minor fix
- emis and emis_cold adjust the length of ef to match ncol of veh.
- Fix #42

### vein 0.2.2-30 (Release date: 2018-01-10)

- Speciation of NMHC

### vein 0.2.2-29 (Release date: 2017-12-27)

- Fix #43: ADT

### vein 0.2.2-28 (Release date: 2017-12-22)

- Fix #4: list of data-frames not needed so far! vkm needs a numeric as veh

### vein 0.2.2-27 (Release date: 2017-12-18)

- Fix #2: Documentation of ef_nitro.

### vein 0.2.2-26 (Release date: 2017-10-08)

- Fix #35

### vein 0.2.2-25 (Release date: 2017-09-28)

- Fix #32

### vein 0.2.2-24 (Release date: 2017-09-19)

- Fix #31
- Updated speciation of paintings for CBMZ
- Update message in emis

### vein 0.2.2-23 (Release date: 2017-09-18)

- Fix #29

### vein 0.2.2-22 (Release date: 2017-09-18)(Viva Chile)

- Fix #27. 

### vein 0.2.2-21 (Release date: 2017-09-18)

- Fix #11 (reopen). Now prints in kg

### vein 0.2.2-20 (Release date: 2017-09-16)

- Fix #26

### vein 0.2.2-19 (Release date: 2017-09-15)

- Experimental: adding NMHC speciation for industrial and buildings painting

### vein 0.2.2-18 (Release date: 2017-09-11)

- Fix #25

### vein 0.2.2-17 (Release date: 2017-09-10)

- Fix #24

### vein 0.2.2-16 (Release date: 2017-09-09)

- Fix #23

### vein 0.2.2-15 (Release date: 2017-09-09)

- Fix #22

### vein 0.2.2-14 (Release date: 2017-09-09)

- Fix #11

### vein 0.2.2-13 (Release date: 2017-09-09)

- Fix #7 

### vein 0.2.2-12 (Release date: 2017-09-09)

- Fix #21 

### vein 0.2.2-11 (Release date: 2017-09-09)

- Fix #20

### vein 0.2.2-10 (Release date: 2017-09-04)

- Fix #10, #17 and #18 and demo

### vein 0.2.2-9 (Release date: 2017-09-02)

- Fix #3 and #15

### vein 0.2.2-8 (Release date: 2017-09-01)

- Fix #14, emis_wrf

### vein 0.2.2-7 (Release date: 2017-08-30)

- Fix #12, speciate now returns mol/h when spec = iag

### vein 0.2.2-6 (Release date: 2017-08-23)


- Minor update #8, to exclude na in veh of function my_age

### vein 0.2.2-5 (Release date: 2017-08-07)

- Update evaporative emissions emis_evap  to include lists of 'Vehicles' data-
frame

### vein 0.2.2-4 (Release date: 2017-08-05)

- emis and emis_cold include stop when ncol(veh) != length(ef) and when
veh is a list, length(veh) != ncol(speed). In emis agemax determined by ncol of
vehicles
- Fix demo

# vein 0.2.2-3 (Release date: 2017-08-03)

- speed in emis and emis cold is now a dataframe with columns as number of hours.
- netspeed argument isList changed to scheme
- Versions with three numbers. Fix speciate for "iag"
- Corrected sysdata for PC with gasoline euro PRE pollutant HC
- In emis, emis_cold and emis_paved: agemax  =  ncol(veh) by default

### vein 0.2.1-7 (Chiba) (Release date: 2017-05-18)

- Evaporative class with units d/day according emission guidelines EEA Tier 2.
Fix some documentation errors. ef_evap return unit (g). Fix speciate and some
documentation. field of data 'net' now has units
- Adds several S3 classes Vehicles, Speed, EmissionFactors, EmissionFactorsList

Emission, EmissionsArray, EmissionsList and methods print, plot and summary

## vein 0.1.1.1 (Release date: 2017-03-25)

- Added some categories to bcom
- To avoid confusion with the REMI model (www.remi.com),
the name was changed to Vehicular Emissions INventory Model (vein).

###  REMI 0.1.0-31 (Release date: 2017-03-13)

- New function: my_age. Distribute vehicle data using own vehicle distribution
from a numeric vector.
- Description title changed from "An R package for elaborating emissions
inventories" to "An R package for traffic emissions modelling".
- age functions now used colSums for messaging average age.
- sysdata for table of EMEP/EEA PC emission factors, TYPE now has value "ALL"
when EURO is "PRE".
- ef_ldv_speed size motor FC Euro IV, V, VI and VIc changed from "800_1400"
to "<=1400" to create have a faster and easier estimation with ef_ldv_scaled.
The category <=800 remains.
- ef_ldv_speed_cold called a database with some errors in ifelse closure.
Now fixed.
- Age function nows append element equal to the last element of the
vector.

##  REMI 0.1.0-21 (Amanda) (Release date: 2017-01-08)

- Age function nows append element equal to the last element of the
vector.
- bus fix in emissions_dow and emissions_cold when array=F.
- emissions 24 were changed to lapply over the length of ef.
- includes function vkm to calculate vkm.
- includes function emissions_paved for estimating resuspension from paved roads.
- Includes function ef_ldv_cold_list for list of cold start emission factors.
  Also, the function emissions_cold.
- The internal dataframe with EMEP emission factor for ldv vehicles
  changes type from "PRE_ECE" to "PRE".
- bus fix in emissions_dow and emissions_cold when array=F.
- emissions 24 were changed to lapply over the length of ef.

# REMI 0.1.0-0 (Release date: 2016-10-19)

- REMI released.
- Prior REMI 0.1.0-0 (Release date: 2016-10-19)
- The model started as a collection of rscripts named "remIAG".
see: Ibarra-Espinosa S., Ynoue R. 2016. REMI model: Bottom-up emissions
inventories for cities with lack of data. Journal of Earth Sciences &
Geotechnical Engineering. issn 1792--9660.
URL =
https://www.researchgate.net/publication/313253283_REMI_model_Bottom-up_emissions_inventories_for_cities_with_lack_of_data
