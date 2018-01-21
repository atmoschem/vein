## vein v0.2.3.9001-v0.2.3.9004 (Release date: 2018-01-18)
- sp now imported instead of Depend.
- Improved Documentation
## vein v0.2.3.9000 (Release date: 2018-01-18)
- vein and emis_save functions. This is a pre-release.
## vein v0.2.2-30 (Release date: 2018-01-10)
- Speciation of NMHC
## vein v0.2.2-29 (Release date: 2017-12-27)
- Fix #43: ADT
## vein v0.2.2-28 (Release date: 2017-12-22)
- Fix #4: list of data-frames not needed so far! vkm needs a numeric as veh
## vein v0.2.2-27 (Release date: 2017-12-18)
- Fix #2: Documentation of ef_nitro.
## vein v0.2.2-26 (Release date: 2017-10-08)
- Fix #35
____________________________________________________________________________________

## vein v0.2.2-25 (Release date: 2017-09-28)
- Fix #32
## vein v0.2.2-24 (Release date: 2017-09-19)
- Fix #31
- Updated speciation of paintings for CBMZ
- Update message in emis
## vein v0.2.2-23 (Release date: 2017-09-18)
- Fix #29
## vein v0.2.2-22 (Release date: 2017-09-18)(Viva Chile)
- Fix #27. 
## vein v0.2.2-21 (Release date: 2017-09-18)
- Fix #11 (reopen). Now prints in kg
## vein v0.2.2-20 (Release date: 2017-09-16)
- Fix #26
## vein v0.2.2-19 (Release date: 2017-09-15)
- Experimental: adding NMHC speciation for industrial and buildings painting
## vein v0.2.2-18 (Release date: 2017-09-11)
- Fix #25
## vein v0.2.2-17 (Release date: 2017-09-10)
- Fix #24
## vein v0.2.2-16 (Release date: 2017-09-09)
- Fix #23
## vein v0.2.2-15 (Release date: 2017-09-09)
- Fix #22
## vein v0.2.2-14 (Release date: 2017-09-09)
- Fix #11
## vein v0.2.2-13 (Release date: 2017-09-09)
- Fix #7 
## vein v0.2.2-12 (Release date: 2017-09-09)
- Fix #21 
## vein v0.2.2-11 (Release date: 2017-09-09)
- Fix #20
## vein v0.2.2-10 (Release date: 2017-09-04)
- Fix #10, #17 and #18 and demo
## vein v0.2.2-9 (Release date: 2017-09-02)
- Fix #3 and #15
## vein v0.2.2-8 (Release date: 2017-09-01)
- Fix #14, emis_wrf
## vein v0.2.2-7 (Release date: 2017-08-30)
- Fix #12, speciate now returns mol/h when spec = iag
## vein v0.2.2-6 (Release date: 2017-08-23)
- Minor update #8, to exclude na in veh of function my_age
## vein v0.2.2-5 (Release date: 2017-08-07)
- Update evaporative emissions emis_evap  to include lists of 'Vehicles' data-
frame
____________________________________________________________________________________

## vein v0.2.2-4 (Release date: 2017-08-05)

- emis and emis_cold include stop when ncol(veh) != length(ef) and when
veh is a list, length(veh) != ncol(speed). In emis agemax determined by ncol of
vehicles
- Fix demo

____________________________________________________________________________________

## vein v0.2.2-3 (Release date: 2017-08-03)

- speed in emis and emis cold is now a dataframe with columns as number of hours.
- netspeed argument isList changed to scheme
- Versions with three numbers. Fix speciate for "iag"
- Corrected sysdata for PC with gasoline euro PRE pollutant HC
- In emis, emis_cold and emis_paved: agemax  =  ncol(veh) by default

____________________________________________________________________________________

### vein v0.2.1-7 (Chiba) (Release date: 2017-05-18)

- Evaporative class with units d/day according emission guidelines EEA Tier 2.
Fix some documentation errors. ef_evap return unit (g). Fix speciate and some
documentation. field of data 'net' now has units
- Adds several S3 classes Vehicles, Speed, EmissionFactors, EmissionFactorsList
Emission, EmissionsArray, EmissionsList and methods print, plot and summary
____________________________________________________________________________________

### vein v0.1.1.1 (Release date: 2017-03-25)

- Added some categories to bcom
- To avoid confusion with the REMI model (www.remi.com),
the name was changed to Vehicular Emissions INventory Model (vein).

____________________________________________________________________________________

###  REMI v0.1.0-31 (Release date: 2017-03-13)

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

____________________________________________________________________________________

###  REMI v0.1.0-21 (Amanda) (Release date: 2017-01-08)

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

____________________________________________________________________________________


### REMI v0.1.0-0 (Release date: 2016-10-19)

- REMI released.

Prior REMI v0.1.0-0 (Release date: 2016-10-19)

- The model started as a collection of rscripts named "remIAG".
see: Ibarra-Espinosa S., Ynoue R. 2016. REMI model: Bottom-up emissions
inventories for cities with lack of data. Journal of Earth Sciences &
Geotechnical Engineering. issn 1792--9660.
URL =
https://www.researchgate.net/publication/313253283_REMI_model_Bottom-up_emissions_inventories_for_cities_with_lack_of_data

Versioning:
CRAN includes <big>.<major>.<minor> eg: 0.2.2
Github includes <big>.<major>.<minor>-<sub-minor> 0.2.2-28
