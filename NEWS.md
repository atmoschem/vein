NEWS
===========

### vein 1.4.0 (date: 2025-03-30)

- Add spec pm2025 for LDV and HDV

### vein 1.3.1 (date: 2025-03-30)

- Update spec "voc"
- In R 4.04 as.Date origin must be specified.
- This fixed dmonth

### vein 1.3.0 (date: 2025-03-28)

- Deprecated long_to_wide, wide_to_long, 
- emis_chem, in favor of emis_chem2 (Carter 2015)

### vein 1.2.1 (date: 2025-03-16)

- Update speciation for "voc": (CAMS)

### vein 1.2.0 (date: 2024-12-25)

- Deprecated age_ldv, age_hdv and age_mc in favor of age_veh.
- Deleted emis_merge, now all projects store emissions in databases.

### vein 1.1.4 (date: 2024-08-20)

- ef_ldv_speed when LCV G and eu longer than 1,  works 

### vein 1.1.2 (date: 2024-01-24)

- fix speciation "pm2023"

### vein 1.1.1 (date: 2023-10-03)

- speciation voc according EDGAR (voc 1..25)


### vein 1.1.0 (date: 2023-08-01)

- fortran files now end on .90
- manizales and europe now adds wear


### vein 1.0.5 (date: 2023-06-14)

- ef_china_speed now has long argument to process in long format


### vein 1.0.4 (date: 2023-05-31)

- project newer cetesb emission factors
add project to estimate emissions in Brazil
by state a year

### vein 1.0.3 (date: 2023-05-05)

- Converts ef_emfac FC gallons to grams
- add survival logical parameter in projects

### vein 1.0.2

- Change all units according units package version 0.8.2.
- Deprecated some functions.

### vein 1.0.1

- Add emis_emfac.
- Add ef_emfac.
- moves_rpdy reads tfs for when veh is data.frame

### vein 1.0.0

- Integrates all the recent changes.
- Todo: Add weather effects at hourly and street level
in the emission factors of all projects.
- Decrease the number of projects adding more scripts
as in the new chinese project.
- Add project to develop ensembles

### vein 0.9.94

- add chinese voc profiles

### vein 0.9.92

- add more env chinese ef
- remove ive. It is better to run MOVES and extract EF
- add project china_bu_chem
- add more vehicles in ef_wear
- update brazil_bu_chem
- emis_chem2 now return al variables for chemical mechanisms,
not only the matching pollutants. In this way, there will be
more pollutants with 0 mass as a result of emis_chem2
- add argument units_area into emis_grid, which allows to
generate flux as mass / cm2 (for chimere) or other area unit.
- merge several projects into brazil_bu_chem

### vein 0.9.91

- add hybrid gasoline and diesel to chinese EF
- Preparing chinese bottom-up project
- Mario added PM scale in ef_cetesb

### vein 0.9.9

- CRAN release

### vein 0.9.7

- add project brazil_bu_chem_month
- add speciation for liquid fuels E25, E100 and G

### vein 0.9.6

- Changes ef_eea from arabic number to roman and categories. 
Now there is consistency between ef_eea and ef_*_speed .
- get_project with *Europe* "eu_bu_chem" (2022-02-20).
- add workflow for project "eu_bu_chem" (2022-02-20)
- get_project with *Europe* "eu_bu_chem_simple" (2022-03-12).
- add function dmonth to return days of the month.

### vein 0.9.5

- Add IM effect on Ecuador EF.
- add project `manizales_bu`. TODO: add IM and EF EEA 2019. (2021-11-12)
- add IM projects for brazil (experimental). (2021-11-24)
- add IM projects for sebrazil (experimental). (2021-12-22)
- fix tests for units package 0.8.0
- preparing CRAN release

### vein 0.9.4 (5 years!!)
- add CBMZ into chem_vein. Based on Yang Zhang Labs data and Carter 2015 (Release date: 2021-06-28)
- add projects `curitiba` and `masp2020`  (Release date: 2021-06-28)
- fix chem_vein2 for CB4 and CBMZ  (Release date: 2021-07-08)
- add CB05opt2 into emis_chem2
- fix fe ETOH on cetesb
- add add_miles and family add distance
- add dist argument into Speed to obtain miles/h
- add function moves_speed to return speed bins used by MOVES
- changes plots of vein classes, now imports fields::image.plot
- add moves_exhaust
- add data(decoder) for MOVES
- add ef_eea, updated european emission factors
- add fuel E85 exhaust-evaporative and CNG-exhaust in NMHC speciation
- add Ecuador
- For MOVES contributions, added Joao Bazzo
- reduced size for ef_hdv_speed. Speciation use `speciate`.
- Deprecated ef_emfac. I never used and it was too heavy.

### vein 0.9.3 (Release date: 2021-06-09)
- Fix RCHO in ef_cetesb
- Change numeric_dc to vector_dv to avoid copying (dotCall64)
- update gitlab link for veinextras in get_project
- fix ef_cetesb #211 project ef_cetesb
- fix #207 NH3 in ef_cetesb
- Fix warning units "veh", defining "veh" with .onLoad and removing with .onULoad
- update NMHC for BRAZIL. implies that ethanol from exhaust must be estimated separatly
- Add project brazil_bu_chem_streets
- Add 2020 EF for cetesb (provisional until new official EF, 09/06/2020)
- Update import of sf (1.0)

### vein 0.9.1-0.9.1.2 (Release date: 2021-02-15)
- Add deterioration factors in ef_cetesb
- Add OpenMP again and dotCall64. Thanks to Avraham Adler @aadler
- Add project sebr_cb05co2
- Fix and resubmit to cran to fix init.c

### vein 0.9.1 (Release date: 2020-12-05)
- Update speciate (2020-11-27)
- Add top-down approach with chemistry for WRF (2020-12-04)
- Update ef_cetesb with scale = "tunnel"
- Update speciate for exhaust E100 and chem_vein (2020-12-28)
- remove NMHC species form ef_hdv_speed because already available in speciate

### vein 0.9.0.2 (Release date: 2020-10-07)

- Add mechanisms neu_cb05, neu_cb05v2, pmneu, pmneu2 (thanks to Daniel Schuch from Northeastern University) (2020-10-13)
- Add x to colplot (2020-11-14)
- Add ELEC EF to ef_cetesb (2020-11-14)
- Add ef_emfac (2020-11-14)
- speciate names upper and without point (e_pm2.55i -> E_PM25I) (2020-11-17)
- Update scale tunnel in ef_cetesb (2020-11-23)

### vein 0.9.0.1 (Release date: 2020-10-13)
- REMOVED OPENMP DUE TO MAC OS ISSUES ANd FORTRAN

### vein 0.9.0 (Release date: 2020-09-23) "Stuck-at-home"

- evaporative emissions from ef_cetesb return units with grams (2020-06-11)
- Added projet "emislacovid" (2020-07-11)
- Improved colplot (2020-07-17)
- Added scaled brazilian emission factors with tunnel measurements (2020-07-21) and speciations.
- Remove evaporative emission factors of flex motorcycles for year 2008. (2020-07-23)
- Add verbose argument in emis_merge to show list of files being reador not. (2020-07-24)
- Replaces net$geometry by st_geometry(net) to avoid geometry name. (2020-07-24)
- Add argument seconds to emis_order to control time difference with UTC. This is
useful when local time changes for daylight (2020-07-26)
- Add projects csv, csv.gz and cb05. (2020-08-06)
- Add option to include monthly emission factors in emis_hot_td
- -Add OpenMP option in all functions with fortran = TRUE (2020-09-23)-.
- -Add the function check_nt() to return max number of threads (2020-09-23)-.
- Add option to write ef_cetesb in csv file. (2020-09-23).

### vein 0.8.9  (Release date: 2020-06-11)

- Revise mileage functions. Especifically, fix KM_LDC_FLEX so that 
it does not increase mileage over time. (2020-05-08)
- Moved to https://gitlab.com/ibarraespinosa/vein. (2020-05-08)
- Add add_lkm to add units km into R objects.(2020-05-24)
- Enhance emis_post to read Emissions and EmissionsArray classes (2020-05-24)
- Assume evaporative emission factors from PC into MC ef_cetesb

### vein 0.8.8  (Release date: 2020-03-11)

- when LCV and eu is V, ef_ldv_speed return wrong values, as shown on EEA 
guidelines 2016. When `v` is 'LCV' and `eu` is 'V', replaces `v` by 'PC' 
and `cc` by >'2000' see issue #204". I will update for version 2019,
maybe in a newer function, o updating the existing ones.
- Adds get_project to easily estimate emissions using a template
- New emis_order with aliases to weekly. Thanks Daniel.

### vein 0.8.7  (Release date: 2020-02-17)

- emis_grid now returns emissions as flux: mass / area / time (implicit)
- Deprecates matvect
- Adds argument 'flux' in emis_grid to return polytgon flux or points, in a similar
fashion with EDGAR.
- remove eixport and sp dependencies.
- fix #73

### vein 0.8.6  (Release date: 2020-01-19)

- CRAN. Added Fortran in emis, emis_cold_td and emis_hot_td.

### vein 0.8.5  (Release date: 2020-01-17)

- CRAN. Added Fortran in emis, emis_cold_td and emis_hot_td.

# #vein 0.8.4  (Release date: 2019-12-22)

- just add more tests
- adds Fortran function to make emis faster
- improves emis, now faster

### vein 0.8.3  (Release date: 2019-11-14)

- grid_emis now it is like a proper opposite of emis_grid, becuase it reads spatial 
grid and transform it Lines, considering all the emissionsColumns) 
- adds st_explode.

### vein 0.8.2  (Release date: 2019-11-06)

- emis now accepts ef as units or EmissionFactorsList. When it is units,
speed is not needed and if present shows a warning

### vein 0.8.1  (Release date: 2019-10-04)

- show.main = FALSE in inventory
- remove warning in make_grid when checking class for bbox

### vein 0.8.0  (Release date: 2019-09-05)

- Update vein_notes

### vein 0.7.14  (Release date: 2019-08-20)

- update emis_to_streets. Now it check class of dfemis and transform to data.frame if is sf
- Create grids from bbox (sf)
- adding df argument at ef*scaled (again!)

### vein 0.7.13  (Release date: 2019-08-06)

- update emis_grid when spobj comes from data.table
- Add function in emis_grid, supporting evaluated parsed text ("sum", "mean", etc)


### vein 0.7.12  (Release date: 2019-07-21)

- Updated inventory for Windows
- Fix projection of cetesb EF

### vein 0.7.11  (Release date: 2019-07-15)

- Fixed ugly bug in emis_grid!
- Added test for emis_grid
- Add streets profile in emis_to_streets


### vein 0.7.10  (Release date: 2019-06-21)

- Add projection forward and backward in ef_cetesb

### vein 0.7.9  (Release date: 2019-06-14)

- Found error on ef_china. Specifically on sulfur correction. fix #182
- Adding more tests on ef_china.
- Improve documentation of ef_china
- Update in age. Now it returns number of survived fleet by year. Really cool!


### vein 0.7.8  (Release date: 2019-06-09)

- Adjustment in emis_chem
- Adds long_to_wide
- Improves documentation
- Enhances split_emis
- Upload to CRAN

### vein 0.7.7  (Release date: 2019-06-03)

- Adjustment in emis_chem

# vein 0.7.6  (Release date: 2019-05-08)

- Fix erro in some aldehyes LCV
- improves long_to_wide.
- Adds to_latex.

### vein 0.7.5  (Release date: 2019-05-08)

- Add long_to_wide

### vein 0.7.4  (Release date: 2019-05-08)

- Improves ef_nitro and documentation. HOw it is easier.

### vein 0.7.3  (Release date: 2019-05-08)

- add speciation in evaporative emission factors.
- improve emis_chem
- Splits m,p-xylene in m-xylene and p-xylene fix #176
- Deprecated emis_wrf in favou of eixport::to_as4wrf #fix #174

### vein 0.7.2  (Release date: 2019-04-26)

- adds data pollutants including CAS ID and molar mass.
- replace data fe2015 from ef_cetesb updated and remove columns related to Motorcycles.
- Adds emis_chem to aggregate emissions by chemical mechanisms  "SAPRC", "RACM", 
"RADM2", "CBMZ", "MOZART", "SAPRC99", "CB05", "CB06CMAQ", "RACM2CMAQ", "SAPRC99CMAQ",
"SAPRC07CMAQ" or "SAPRC07A" and more.
- Add long_to_wide

### vein 0.7.1  (Release date: 2019-04-13)

- adds ef_china.
- fix k in age_hdv.
- ADd remove_units


### vein 0.7.0  (Release date: 2019-03-25)

- Consolidation fo all minor release.
- Improves print methods for classes.

### vein 0.6.12 (Release date: 2019-03-25)

- Fix #168 update ef cetesb 2017.
- fix #169. Adds function age, which incorporated survival functions:
Gompertz, Weibull, logistic, including literature references. This function aims
to replace age_ldv, age_moto and age_hdv.
- Improves documentation age*


### vein 0.6.11 (Release date: 2019-03-18)

- Fix ef_*_scale. 
- The problem was solved adding k2 to ef*speed

### vein 0.6.10 (Release date: 2019-03-01)

- Enhances vein_notes.

### vein 0.6.9 (Release date: 2019-02-25)

- Fix #157. age_ldv(1, agemax = 1) now returns 1 [veh]
- Fix #158, new argument namerows to change row.names oin age* functions.
- Change argument message to verbose in all age* functions default = FALSE.
- Add emission factors of hybrid vehicles. EMEP/EEA air pollutant emission inventory
guidebook 2016 (Leonidas Ntziachristos, Zissis Samaras, 2016), only publish
factors for Euro IV technology. In order to have a complete data base I projected
these factors to euro V, VI and VIc with the same values as IV. When new 
literature is available this will be updated.

### vein 0.6.8 (Release date: 2019-02-21)

- Fix #152. Adds emis_hot_td hot exhaust estimation with top-down approach.
- Improve documentation.
- Allow pro_month varies across each simple feature or row os Vehicle data.frame.
- Fix minor bugs.

### vein 0.6.7 (Release date: 2019-02-18)

- Fix #150. Add options to emis_cold_td and emis_evap to add columns to output.
- Add verbose argument in emis_cold_td and emis_evap.
- Add message in emis. hour and day will be deprecated.
- Add warnings in ef_evap, and now has options for months and accepts several ef.
- Add argument speed in ef_ldv_speed, ef_hdv_speed and ef_ldv_cold.


### vein 0.6.6 (Release date: 2019-02-14)

- Fix #146. now dt of ef_evap accept numeric or character vectors.
- Other arguments of ef_evap can be several.
- Add stop message on ef_evap.
- Add argument of ltrip to convert g/trip in g/km and kmday to convert g/day to g/km
- Add new emis_evap function, and the older now has the name emis_evap2.
- Deprecates running_losses and hot_soak and replaced by the new emis_evap
function.

### vein 0.6.5 (Release date: 2019-02-12)

- Created function emis_cold_td for top-down approach. Fix #142
- no more time units,fix #118
- Add new unit 'veh', for vehicles, dimensionless.
- Deprecates classes Evaporative and EmissionsList.

### vein 0.6.4 (Release date: 2019-02-11)

- Adds support for top-down estimation in emis_cold. Fix ###142.]
This is, it returns a data-frame when profile is missing.
What would happen if annaul activity is used when profile?
WHat happen if this profile are normalized projections for future?
The result would be an EmissionsArray with emissions projections.
However, the emission factors would be contant, which would be incorrect
due to emissions degradations. Thusm it would be a good idea start to think
in a dedicated function for top-down estimations, which ideally, would cover
cold start and evaporative cases. Also, what happens for automatic monthly estimation?
Perhaps, 0.7.
- Adds cold_mileage depending on length of trip and temperature, from 
EMEP/EEA air pollutant emission inventory guidebook, p60.


### vein 0.6.3 (Release date: 2019-02-07)

- Fix milage equation of  fkm$KM_LDV_GNV. Fix #137.
- Fix "V", "VI", "VIc". Fix #138
- Fix #139
- Fix #140. Add base year in ef_cetesb.
- Fix #141
    
### vein 0.6.2 (Release date: 2019-01-29)

- fix some metals in ldv, m
- Adds data with name of all pollutants covered.


### vein 0.6.1 (Release date: 2019-01-11)

- Adds split_emis. Fix #135

### vein 0.6.0 (Release date: 2019-01-11) 'Changchung'

- Add PM characteristics:
- Add Active Surface (cm2/km): "AS_urban", "AS_rural", "AS_highway",
- Add number of particles: "N_urban", "N_rural", "N_highway",
"N_50nm_urban", "N_50_100nm_rural", "N_100_1000nm_highway".

### vein 0.5.9 (Release date: 2019-01-11)

- fix #134.

### vein 0.5.8 (Release date: 2018-12-29)

- Fix EF of metals fix #91.

### vein 0.5.7 (Release date: 2018-11-29)

- Opens main.R in inventory(, fix #132.
- Adds matvect in any age* by row, fix #130.
- emis can read vehicles as 'sf' objects setting geometry to NULL, fix #116.
- Fix message on age* function when there is a NA in traffic, fix #131.
- Enhance inventory.

### vein 0.5.6 (Release date: 2018-11-26)

- Add matvect, just a helper function to multiply matrices by row or 
column with vectors.
- Add logo at help page.
- Add tests.

### vein 0.5.5 (Release date: 2018-11-13)

- Fix top-down estimation

### vein 0.5.4 (Release date: 2018-10-02)

- Fix evaporative emission factors on CETESB.
- Remove SO2 from cetesb. It must be calculated based on FC and content
of S. The same applies for Pb.
- Add ignore in emis_merge.


### vein 0.5.3 (Release date: 2018-10-02)

- Adds evaporative emission factors on CETESB.

### vein 0.5.2 (Release date: 2018-10-02)

- Adds grid_emis. This function allocates a grid of emissions into streets by
each grid cell.
- Improves emis_dist.
- Improves emis_grid.
- Add verbose in emis_dist and grid_emis.


### vein 0.5.1 (Release date: 2018-09-30)

- Improves emis_dist

### vein 0.5.0 (Release date: 2018-09-24) 'Takamatsu'

- Consolidates all news functions.
- Add function ef_im for emission factors with normal deterioration that would
be approved in a inspection and mantainence program.
- Add ef_fe for taking into account for high emitters.

# vein 0.4.6 (Release date: 2018-09-06)

- Add base emission factors from IVE model.
- Add argument utc to convert local emissions into UTC emissions.

### vein 0.4.5 (Release date: 2018-09-05)

- enhances emis_order. Now covers GriddedEmissionsArray, sf, data.frame, matrix,
or any Spatial. Fix #110.

### vein 0.4.4 (Release date: 2018-09-02)

- add function vein_notes.
- add ef_cetesb #90. 

### vein 0.4.3 (Release date: 2018-08-29)

- add function ef_fun #106.
- Fix missing isopentane #92.
- Fix #107.
- included more options to emis_merge

### vein 0.4.2 (Release date: 2018-08-18)

- add function ef_fun #103.

### vein 0.4.1 (Release date: 2018-08-07)

- add bystreet in my_age.

### vein 0.4.0 (Release date: 2018-07-13)

- add emis_dist. Add test. 

### vein 0.3.36 (Release date: 2018-07-11)

- add emis_order (alias weekly) and emis_source. Also, updated reference.

### vein 0.3.35 (Release date: 2018-06-11)

- Fix emis_wrf. Minor fix for GMD.

### vein 0.3.34 (Release date: 2018-06-08)

- Add message in emis_grid of original and resulting emissions.
Other minor fix for GMD.

### vein 0.3.32 (Release date: 2018-06-04)

- make_grid with wrfinput fixed.

### vein 0.3.31 (Release date: 2018-06-01)

- Fix #69. Now make_grid prints the number of Longitude and Latitude cells
(points). when spobj is "character", it is a path to wrfinput file and
then runs eixport::wrf_grid to create a grid based on a wrf_input file.

### vein 0.3.30 (Release date: 2018-05-26)

- Prevent emis_grid to sum 1 + NA or 1 + NaN. Now it would be 1 + 0.
- Also, emis_merge now it is conservative.
- Deprecate emis_wrf in favout of eixport functions  to_wrf and to_as4wrf.

### vein 0.3.29 (Release date: 2018-05-25)

- Fix emis_merge.

### vein 0.3.28 (Release date: 2018-05-24)

- Adds argument as_df in vkm. This allows to transform the resulting
array into with dimensions rows  = streets and columns  = hours x days.
- Removes array requirement on emis_paved.
- Add speciation PM pmiag for WRF Chem: E_SO4i, E_SO4j, E_NO3i, E_NO3j, E_MP2.5i
E_MP2.5j, E_ORGi, E_ORGj, E_ECi, E_ECj , H2O.

### vein 0.3.27 (Release date: 2018-05-23)

- fix emis_merge. 

### vein 0.3.26 (Release date: 2018-05-20)

- fix #87 and #88

### vein 0.3.25 (Release date: 2018-05-19)

- Add factors of metals and NMHC for speed functions.

### vein 0.3.24 (Release date: 2018-05-17)

- Add factors of Dioxins and furans: PCDD, PCDF and PCB expressed as 
(g equivalent toxicity / km).

### vein 0.3.23 (Release date: 2018-05-16)

- Emission factors of Also polycyclic aromatic hydrocarbons (PAHs) and
persistent organi pollutants (POPs) in ef_hdv_speed.

### vein 0.3.22 (Release date: 2018-05-15)

- Emission factors of Also polycyclic aromatic hydrocarbons (PAHs) and
persistent organi pollutants (POPs) in ef_ldv_speed.
- Add contributing.
- remove warning about df in ef_ldv_scaled 

### vein 0.3.21 (Release date: 2018-05-14)

- Fix #84, #75, and #44. Add emission factors of CO2, SO2, NMHC and CH4.

### vein 0.3.20 (Release date: 2018-05-13)

- Fix #85. Add corrections in emis_wear.
- Add test for emis_wear.

### vein 0.3.19 (Release date: 2018-05-05)

- Fix #82.

### vein 0.3.18 (Release date: 2018-05-01)

- add fuel_corr function #65.

### vein 0.3.17 (Release date: 2018-04-22)

- add net argument in temp_fact, netspeed and emis_post #70.

### vein 0.3.16 (Release date: 2018-04-09)

- fix bug in post.R by inventory.

### vein 0.3.15 (Release date: 2018-04-01)

- ef_wear and emis_wear updated.
- age* functions now incldues default name "veh"

### vein 0.3.14 (Release date: 2018-03-31)

- emis_paved now includes default values for k = 0.62 g/km,
sL1 = 0.6 (g/m^2), sL2 = 0.2 (g/m^2), sL3 = 0.06 (g/m^2), sL4 = 0.03 (g/m^2).
- profile in emis, vkm and emis_cold checks for data.frame, matrix and if its a vector, it is transformed into matrix.

### vein 0.3.13 (Release date: 2018-03-28)

- emis_grid internally filters inputs columns keeping only numeric
columns.

### vein 0.3.12 (Release date: 2018-03-25)

- Change default values of hour and day in emis* functions (fix #76).
- Add tests.
- emis_post now derives hours and days from EmissionsArray.
- Inventory includes argument rush.hour for creating a template for estimations
of only rush hour.

### vein 0.3.11 (Release date: 2018-03-09)

### vein 0.3.10 (Release date: 2018-03-09)

- fix #74.
- Add tests.

### vein 0.3.10 (Release date: 2018-03-03)

- Improve inventory.
- fix #72.


### vein 0.3.9 (Release date: 2018-03-01)

- included all examples with less than 5 secs.
- Preparing submission to CRAN.
- inventoyr prints directory with files.

# vein 0.3.8 (Release date: 2018-02-27)

#### vein 0.3.8 (Release date: 2018-02-27)

- Improve inventory.
- Starts adding codecov.


### vein 0.3.7 (Release date: 2018-02-26)

- Improve inventory function.
- Change order of arguments on emis_merge.


### vein 0.3.6 (Release date: 2018-02-22)

- fix #67.
- make_grid now uses sf::st_make_grid

#### vein 0.3.5 (Release date: 2018-02-22)

- Add emis_merge.
- improve inventory.

### vein 0.3.4 (Release date: 2018-02-21)

- improve inventory.

### vein 0.3.3 (Release date: 2018-02-08)

- Fix #62.

### vein 0.3.2 (Release date: 2018-02-08)

- Fix #60 and #61.

#### vein 0.3.1 (Release date: 2018-02-07)

- Fix #57, #58 and #59.
- GriddedEmissionsInventory corrected orientation.

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

#### vein 0.2.2-19 (Release date: 2017-09-15)

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

### vein 0.2.2-3 (Release date: 2017-08-03)

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

### vein 0.1.1.1 (Release date: 2017-03-25)

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

###  REMI 0.1.0-21 (Amanda) (Release date: 2017-01-08)

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

### REMI 0.1.0-0 (Release date: 2016-10-19)

- REMI released.
- Prior REMI 0.1.0-0 (Release date: 2016-10-19)
- The model started as a collection of rscripts named "remIAG".
see: Ibarra-Espinosa S., Ynoue R. 2016. REMI model: Bottom-up emissions
inventories for cities with lack of data. Journal of Earth Sciences &
Geotechnical Engineering. issn 1792--9660.
URL =
https://www.researchgate.net/publication/313253283_REMI_model_Bottom-up_emissions_inventories_for_cities_with_lack_of_data


