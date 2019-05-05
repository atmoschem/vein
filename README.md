---
title: "VEIN model"
author: "Sergio Ibarra-Espinosa"
date: "19 de Octubre de 2016"
output: html_document
---


[![Travis-CI Build Status](https://travis-ci.org/atmoschem/vein.svg?branch=master)](https://travis-ci.org/atmoschem/vein)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ibarraespinosa/vein?branch=master&svg=true)](https://ci.appveyor.com/project/ibarraespinosa/vein)
[![](http://cranlogs.r-pkg.org/badges/vein)](http://cran.rstudio.com/web/packages/vein/index.html)
[![DOI](https://zenodo.org/badge/88201850.svg)](https://zenodo.org/badge/latestdoi/88201850)
[![Coverage Status](https://img.shields.io/codecov/c/github/atmoschem/vein/master.svg)](https://codecov.io/github/atmoschem/vein?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/vein)](http://cran.r-project.org/web/packages/vein) 
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/vein?color=orange)](http://cran.r-project.org/package=vein)
[![Package Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Github Stars](https://img.shields.io/github/stars/atmoschem/vein.svg?style=social&label=Github)](https://github.com/atmoschem/vein)

# **V**ehicular **E**missions **IN**ventory **(VEIN)** model. ![](https://i.imgur.com/dp8spyp.png)


![](https://i.imgur.com/RKftr2z.gif)

## What is VEIN?

Vehicular Emissions Inventories. An R package to estimate vehicular emissions.
It currently covers the following pollutants in speed functions:

### European emission factors for all available vehicle categories exhaust:

- **Criteria (g/km)**: "CO", "NOx", "HC", "PM", "CH4", "NMHC", "CO2", "SO2",
"Pb", "FC" (Fuel Consumption),"NO", "NO2".
- **PAH and POP**: indeno(1,2,3-cd)pyrene", "benzo(k)fluoranthene",
"benzo(b)fluoranthene", "benzo(ghi)perylene", "fluoranthene",
"benzo(a)pyrene", "pyrene", "perylene",  "anthanthrene", "benzo(b)fluorene",
"benzo(e)pyrene", "triphenylene", "benzo(j)fluoranthene",
"dibenzo(a,j)anthacene", "dibenzo(a,l)pyrene", "3,6-dimethyl-phenanthrene",
"benzo(a)anthracene", "acenaphthylene", "acenapthene",
"chrysene", "phenanthrene", "napthalene",  "anthracene", "coronene",
"dibenzo(ah)anthracene".
- **Dioxins and Furans (g/km)**: "PCDD", "PCDF", "PCB".
- **Metals (g/km)**: "As", "Cd", "Cr", "Cu", "Hg", "Ni", "Pb", "Se", "Zn".
- **NMHC (g/km)**:
- _ALKANES_: "ethane", "propane", "butane", "isobutane", "pentane",
"isopentane", "hexane", "heptane", "octane", "2-methylhexane", "nonane",
"2-methylheptane", "3-methylhexane", "decane", "3-methylheptane",
"alkanes_C10_C12", "alkanes_C13".
- _CYCLOALKANES_: "cycloalcanes".
- _ALKENES_: "ethylene", "propylene", "propadiene", "1-butene",
"isobutene", "2-butene", "1,3-butadiene", "1-pentene", "2-pentene",
"1-hexene", "dimethylhexene".
- _ALKYNES_:"1-butyne", "propyne", "acetylene".
- _ALDEHYDES_: "formaldehyde", "acetaldehyde", "acrolein", "benzaldehyde",
"crotonaldehyde", "methacrolein", "butyraldehyde", "isobutanaldehyde",
"propionaldehyde", "hexanal", "i_valeraldehyde", "valeraldehyde",
"o_tolualdehyde", "m_tolualdehyde", "p_tolualdehyde".
- _KETONES_: "acetone", "methylethlketone".
- _AROMATICS_: "toluene", "ethylbenzene", 
"m-xylene",  "p-xylene", "o-xylene",
"1,2,3-trimethylbenzene", "1,2,4-trimethylbenzene",
"1,3,5-trimethylbenzene", "styrene", "benzene", "C9", "C10", "C13".
- **Active Surface (cm2/km)**
- "AS_urban", "AS_rural", "AS_highway".
- **Total Number of particles (N/km)**
- "N_urban", "N_rural", "N_highway", "N_50nm_urban", "N_50_100nm_rural", "N_100_1000nm_highway".

### European emission factors speciation for evapoative emissions:

- **Criteria (g/km)**: "NMHC".
- **NMHC (g/km)**:
- _ALKANES_: "ethane", "propane", "n-butane", "i-pentane", "n-pentane",
"2-methylheptane", "3-methylheptane", "n-hexane", "n-heptane"
- _ALKENES_: "ethene", "propene", "1-butene", "trans-2-butene", "isobutene", 
"cis-2-butene",  "1,3-butadiene", "trans-2-pentene", "cis-2-pentene", "isoprene"
- _ALKYNES_:"propyne", "acetylene".
- _AROMATICS_: "benzene", "toluene", "ethylbenzene", "m-xylene", "o-xylene",
"1,2,3-trimethylbenzene" and "1,3,5-trimethylbenzene", 

### Brazilian emission factors for all available vehicle categories:

- "COd", "HCd", "NMHCd", "CH4", "NOxd", "CO2"
"PM", "N2O", "KML", "FC", "NO2d", "NOd", "gCO2/KWH", "RCHOd",
"CO", "HC", "NMHC", "NOx", "NO2" ,"NO", "RCHO"

### Brazilian speciation based on IAG/USP (Fátima's group) studies:

- "e_eth", "e_hc3", "e_hc5", "e_hc8", "e_ol2", "e_olt", "e_oli", "e_iso",
"e_tol", "e_xyl", "e_c2h5oh", "e_ald", "e_hcho", "e_ch3oh", "e_ket",
"E_SO4i", "E_SO4j", "E_NO3i", "E_NO3j", "E_MP2.5i", "E_MP2.5j", "E_ORGi",
"E_ORGj", "E_ECi", "E_ECj"

### Base emission factors from International Emission Model (IVE)  for all available vehicle categories:

- "VOC_gkm", "CO_gkm", "NOx_gkm", "PM_gkm", "Pb_gkm", "SO2_gkm", "NH3_gkm", 
"ONE_3_butadiene_gkm", "formaldehyde_gkm", "acetaldehyde_gkm", "benzene_gkm",
"EVAP_gkm", "CO2_gkm", "N20_gkm", "CH4_gkm", "VOC_gstart", "CO_gstart", 
"NOx_gstart", "PM_gstart", "Pb_gstart", "SO2_gstart", "NH3_gstart",
"ONE_3butadiene_gstart", "formaldehyde_gstart","acetaldehyde_gstart",
"benzene_gstart", "EVAP_gstart", "CO2_gstart", "N20_gstart", 
"CH4_gstart"

### Emission factors from Chinese emission guidelines

- "CO", "NOx", "HC", "PM10", "PM2.5".
- They depend on humidity, temperature, altitude and  other parameters.



### System requirements

vein imports functions from spatial packages listed below. In order to
install these packages, firstly the user must install the requirements
mentioned [here](https://www.github.com/r-spatial/sf).

### Packages needed

After installing system dependencies, you will need these packages:

- [sf](https://github.com/r-spatial/sf)
- [sp](https://github.com/edzer/sp/) 
- [lwgeom](https://github.com/r-spatial/lwgeom/) 
- [units](https://github.com/edzer/units/) 
- [eixport](https://github.com/atmoschem/eixport/) 

In order to run the demo, this package is also needed:

- [ggplot2](https://github.com/tidyverse/ggplot2)


### Installation

VEIN can be installed via CRAN or github

```r
library(remotes) 
install_github("atmoschem/vein")
library(vein)
demo(VEIN)
```

or

```r
install.packages("vein")
library(vein)
demo(VEIN)
```



## What is new? 0.7.0

- Re-design of ef_ldv_speed and ef_hdv_speed. Adds argument speed into ef_ldv_speed in order to return
data.frames insted of list of functions. For instance, speed = Speed(0:120). Also,
it euro can be with length bigger than one, and also can be data.frames. This is useful, when
you have regions with different emission standards.
- Add Active Surface (cm2/km): "AS_urban", "AS_rural", "AS_highway".
- Add number of particles (N/km): "N_urban", "N_rural", "N_highway", "N_50nm_urban", "N_50_100nm_rural", "N_100_1000nm_highway".
- [split_emis](https://atmoschem.github.io/vein/reference/split_emis.html) Split street emissions based on a grid or polygon. For instance, let's say that you need street emissiosn at
10 m. You could split your street emissions with this.
- Fix some EF metals in ef_ldv_speed.
- Fix milage equation of  fkm$KM_LDV_GNV. Fix #137.
- Adds [cold_mileage](https://atmoschem.github.io/vein/reference/cold_mileage.html) 
- Adds [emis_cold_td](https://atmoschem.github.io/vein/reference/emis_cold_td.html) 
- Adds [emis_hot_td](https://atmoschem.github.io/vein/reference/emis_hot_td.html) 
- Adds [emis_evap](https://atmoschem.github.io/vein/reference/emis_evap.html) 
- Adds pro_month in emis_hot_td and emis_cold_td, to account for monthly variation.
- Add argumnent namerows in age*functions to change row.names.
- Add argument fcorr to account for the effect of fuel composition by euro standard. This result in
a factor by euro standard.
- Adds [age](https://atmoschem.github.io/vein/reference/age.html) with survival function of Gompertz, Weibull, logistic. This function aims to replace age_ldv, age_moto and age_hdv.
- Update [ef_cetesb](https://atmoschem.github.io/vein/reference/ef_cetesb.html) for emission factors 2017. CETESB updated (and possibly recalcualted) most of PC with gasoline emission factors.
- Fix minor bugs.
- Check the [NEWS](https://github.com/atmoschem/vein/blob/master/NEWS.md)

Future steps:

- Enhance [inventory](https://atmoschem.github.io/vein/reference/inventory.html). The idea is to 
configurate a whole emissions inventory for emission factors of CETESB, COPERT.
- Adds ef_china, to include chinese emission factors.
- Adds or connect traffic from gtfs services.
- Add HBEFA EF.
- Fortran.
- Estimation of evaporative emissions with Copert Tier 3.
- Group species by chemical mechanism.


## How does it work?

VEIN consist of: "Elaboration of vehicular emissions inventories, consisting in four stages:

1. pre-processing activity data,
2. preparing emissions factors,
3. estimating the emissions and
4. post-processing of emissions in maps and databases."


This implies the use of several functions in a coordinates ans structured way,
therefore it is added the new function **inventory** which creates a structured
set of directories and scripts to run VEIN. Please, open the file 'main.R' and
run each line to understand VEIN. **Remember, if you have doubts with any function,
just type '?' with the name of the function. For intance: `?inventory`.**

Using [inventory](https://atmoschem.github.io/vein/reference/inventory.html)


```r
library(vein)
inventory(name = file.path(tempdir(), "YourCity"), 
          vehcomp = c(PC = 1, LCV = 1, HGV = 1, BUS = 1, MC = 1),
          show.dir = T, 
          show.scripts = T)
```

```
files at /tmp/RtmpEorTs8/YourCity
Directories:
 [1] "/tmp/RtmpEorTs8/YourCity"              
 [2] "/tmp/RtmpEorTs8/YourCity/ef"           
 [3] "/tmp/RtmpEorTs8/YourCity/emi"          
 [4] "/tmp/RtmpEorTs8/YourCity/emi/BUS_01"  
 [5] "/tmp/RtmpEorTs8/YourCity/emi/HGV_01"   
 [6] "/tmp/RtmpEorTs8/YourCity/emi/LCV_01"  
 [7] "/tmp/RtmpEorTs8/YourCity/emi/MC_01"    
 [8] "/tmp/RtmpEorTs8/YourCity/emi/PC_01"   
 [9] "/tmp/RtmpEorTs8/YourCity/est"          
 [10] "/tmp/RtmpEorTs8/YourCity/images"       
 [11] "/tmp/RtmpEorTs8/YourCity/network"      
 [12] "/tmp/RtmpEorTs8/YourCity/post"        
 [13] "/tmp/RtmpEorTs8/YourCity/post/df"      
 [14] "/tmp/RtmpEorTs8/YourCity/post/grids"   
 [15] "/tmp/RtmpEorTs8/YourCity/post/streets" 
 [16] "/tmp/RtmpEorTs8/YourCity/profiles"    
 [17] "/tmp/RtmpEorTs8/YourCity/veh"         
Scripts:
 [1] "est/BUS_01_input.R" 
 [2] "est/HGV_01_input.R" 
 [3] "est/LCV_01_input.R" 
 [4] "est/MC_01_input.R"  
 [5] "est/PC_01_input.R"  
 [6] "main.R"             
 [7] "post.R"            
 [8] "traffic.R" 
```

Please, read the examples in the documentation of each function and run the demo. 

### 1) Examples with traffic data:

age functions:

- [age_ldv](https://atmoschem.github.io/vein/reference/age_ldv.html)
- [age_hdv](https://atmoschem.github.io/vein/reference/age_hdv.html)
- [age_moto](https://atmoschem.github.io/vein/reference/age_moto.html)
- [my_age](https://atmoschem.github.io/vein/reference/my_age.html)

```r
data("net")
PC_E25_1400 <- age_ldv(x = net$ldv, name = "PC_E25_1400")
plot(PC_E25_1400, xlab = "age of use")
```
![](https://atmoschem.github.io/vein/reference/age_ldv-1.png)

If you want to know the vehicles per street and by age of use, just
add the net. Age functions now returns 'sf' objects if the net argument is present.

```r
PC_E25_1400net <- age_ldv(x = net$ldv, name = "PC_E25_1400", net = net)
sp::spplot(as(PC_E25_1400net, "Spatial"),
       c("PC_E25_1400_1", "PC_E25_1400_9"),
       main = "PC by age of use", scales = list(Draw = T),
       col.regions = rev(cptcity::cpt()))
```
- [temp_fact](https://atmoschem.github.io/vein/reference/temp_fact.html)
- [netspeed](https://atmoschem.github.io/vein/reference/netspeed.html)
temporal factors and netspeed

```r
data("net")
data("pc_profile")
pc_week <- temp_fact(net$ldv+net$hdv, pc_profile)
dfspeed <- netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1.5)
plot(dfspeed)

```
![](https://atmoschem.github.io/vein/reference/netspeed-1.png)
```

If you want ot check the speed at different hours by street, just add net:

```r
dfspeednet <- netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm,
                       alpha = 1.5, net = net)
sp::spplot(as(dfspeednet, "Spatial"),
       c("S1", "S9"), scales = list(Draw = T),
       col.regions = rev(cptcity::cpt()))
```

### 2) Emission Factors

- [ef_ldv_speed](https://atmoschem.github.io/vein/reference/ef_ldv_speed.html)
- [ef_hdv_speed](https://atmoschem.github.io/vein/reference/ef_hdv_speed.html)
- [ef_ldv_scaled](https://atmoschem.github.io/vein/reference/ef_ldv_scaled.html)
- [ef_hdv_scaled](https://atmoschem.github.io/vein/reference/ef_hdv_scaled.html)
- [EmissionFactors](https://atmoschem.github.io/vein/reference/EmissionFactors.html)
- [EmissionFactorsList](https://atmoschem.github.io/vein/reference/EmissionFactorsList.html)

```r
V <- 0:150
ef1 <- ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
p = "CO")
efs <- EmissionFactors(ef1(1:150))
plot(Speed(1:150), efs, xlab = "speed[km/h]", type = "b", pch = 16)
```
![](https://atmoschem.github.io/vein/reference/ef_ldv_speed-1.png)

### 3) Estimation of emissions

- [emis](https://atmoschem.github.io/vein/reference/emis.html)

```r
euro <- c(rep("V", 5), rep("IV", 5), rep("III", 5), rep("II", 5),
          rep("I", 5), rep("PRE", 15))
lef <- lapply(1:40, function(i) {
ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
          eu = euro[i], p = "CO", show.equation = FALSE) })
E_CO <- emis(veh = PC_E25_1400, lkm = net$lkm, ef = lef, speed = dfspeed,
             profile = pc_profile)
plot(E_CO, xlab = "Hours", ylab = "[g/h]")
```

### 4) Post Emissions

- [emis_post](https://atmoschem.github.io/vein/reference/emis_post.html)
- When the argument by = "veh" the emissions are aggregated by age and 
hour.
- When the argument by = "streets_wide", aggregated the emissions
by street. In this cae, if you add the argument net with the respective streets,
it returns an spatial net with the hourly emissions.

```r
E_CO_DF <- emis_post(arra = E_CO,  veh = "PC", size = "<1400", fuel = "G",
pollutant = "CO", by = "veh")
E_CO_STREETS <- emis_post(arra = E_CO, pollutant = "CO", by = "streets_wide")
```

#### Grids

- [make_grid](https://atmoschem.github.io/vein/reference/make_grid.html).


You can use this function in two ways

when spobj is "character", it is a path to wrfinput file and
then runs eixport::wrf_grid to create a grid based on a wrf_input file.

1) Create a grid using `make_grid`.The spobj is the spatial net. The size of
the grid has the size of the net. You have to specify the grid spacing.
2) Create a grid using a path to wrfinput file instead a net. The grid will have
the size of the wrf_input. You don't have to specify the grid spacing.


```r
data(net)
E_CO_STREETSnet <- emis_post(arra = E_CO, pollutant = "CO", by = "streets_wide",
                             net = net)
g <- make_grid(net, 1/102.47/2) #500m in degrees
plot(g)
E_CO_g <- emis_grid(spobj = E_CO_STREETSnet, g = g, sr= 31983)
#sp::spplot(as(E_CO_g, "Spatial"),
#       c("V1", "V9"), scales = list(Draw = T),
#       col.regions = rev(cptcity::cpt()))
```
![](https://atmoschem.github.io/vein/reference/make_grid-1.png)


**At this step, you can feed you grid with emissions from other sources!**

### Creating a WRFChem Input file using [eixport](https://atmoschem.github.io/eixport/):

1. Create a grid using [make_grid](https://atmoschem.github.io/vein/reference/make_grid.html) and a wrfinput file
2. Run [emis_grid](https://atmoschem.github.io/vein/reference/emis_grid.html) to grid your emissions.
3. Create a [GriddedEmissionsArray](https://atmoschem.github.io/vein/reference/GriddedEmissionsArray.html).
4. Create a wrfchem input file [eixport::wrf_create](https://atmoschem.github.io/eixport/reference/wrf_create.html.
5. Put the [GriddedEmissionsArray](https://atmoschem.github.io/vein/reference/GriddedEmissionsArray.html)
into the wrf chem input file using [eixport::wrf_put](https://atmoschem.github.io/eixport/reference/wrf_put.html).

```r
eixport::wrf_create(wrfinput_dir = "PathToWrfInput", wrfchemi_dir = "OutputPathWrfChemInput")
gwrf <- eixport::wrf_grd("PathToWrfInput")
E_CO_gwrf <- emis_grid(spobj = E_CO_STREETSnet, g = gwrf)
gr <- GriddedEmissionsArray(E_CO_gwrf, rows = 19, cols = 23, times = 168, T)
eixport::wrf_put(file = "Path/To/WRFChemInputFile, name = "E_CO", POL = gr)
```

### Creating a WRFChem Input file using AS4WRF

1. Create a grid using [make_grid](https://atmoschem.github.io/vein/reference/make_grid.html) and your net.
2. Run [emis_grid](https://atmoschem.github.io/vein/reference/emis_grid.html) to grid your emissions.
3. Run [emis_wrf](https://atmoschem.github.io/vein/reference/emis_wrf.html) to create a 
data.frame the specifications for AS4WRF.ncl.
4. Export the output of [emis_wrf](https://atmoschem.github.io/vein/reference/emis_wrf.html) to a text file
without header. Recall that AS4WRF requires all the lumped species.
5. Contact the developer of AS4WRF Angel Vara alvv1986@gmail.com to get a copy and run AS4WRF.ncl.

Thanks and enjoy VEIN!

## Citation

If you use VEIN, please, cite it ([BIBTEX](https://www.geosci-model-dev.net/11/2209/2018/gmd-11-2209-2018.bib), [ENDNOTE](https://www.geosci-model-dev.net/11/2209/2018/gmd-11-2209-2018.ris)):

Ibarra-Espinosa, S., Ynoue, R., O'Sullivan, S., Pebesma, E., Andrade, M. D. F., and
  Osses, M.: VEIN v0.2.2: an R package for bottom-up vehicular emissions inventories,
  Geosci. Model Dev., 11, 2209-2229, https://doi.org/10.5194/gmd-11-2209-2018, 2018.

```
@article{gmd-11-2209-2018,
author = {Ibarra-Espinosa, S. and Ynoue, R. and O'Sullivan, S. and Pebesma, E. and Andrade, M. D. F. and Osses, M.},
title = {VEIN v0.2.2: an R package for bottom--up vehicular emissions inventories},
journal = {Geoscientific Model Development},
volume = {11},
year = {2018},
number = {6},
pages = {2209--2229},
url = {https://www.geosci-model-dev.net/11/2209/2018/},
doi = {10.5194/gmd-11-2209-2018}
}
```


## Communications, doubts etc

- Drop me an email sergio.ibarra@usp.br
- 嗨！ 你可以写信给我！zergioibarra@hotmail.com
- You can ask on [Eart Science Stackoverflow](https://earthscience.stackexchange.com/),.
- Check the project on [ResearchGate](https://www.researchgate.net/project/VEIN-An-R-package-for-vehicular-emissions-inventories).
- Check the group on GoogleGroups [Group](https://groups.google.com/d/forum/veinmodel).

##  Issues

If you encounter any issues while using VEIN, please submit your issues to: https://github.com/atmoschem/vein/issues/

If you have any suggestions just let me know to sergio.ibarra@usp.br.


### Contributing

Please, read [this](https://github.com/atmoschem/vein/blob/master/CONTRIBUTING.md) guide.
Contributions of all sorts are welcome, issues and pull requests are the preferred ways of sharing them.
When contributing pull requests, please follow the [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml).
This project is released with a [Contributor Code of Conduct](https://github.com/atmoschem/vein/blob/master/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### Note for non-english and anaconda users

Sometimes you need to install R and all dependencies and a way for doing that is using anaconda. Well, as my system is in portuguese, after installing R using anaconda it changed the decimal character to ','. In order to change it back to english meaning decimal separator as '.', I added this variable  into the .bashrc

```
nano ~/.bashrc
export Lang=C
```

More details on [StackOverflow](https://stackoverflow.com/questions/13575180/how-to-change-language-settings-in-r) 


### More

You can learn more about VEIN reading the documentation in [PDF](https://cran.r-project.org/web/packages/vein/vein.pdf), [online](https://atmoschem.github.io/vein/), reading the book [online](https://ibarraespinosa.github.io/VEINBOOK/), or buy it in  [Kindle](https://www.amazon.com/VEINBOOK-Estimating-vehicular-emissions-package-ebook-dp-B07L7XRFKC/dp/B07L7XRFKC/ref=mt_kindle?_encoding=UTF8&me=&qid=) or  [Paperback](https://www.amazon.com/gp/product/1791571158?pf_rd_p=1581d9f4-062f-453c-b69e-0f3e00ba2652&pf_rd_r=EMDPZM3G7BWCHAD9F4QP)

![](https://i.imgur.com/RcfNmDm.jpg)


