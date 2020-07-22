
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- date: "19 de Octubre de 2016" -->

# VEIN <img src="man/figures/logo.png" align="right" alt="" width="220" />

  - build: [![Travis-CI Build
    Status](https://travis-ci.org/atmoschem/vein.svg?branch=master)](https://travis-ci.org/atmoschem/vein)
    [![AppVeyor Build
    Status](https://ci.appveyor.com/api/projects/status/github/ibarraespinosa/vein?branch=master&svg=true)](https://ci.appveyor.com/project/ibarraespinosa/vein)
    [![Coverage
    Status](https://img.shields.io/codecov/c/github/atmoschem/vein/master.svg)](https://codecov.io/github/atmoschem/vein?branch=master)
  - cran:
    [![](http://cranlogs.r-pkg.org/badges/vein)](http://cran.rstudio.com/web/packages/vein/index.html)
    [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/vein)](http://cran.r-project.org/web/packages/vein)
    [![CRAN
    Downloads](http://cranlogs.r-pkg.org/badges/grand-total/vein?color=orange)](http://cran.r-project.org/package=vein)
    [![Package
    Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
    ![CRAN/METACRAN](https://img.shields.io/cran/l/vein?style=plastic)
  - doi:
    [![DOI](https://zenodo.org/badge/88201850.svg)](https://zenodo.org/badge/latestdoi/88201850)
  - github: [![Github
    Stars](https://img.shields.io/github/stars/atmoschem/vein.svg?style=social&label=Github)](https://github.com/atmoschem/vein)
    ![GitHub code size in
    bytes](https://img.shields.io/github/languages/code-size/atmoschem/vein)
    ![GitHub
    issues](https://img.shields.io/github/issues/atmoschem/vein)
    <!-- ![Liberapay giving](https://img.shields.io/liberapay/gives/sergio.ibarra) -->
    ![GitHub commit
    activity](https://img.shields.io/github/commit-activity/y/ibarraespinosa/vein)
    <!-- ![](https://github.com/atmoschem/vein/raw/master/all_comp.gif) -->

# The GitHub repository is a mirror from <https://gitlab.com/ibarraespinosa/vein>

  - The most updated version is here
    <https://gitlab.com/ibarraespinosa/vein>
  - The GitHub <https://github.com/atmoschem/veinis> updated as soon as
    possible, usually between 03:00:00 and 10:00:00 **UTC**

## What is VEIN?

**V**ehicular **E**missions **IN**ventories (VEIN). An R package to
estimate vehicular emissions. It currently covers the following
pollutants in speed functions:

### European emission factors for all available vehicle categories exhaust:

  - **Criteria (g/km)**: “CO”, “NOx”, “HC”, “PM”, “CH4”, “NMHC”, “CO2”,
    “SO2”, “Pb”, “FC” (Fuel Consumption),“NO”, “NO2”.
  - **PAH and POP**: “indeno(1,2,3-cd)pyrene”, “benzo(k)fluoranthene”,
    “benzo(b)fluoranthene”, “benzo(ghi)perylene”, “fluoranthene”,
    “benzo(a)pyrene”, “pyrene”, “perylene”, “anthanthrene”,
    “benzo(b)fluorene”, “benzo(e)pyrene”, “triphenylene”,
    “benzo(j)fluoranthene”, “dibenzo(a,j)anthacene”,
    “dibenzo(a,l)pyrene”, “3,6-dimethyl-phenanthrene”,
    “benzo(a)anthracene”, “acenaphthylene”, “acenapthene”, “chrysene”,
    “phenanthrene”, “napthalene”, “anthracene”, “coronene”,
    “dibenzo(ah)anthracene”.
  - **Dioxins and Furans (g/km)**: “PCDD”, “PCDF”, “PCB”.
  - **Metals (g/km)**: “As”, “Cd”, “Cr”, “Cu”, “Hg”, “Ni”, “Pb”, “Se”,
    “Zn”.
  - **NMHC (g/km)**:
  - *ALKANES*: “ethane”, “propane”, “butane”, “isobutane”, “pentane”,
    “isopentane”, “hexane”, “heptane”, “octane”, “2-methylhexane”,
    “nonane”, “2-methylheptane”, “3-methylhexane”, “decane”,
    “3-methylheptane”, “alkanes\_C10\_C12”, “alkanes\_C13”.
  - *CYCLOALKANES*: “cycloalcanes”.
  - *ALKENES*: “ethylene”, “propylene”, “propadiene”, “1-butene”,
    “isobutene”, “2-butene”, “1,3-butadiene”, “1-pentene”,
    “2-pentene”, “1-hexene”, “dimethylhexene”.
  - *ALKYNES*:“1-butyne”, “propyne”, “acetylene”.
  - *ALDEHYDES*: “formaldehyde”, “acetaldehyde”, “acrolein”,
    “benzaldehyde”, “crotonaldehyde”, “methacrolein”, “butyraldehyde”,
    “isobutanaldehyde”, “propionaldehyde”, “hexanal”,
    “i\_valeraldehyde”, “valeraldehyde”, “o\_tolualdehyde”,
    “m\_tolualdehyde”, “p\_tolualdehyde”.
  - *KETONES*: “acetone”, “methylethlketone”.
  - *AROMATICS*: “toluene”, “ethylbenzene”, “m-xylene”, “p-xylene”,
    “o-xylene”, “1,2,3-trimethylbenzene”, “1,2,4-trimethylbenzene”,
    “1,3,5-trimethylbenzene”, “styrene”, “benzene”, “C9”, “C10”,
    “C13”.
  - **Active Surface (cm2/km)**
  - “AS\_urban”, “AS\_rural”, “AS\_highway”.
  - **Total Number of particles (N/km)**
  - “N\_urban”, “N\_rural”, “N\_highway”, “N\_50nm\_urban”,
    “N\_50\_100nm\_rural”, “N\_100\_1000nm\_highway”.

### European emission factors speciation for evapoative emissions:

  - **Criteria (g/km)**: “NMHC”.
  - **NMHC (g/km)**:
  - *ALKANES*: “ethane”, “propane”, “n-butane”, “i-pentane”,
    “n-pentane”, “2-methylheptane”, “3-methylheptane”, “n-hexane”,
    “n-heptane”
  - *ALKENES*: “ethene”, “propene”, “1-butene”, “trans-2-butene”,
    “isobutene”, “cis-2-butene”, “1,3-butadiene”, “trans-2-pentene”,
    “cis-2-pentene”, “isoprene”
  - *ALKYNES*:“propyne”, “acetylene”.
  - *AROMATICS*: “benzene”, “toluene”, “ethylbenzene”, “m-xylene”,
    “o-xylene”, “1,2,3-trimethylbenzene” and “1,3,5-trimethylbenzene”,

### Brazilian emission factors for all available vehicle categories:

  - “COd”, “HCd”, “NMHCd”, “CH4”, “NOxd”, “CO2” “PM”, “N2O”, “KML”,
    “FC”, “NO2d”, “NOd”, “gCO2/KWH”, “RCHOd”, “CO”, “HC”, “NMHC”,
    “NOx”, “NO2” ,“NO”, “RCHO” and scaled factors based on tunnel
    measurements see
    [ef\_cetesb](https://atmoschem.github.io/vein/reference/ef_cetesb.html).

### Brazilian speciation based on IAG/USP (Fátima’s group) studies:

  - “e\_eth”, “e\_hc3”, “e\_hc5”, “e\_hc8”, “e\_ol2”, “e\_olt”,
    “e\_oli”, “e\_iso”, “e\_tol”, “e\_xyl”, “e\_c2h5oh”, “e\_ald”,
    “e\_hcho”, “e\_ch3oh”, “e\_ket”, “E\_SO4i”, “E\_SO4j”, “E\_NO3i”,
    “E\_NO3j”, “E\_MP2.5i”, “E\_MP2.5j”, “E\_ORGi”, “E\_ORGj”,
    “E\_ECi”, “E\_ECj” with option to scaled based on tunnel
    measurements.

### Base emission factors from International Emission Model (IVE) for all available vehicle categories:

  - “VOC\_gkm”, “CO\_gkm”, “NOx\_gkm”, “PM\_gkm”, “Pb\_gkm”, “SO2\_gkm”,
    “NH3\_gkm”, “ONE\_3\_butadiene\_gkm”, “formaldehyde\_gkm”,
    “acetaldehyde\_gkm”, “benzene\_gkm”, “EVAP\_gkm”, “CO2\_gkm”,
    “N20\_gkm”, “CH4\_gkm”, “VOC\_gstart”, “CO\_gstart”,
    “NOx\_gstart”, “PM\_gstart”, “Pb\_gstart”, “SO2\_gstart”,
    “NH3\_gstart”, “ONE\_3butadiene\_gstart”,
    “formaldehyde\_gstart”,“acetaldehyde\_gstart”,
    “benzene\_gstart”, “EVAP\_gstart”, “CO2\_gstart”, “N20\_gstart”,
    “CH4\_gstart”

### Emission factors from Chinese emission guidelines 你好中国朋友

  - “CO”, “NOx”, “HC”, “PM10”, “PM2.5”.
  - They depend on humidity, temperature, altitude and other parameters.

### System requirements

vein imports functions from spatial packages listed below. In order to
install these packages, firstly the user must install the requirements
mentioned [here](https://www.github.com/r-spatial/sf).

### Installation

VEIN can be installed via CRAN or github

``` r
install.packages("vein")
```

``` r
library(remotes) 
install_gitlab("ibarraespinosa/vein")
```

In order to run the demo, this package is also needed:

  - [ggplot2](https://github.com/tidyverse/ggplot2)

<!-- end list -->

``` r
library(vein) 
demo(VEIN)
```

### What is new?

  - [colplot](https://atmoschem.github.io/vein/reference/colplot.html).
  - emisla (COVID-19) in get\_project. Emissions for March 2020 in São
    Paulo.

### Future steps

  - Add OpenMP in Fortran.
  - Improve plots.
  - Add projects for European and Chinese emission factors
  - Add HBEFA and EMFAC EF.
  - Estimation of evaporative emissions with Copert Tier 3.
  - Group species by chemical mechanism.
  - See issues [GitHub](https://github.com/atmoschem/vein/issues) and
    [GitLab](https://gitlab.com/ibarraespinosa/vein/-/issues)

## How does it work?

VEIN consist of: "Elaboration of vehicular emissions inventories,
consisting in four stages:

1.  pre-processing activity data,
2.  preparing emissions factors,
3.  estimating the emissions and
4.  post-processing of emissions in maps and databases."

This implies the use of several functions in a coordinates ans
structured way, therefore it is added the new function **inventory**
which creates a structured set of directories and scripts to run VEIN.
Please, open the file ‘main.R’ and run each line to understand VEIN.
**Remember, if you have doubts with any function, just type ‘?’ with the
name of the function. For intance: `?inventory`.**

Using
[inventory](https://atmoschem.github.io/vein/reference/inventory.html)
**Inventory will experience some changes, please be patient and check
<https://github.com/atmoschem/vein/projects/3>** Please, read the
examples in the documentation of each function and run the demo.

**Brazilian users, just use `get_project` and run a project read**

  - <https://ibarraespinosa.github.io/cursovein>

### 1\) Examples with traffic data:

1.  If you know the distribution of the vehicles by age of use , use:
    [my\_age](https://atmoschem.github.io/vein/reference/my_age.html)
2.  If you know the sales of vehicles or better the registry of new
    vehicles, use
    [age](https://atmoschem.github.io/vein/reference/age.html) to apply
    a survival function.
3.  If you know the theoretical shape of the circulating fleet and you
    can use
    [age\_ldv](https://atmoschem.github.io/vein/reference/age_ldv.html),
    [age\_hdv](https://atmoschem.github.io/vein/reference/age_hdv.html)
    or
    [age\_moto](https://atmoschem.github.io/vein/reference/age_moto.html).
    For instance, you dont know the sales or registry of vehicles, but
    somehow you know the shape of this curve.
4.  You can use/merge/transform/adapt any of these functions.

<!-- end list -->

``` r
data("net")
PC_E25_1400 <- age_ldv(x = net$ldv, name = "PC_E25_1400")
plot(PC_E25_1400, xlab = "age of use")
```

![](https://atmoschem.github.io/vein/reference/age_ldv-1.png)

If you want to know the vehicles per street and by age of use, just add
the net. Age functions now returns ‘sf’ objects if the net argument is
present.

``` r
PC_E25_1400net <- age_ldv(x = net$ldv, name = "PC_E25_1400", net = net)
plot(PC_E25_1400net)
```

![](https://atmoschem.github.io/vein/reference/age_ldv-2.png)

  - [temp\_fact](https://atmoschem.github.io/vein/reference/temp_fact.html)
  - [netspeed](https://atmoschem.github.io/vein/reference/netspeed.html)

temporal factors and netspeed

``` r
data("net")
data("pc_profile")
pc_week <- temp_fact(net$ldv+net$hdv, pc_profile)
dfspeed <- netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1.5)
plot(dfspeed)
```

![](https://atmoschem.github.io/vein/reference/netspeed-1.png)

If you want ot check the speed at different hours by street, just add
net:

``` r
dfspeednet <- netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm,
                       alpha = 1.5, net = net)
sp::spplot(as(dfspeednet, "Spatial"),
       c("S1", "S9"), scales = list(Draw = T),
       col.regions = rev(cptcity::cpt()))
```

![](https://i.imgur.com/qJUdMea.png) \#\#\# 2) Emission Factors

  - [ef\_ldv\_speed](https://atmoschem.github.io/vein/reference/ef_ldv_speed.html)
  - [ef\_hdv\_speed](https://atmoschem.github.io/vein/reference/ef_hdv_speed.html)
  - [ef\_ldv\_scaled](https://atmoschem.github.io/vein/reference/ef_ldv_scaled.html)
  - [ef\_hdv\_scaled](https://atmoschem.github.io/vein/reference/ef_hdv_scaled.html)
  - [EmissionFactors](https://atmoschem.github.io/vein/reference/EmissionFactors.html)
  - [EmissionFactorsList](https://atmoschem.github.io/vein/reference/EmissionFactorsList.html)

<!-- end list -->

``` r
V <- 0:150
ef1 <- ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
p = "CO")
efs <- EmissionFactors(ef1(1:150))
plot(Speed(1:150), efs, xlab = "speed[km/h]", type = "b", pch = 16)
```

![](https://atmoschem.github.io/vein/reference/ef_ldv_speed-1.png)

### 3\) Estimation of emissions

  - [emis](https://atmoschem.github.io/vein/reference/emis.html)

<!-- end list -->

``` r
euro <- c(rep("V", 5), rep("IV", 5), rep("III", 5), rep("II", 5),
          rep("I", 5), rep("PRE", 15))
lef <- lapply(1:40, function(i) {
ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
          eu = euro[i], p = "CO", show.equation = FALSE) })
E_CO <- emis(veh = PC_E25_1400, lkm = net$lkm, ef = lef, speed = dfspeed,
             profile = pc_profile)
```

### 4\) Post Emissions

  - [emis\_post](https://atmoschem.github.io/vein/reference/emis_post.html)
  - When the argument by = “veh” the emissions are aggregated by age and
    hour.
  - When the argument by = “streets\_wide”, aggregated the emissions by
    street. In this cae, if you add the argument net with the respective
    streets, it returns an spatial net with the hourly emissions.

<!-- end list -->

``` r
E_CO_DF <- emis_post(arra = E_CO,  veh = "PC", size = "<1400", fuel = "G",
pollutant = "CO", by = "veh")
E_CO_STREETS <- emis_post(arra = E_CO, pollutant = "CO", by = "streets")
```

#### Grids

  - [make\_grid](https://atmoschem.github.io/vein/reference/make_grid.html).

<!-- end list -->

1)  Create a grid using `make_grid`.The spobj is the spatial net. The
    size of the grid has the size of the net. You have to specify the
    grid spacing.
2)  Create a grid using a path to wrfinput file instead a net. The grid
    will have the size of the wrf\_input. You don’t have to specify the
    grid spacing.

<!-- end list -->

``` r
data(net)
E_CO_STREETSnet <- emis_post(arra = E_CO, pollutant = "CO", by = "streets_wide",
                             net = net)
g <- make_grid(net, 1/102.47)
E_CO_g <- emis_grid(spobj = E_CO_STREETSnet, g = g, sr= 31983)
na <- paste0("V", 1:168)
for(i in 1:168) E_CO_g[[na[i]]] <- E_CO_g[[na[i]]] * units::set_units(1, "1/h")
plot(E_CO_g["V9"], axes = T, pal = cptcity::cpt(colorRampPalette = T, rev = T))
```

![](https://i.imgur.com/Ydsvt8x.png)

### Creating a WRFChem Input file using [eixport](https://atmoschem.github.io/eixport/):

1.  Create a grid using
    [make\_grid](https://atmoschem.github.io/vein/reference/make_grid.html)
    and a wrfinput file
2.  Run
    [emis\_grid](https://atmoschem.github.io/vein/reference/emis_grid.html)
    to grid your emissions.
3.  Create a
    [GriddedEmissionsArray](https://atmoschem.github.io/vein/reference/GriddedEmissionsArray.html).
4.  Create a wrfchem input file
    \[eixport::wrf\_create\](<https://atmoschem.github.io/eixport/reference/wrf_create.html>.
5.  Put the
    [GriddedEmissionsArray](https://atmoschem.github.io/vein/reference/GriddedEmissionsArray.html)
    into the wrf chem input file using
    [eixport::wrf\_put](https://atmoschem.github.io/eixport/reference/wrf_put.html).

<!-- end list -->

``` r
library(eixport)
dir.create(file.path(tempdir(), "EMISS"))
wrf_create(wrfinput_dir         = system.file("extdata", package = "eixport"),
           wrfchemi_dir         = file.path(tempdir(), "EMISS"),
           domains              = 2,
           frames_per_auxinput5 = 1, #hours
           auxinput5_interval_m = 60,
           verbose              = TRUE)
path_to_wrfi <- paste0(system.file("extdata", package = "eixport"), "/wrfinput_d02")
path_to_wrfc <- list.files(file.path(tempdir(), "EMISS"), full.names = TRUE)[1]
gwrf <- eixport::wrf_grid(path_to_wrfi)
E_CO_gwrf <- emis_grid(spobj = E_CO_STREETSnet, g = gwrf)
gr <- GriddedEmissionsArray(E_CO_gwrf, rows = 51, cols = 63, times = 1)
eixport::wrf_put(file = path_to_wrfc, name = "E_CO", POL = gr)
```

### Creating a WRFChem Input file using AS4WRF

1.  Create a grid using
    [make\_grid](https://atmoschem.github.io/vein/reference/make_grid.html)
    and your net.
2.  Run
    [emis\_grid](https://atmoschem.github.io/vein/reference/emis_grid.html)
    to grid your emissions.
3.  Run
    [eixport::to\_as4wrf](https://atmoschem.github.io/eixport/reference/to_as4wrf.html)
    to create a data.frame the specifications for AS4WRF.ncl.
4.  Export data.frame to a text.file. Recall that AS4WRF requires all
    the lumped species.
5.  Contact the developer of AS4WRF Angel Vara <alvv1986@gmail.com> to
    get a copy and run AS4WRF.ncl.

Thanks and enjoy VEIN\!

## Citation

If you use VEIN, please, cite it
([BIBTEX](https://www.geosci-model-dev.net/11/2209/2018/gmd-11-2209-2018.bib),
[ENDNOTE](https://www.geosci-model-dev.net/11/2209/2018/gmd-11-2209-2018.ris)):

Ibarra-Espinosa, S., Ynoue, R., O’Sullivan, S., Pebesma, E., Andrade, M.
D. F., and Osses, M.: VEIN v0.2.2: an R package for bottom-up vehicular
emissions inventories, Geosci. Model Dev., 11, 2209-2229,
<https://doi.org/10.5194/gmd-11-2209-2018>, 2018.

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

## Communications, doubts etc

  - Drop me an email <sergio.ibarra@usp.br> or
    <zergioibarra@hotmail.com> (你好中国朋友 - Hello Chinese friends\!)
  - Check the group on GoogleGroups
    [Group](https://groups.google.com/d/forum/veinmodel).
  - Check the project on
    [ResearchGate](https://www.researchgate.net/project/VEIN-An-R-package-for-vehicular-emissions-inventories).

## Issues

If you encounter any issues while using VEIN, please submit your issues
to: <https://github.com/atmoschem/vein/issues/> If you have any
suggestions just let me know to <sergio.ibarra@usp.br>.

### Contributing

Please, read
[this](https://github.com/atmoschem/vein/blob/master/CONTRIBUTING.md)
guide. Contributions of all sorts are welcome, issues and pull requests
are the preferred ways of sharing them. When contributing pull requests,
please follow the [Google’s R Style
Guide](https://google.github.io/styleguide/Rguide.xml). This project is
released with a [Contributor Code of
Conduct](https://github.com/atmoschem/vein/blob/master/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

### Note for non-english and anaconda users

Sometimes you need to install R and all dependencies and a way for doing
that is using anaconda. Well, as my system is in portuguese, after
installing R using anaconda it changed the decimal character to ‘,’. In
order to change it back to english meaning decimal separator as ‘.’, I
added this variable into the .bashrc

    nano ~/.bashrc
    export Lang=C

More details on
[StackOverflow](https://stackoverflow.com/questions/13575180/how-to-change-language-settings-in-r)

You can learn more about VEIN reading the documentation in
[PDF](https://cran.r-project.org/web/packages/vein/vein.pdf),
[online](https://atmoschem.github.io/vein/), reading the book
[online](https://ibarraespinosa.github.io/VEINBOOK/), or buy it in
[Kindle](https://www.amazon.com/VEINBOOK-Estimating-vehicular-emissions-package-ebook-dp-B07L7XRFKC/dp/B07L7XRFKC/ref=mt_kindle?_encoding=UTF8&me=&qid=)
or
[Paperback](https://www.amazon.com/gp/product/1791571158?pf_rd_p=1581d9f4-062f-453c-b69e-0f3e00ba2652&pf_rd_r=EMDPZM3G7BWCHAD9F4QP)

![](https://i.imgur.com/RcfNmDm.jpg)
