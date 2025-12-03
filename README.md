
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- date: "19 de Octubre de 2016" -->

# VEIN <img src="man/figures/logo.png" align="right" alt="" width="220" />

- build: [![AppVeyor Build
  Status](https://ci.appveyor.com/api/projects/status/github/ibarraespinosa/vein?branch=master&svg=true)](https://ci.appveyor.com/project/ibarraespinosa/vein)
  [![Coverage
  Status](https://img.shields.io/codecov/c/github/atmoschem/vein/master.svg)](https://codecov.io/github/atmoschem/vein?branch=master)
- cran:
  [![](http://cranlogs.r-pkg.org/badges/vein)](http://cran.rstudio.com/web/packages/vein/index.html)
  [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/vein)](http://cran.r-project.org/web/packages/vein)
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
  ![GitHub issues](https://img.shields.io/github/issues/atmoschem/vein)
  <!-- ![Liberapay giving](https://img.shields.io/liberapay/gives/sergio.ibarra) -->
  ![GitHub commit
  activity](https://img.shields.io/github/commit-activity/y/ibarraespinosa/vein)
  [![R build
  status](https://github.com/atmoschem/vein/workflows/R-CMD-check/badge.svg)](https://github.com/atmoschem/vein/actions)

<!-- ![](https://github.com/atmoschem/vein/raw/master/all_comp.gif) -->

# **V**ehicular **E**missions **IN**ventories (VEIN)

<figure>
<img
src="https://user-images.githubusercontent.com/27447280/234115392-b1c891ff-474d-40a3-b9a7-e816ded9bc70.gif"
alt="vein" />
<figcaption aria-hidden="true">vein</figcaption>
</figure>

### TODO

- Include speed functions with Fortran
- Add EF from HBEFA?
- See issues [GitHub](https://github.com/atmoschem/vein/issues)
- Second edition of my book

### System requirements

vein imports functions from spatial packages listed below. In order to
install these packages, firstly the user must install the requirements
mentioned [here](https://www.github.com/r-spatial/sf).

### Installation

#### CRAN

VEIN can be installed via CRAN or github

``` r
install.packages("vein")
```

#### GitHub

``` r
remotes::install_github("atmoschem/vein")
```

or if you have a **32 bits** machine

``` r
install_github("atmoschem/vein",
INSTALL_opts = "--no-multiarch")
```

------------------------------------------------------------------------

## Run with a project

Use the function
[get_project](https://atmoschem.github.io/vein/reference/get_project.html)
and read the documentation, there you can see more projects as well.

``` r
library(vein)
```

``` r
awesome_city <- tempdir()
awesome_city
#> [1] "/tmp/Rtmp9irGAj"
get_project(directory = awesome_city,
case = "brazil_bu_chem")
#> Your directory is in /tmp/Rtmp9irGAj
```

``` r
system(paste0("tree ", awesome_city))
```

You have to open the file `main.Rproj` with Rstudio and then open and
run `main.R`

To run `main.R` you will need these extra packages:

- ggplot2
- readxl
- eixport (If you plan to generate WRF Chem emissions file)

If you do not have them already, you can install:

``` r
install.packages(c("ggplot2", "readxl", "eixport"))
```

Check the projects
[here](https://atmoschem.github.io/vein/reference/get_project.html)

<figure>
<img
src="https://ibarraespinosa.github.io/2025CU/figuras/vein_projects.jpg"
alt="vein proejcts" />
<figcaption aria-hidden="true">vein proejcts</figcaption>
</figure>

## Too complicated? Watch a YouTube

<iframe width="560" height="315" src="https://www.youtube.com/embed/tHSWIjg26vg" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen>

</iframe>

[English](https://www.youtube.com/embed/tHSWIjg26vg)

<iframe width="560" height="315" src="https://www.youtube.com/embed/6-07Y0Eimng" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen>

</iframe>

[Portuguese](https://www.youtube.com/watch?v=6-07Y0Eimng)

## Check some of my presentations

[presentation and some papers](https://github.com/topics/ibarraslides)

Thanks and enjoy VEIN!

## Citation

If you use VEIN, please, cite it
([BIBTEX](https://gmd.copernicus.org/articles/11/2209/2018/gmd-11-2209-2018.bib),
[ENDNOTE](https://gmd.copernicus.org/articles/11/2209/2018/gmd-11-2209-2018.ris)):

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
    url = {https://gmd.copernicus.org/articles/11/2209/2018/},
    doi = {10.5194/gmd-11-2209-2018}
    }

[Google
Scholar](https://scholar.google.com/scholar?q=VEIN+v0.2.2%3A+an+R+package+for+bottom%E2%80%93up+vehicular+emissions+inventories)

<div id="metrics-365-60926-crossref"
class="metrics-tile metrics-tile-crossref low-opacity">

<a href="javascript:void(0);" title="Toggle crossref metric details">
<img class="metrics-tile-image" alt="" src="https://www.geoscientific-model-development.net/metrics_logo_crossref.png">
</a>

<div class="metrics-tile-footer">

<a href="javascript:void(0);" title="Toggle crossref metric details">50</a>

</div>

</div>

## Special thanks to all the contributors

[![Contributors](https://contrib.rocks/image?repo=atmoschem/vein)](https://github.com/atmoschem/vein/graphs/contributors)

## Communications, doubts etc

- Earth-Sciences on Stackoverflow, tag
  [vein-r-package](https://earthscience.stackexchange.com/questions/tagged/vein-r-package)
- Drop me an email <sergio.ibarraespinosa@colorado.edu> or
  <zergioibarra@hotmail.com> (你好中国朋友 - Hello Chinese friends!)
- Check the group on GoogleGroups
  [Group](https://groups.google.com/d/forum/veinmodel).

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
Guide](https://google.github.io/styleguide/Rguide.xml).

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

<!-- You can learn more about VEIN reading the documentation in [PDF](https://cran.r-project.org/web/packages/vein/vein.pdf), [online](https://atmoschem.github.io/vein/), reading the book [online](https://ibarraespinosa.github.io/VEINBOOK/), or buy it in  [Kindle](https://www.amazon.com/VEINBOOK-Estimating-vehicular-emissions-package-ebook-dp-B07L7XRFKC/dp/B07L7XRFKC/ref=mt_kindle?_encoding=UTF8&me=&qid=) or  [Paperback](https://www.amazon.com/gp/product/1791571158?pf_rd_p=1581d9f4-062f-453c-b69e-0f3e00ba2652&pf_rd_r=EMDPZM3G7BWCHAD9F4QP) -->

<!-- ![](https://i.imgur.com/RcfNmDm.jpg) -->
