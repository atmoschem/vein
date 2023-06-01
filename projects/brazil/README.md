# how to run

This repository give you access to estimate vehicular emissions between 1991 and 2020. All the required information is there

you need the following R packages

- vein
- sf
- ggplot2
- readxl
- crul
- geobr

Following the methodology of Carter (2015), it covers the chemical mechanisms:

1. CB05
2. S99 (SAPRAC99)
3. RADM2
4. MOZT1 (MOZART)
5. RACM

How to proceed:

```
git clone https://gitlab.com/ibarraespinosa/brazil
mv brazil brazil_all
Rscript pre_main.R
Rscript main.R
```


citations:

Ibarra-Espinosa, S., Ynoue, R., O'Sullivan, S., Pebesma, E., Andrade, M. D. F., and Osses, M.: VEIN v0.2.2: an R package for bottom–up vehicular emissions inventories, Geosci. Model Dev., 11, 2209–2229, https://doi.org/10.5194/gmd-11-2209-2018, 2018.

Carter, William PL. "Development of a database for chemical mechanism assignments for volatile organic emissions." Journal of the Air & Waste Management Association 65.10 (2015): 1171-1184.


Nogueira, Thiago, et al. "Evolution of Vehicle Emission Factors in a Megacity Affected by Extensive Biofuel Use: Results of Tunnel Measurements in São Paulo, Brazil." Environmental Science & Technology (2021).



