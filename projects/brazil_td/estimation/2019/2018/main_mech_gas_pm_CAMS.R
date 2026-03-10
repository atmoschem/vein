
options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(stars)
library(eixport)
sessionInfo()

language <- "portuguese" # english spanish

df_gas <- data.table(gas = c("CO",
                             "CO2",
                             "NO2",
                             "NO",
                             "SO2",
                             "CH4",
                             "NH3",
                             "N2O",
			     "PM"),

                     mw = set_units(c(28.01,
                                      44.01,
                                      46.0055,
                                      30.01,
                                      64.066,
                                      16.04,
                                      17.031,
                                      44.013,
				      1), "g/mol"))


source("scripts/mech_gas_pm_CAMS.R")


