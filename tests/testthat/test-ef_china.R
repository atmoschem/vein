context("ef_china")

df_st <- rev(c(as.character(as.roman(5:1)), "PRE"))
v <- "PV"
f = "G"
standard = df_st
p = "CO"
ta <- celsius(15)
altitude = 1000
details = F
correction_only = F
speed <- 30
baseyear_det = 2014
sulphur = 50
load_factor = 0.5

# Checking BASE EF
# Mini G
test_that("ef_china works", {# CO
  expect_equal(ef_china(t = "Mini", f = "G", standard = df_st, p = "CO"),
               EmissionFactors(c(25.72, 6.71, 2.52, 1.18, 0.68, 0.46)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(t = "Mini", f = "G", standard = df_st, p = "HC"),
               EmissionFactors(c(2.685, 0.663, 0.314, 0.191, 0.075, 0.056)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(t = "Mini", f = "G", standard = df_st, p = "NOx"),
               EmissionFactors(c(1.971, 0.409, 0.324, 0.1, 0.032, 0.017)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(t = "Mini", f = "G", standard = df_st, p = "PM2.5"),
               EmissionFactors(c(0.028, 0.026, 0.011, 0.007, 0.003, 0.003)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(t = "Mini", f = "G", standard = df_st, p = "PM10"),
               EmissionFactors(c(0.031, 0.029, 0.012, 0.008, 0.003, 0.003)))})
# Small G
test_that("ef_china works", {# CO
  expect_equal(ef_china(t = "Small", f = "G", standard = df_st, p = "CO"),
               EmissionFactors(c(39.13, 21.43, 15.37, 4.33, 1.98, 1.98)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(t = "Small", f = "G", standard = df_st, p = "HC"),
               EmissionFactors(c(3.695, 2.567, 1.443, 0.373, 0.107, 0.107)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(t = "Small", f = "G", standard = df_st, p = "NOx"),
               EmissionFactors(c(2.938, 1.781, 1.461, 0.474, 0.196, 0.147)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(t = "Small", f = "G", standard = df_st, p = "PM2.5"),
               EmissionFactors(c(0.099, 0.06, 0.018, 0.011, 0.006, 0.006)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(t = "Small", f = "G", standard = df_st, p = "PM10"),
               EmissionFactors(c(0.11, 0.067, 0.020, 0.012, 0.007, 0.007)))})
# Medium G
test_that("ef_china works", {# CO
  expect_equal(ef_china(t = "Medium", f = "G", standard = df_st, p = "CO"),
               EmissionFactors(c(39.13, 21.43, 15.37, 4.33, 1.98, 1.98)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(t = "Medium", f = "G", standard = df_st, p = "HC"),
               EmissionFactors(c(3.695, 2.567, 1.443, 0.373, 0.107, 0.107)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(t = "Medium", f = "G", standard = df_st, p = "NOx"),
               EmissionFactors(c(2.938, 1.781, 1.461, 0.474, 0.196, 0.147)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(t = "Medium", f = "G", standard = df_st, p = "PM2.5"),
               EmissionFactors(c(0.099, 0.06, 0.018, 0.011, 0.006, 0.006)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(t = "Medium", f = "G", standard = df_st, p = "PM10"),
               EmissionFactors(c(0.11, 0.067, 0.020, 0.012, 0.007, 0.007)))})
# Large G
test_that("ef_china works", {# CO
  expect_equal(ef_china(t = "Large", f = "G", standard = df_st, p = "CO"),
               EmissionFactors(c(100.74, 62.09, 16.64, 8.25, 3.77, 3.77)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(t = "Large", f = "G", standard = df_st, p = "HC"),
               EmissionFactors(c(5.144, 5.255, 1.98, 0.869, 0.418, 0.418)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(t = "Large", f = "G", standard = df_st, p = "NOx"),
               EmissionFactors(c(5.156, 2.645, 2.562, 1.52, 0.775, 0.582)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(t = "Large", f = "G", standard = df_st, p = "PM2.5"),
               EmissionFactors(c(0.293, 0.159, 0.072, 0.044, 0.044, 0.044)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(t = "Large", f = "G", standard = df_st, p = "PM10"),
               EmissionFactors(c(0.326, 0.177, 0.08, 0.049, 0.049, 0.049)))})
# Light Trucks G
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "Trucks", t = "Mini", f = "G", standard = df_st, p = "CO"),
               EmissionFactors(c(47.83, 26.16, 21.54, 5.61, 2.37, 2.37)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "Trucks", t = "Mini", f = "G", standard = df_st, p = "HC"),
               EmissionFactors(c(4.987, 3.324, 2.21, 0.61, 0.169, 0.169)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "Trucks", t = "Mini", f = "G", standard = df_st, p = "NOx"),
               EmissionFactors(c(3.31, 2.006, 1.656, 0.534, 0.229, 0.172)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "Trucks", t = "Mini", f = "G", standard = df_st, p = "PM2.5"),
               EmissionFactors(c(0.099, 0.06, 0.018, 0.011, 0.006, 0.006)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "Trucks", t = "Mini", f = "G", standard = df_st, p = "PM10"),
               EmissionFactors(c(0.11, 0.067, 0.02, 0.012, 0.007, 0.007)))})
# Light Trucks G
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "Trucks", t = "Medium", f = "G", standard = df_st, p = "CO"),
               EmissionFactors(c(123.13, 75.79, 23.32, 10.71, 4.5, 4.5)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "Trucks", t = "Medium", f = "G", standard = df_st, p = "HC"),
               EmissionFactors(c(6.884, 6.777, 3.023, 1.371, 0.573, 0.573)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "Trucks", t = "Medium", f = "G", standard = df_st, p = "NOx"),
               EmissionFactors(c(5.807, 2.979, 2.905, 1.713, 0.907, 0.68)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "Trucks", t = "Medium", f = "G", standard = df_st, p = "PM2.5"),
               EmissionFactors(c(0.293, 0.159, 0.072, 0.044, 0.044, 0.044)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "Trucks", t = "Medium", f = "G", standard = df_st, p = "PM10"),
               EmissionFactors(c(0.326, 0.177, 0.08, 0.049, 0.049, 0.049)))})
# Heavy Trucks G
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "Trucks", t = "Heavy", f = "G", standard = df_st, p = "CO"),
               EmissionFactors(c(123.13, 75.79, 23.32, 10.71, 4.5, 4.5)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "Trucks", t = "Heavy", f = "G", standard = df_st, p = "HC"),
               EmissionFactors(c(6.749, 6.759, 3.006, 1.354, 0.555, 0.555)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "Trucks", t = "Heavy", f = "G", standard = df_st, p = "NOx"),
               EmissionFactors(c(5.807, 2.979, 2.905, 1.713, 0.907, 0.68)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "Trucks", t = "Heavy", f = "G", standard = df_st, p = "PM2.5"),
               EmissionFactors(c(0.293, 0.159, 0.072, 0.044, 0.044, 0.044)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "Trucks", t = "Heavy", f = "G", standard = df_st, p = "PM10"),
               EmissionFactors(c(0.326, 0.177, 0.08, 0.049, 0.049, 0.049)))})
# Taxi G
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "PV", t = "Taxi", f = "G", standard = df_st, p = "CO"),
               EmissionFactors(c(36.96, 16.12, 7.27, 3.03, 2.45, 2.45)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "PV", t = "Taxi", f = "G", standard = df_st, p = "HC"),
               EmissionFactors(c(3.84, 1.368, 0.963, 0.454, 0.277, 0.257)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "PV", t = "Taxi", f = "G", standard = df_st, p = "NOx"),
               EmissionFactors(c(2.159, 0.767, 0.81, 0.204, 0.135, 0.095)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "PV", t = "Taxi", f = "G", standard = df_st, p = "PM2.5"),
               EmissionFactors(c(0.028, 0.026, 0.011, 0.007, 0.003, 0.003)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "PV", t = "Taxi", f = "G", standard = df_st, p = "PM10"),
               EmissionFactors(c(0.031, 0.029, 0.012, 0.008, 0.003, 0.003)))})
# BUS G
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "PV", t = "Bus", f = "G", standard = df_st, p = "CO"),
               EmissionFactors(c(100.74, 62.09, 16.64, 8.25, 3.77, 3.77)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "PV", t = "Bus", f = "G", standard = df_st, p = "HC"),
               EmissionFactors(c(5.144, 5.255, 1.980, 0.869, 0.418, 0.418)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "PV", t = "Bus", f = "G", standard = df_st, p = "NOx"),
               EmissionFactors(c(5.156, 2.645, 2.562, 1.52, 0.775, 0.582)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "PV", t = "Bus", f = "G", standard = df_st, p = "PM2.5"),
               EmissionFactors(c(0.293, 0.159, 0.072, 0.044, 0.044, 0.044)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "PV", t = "Bus", f = "G", standard = df_st, p = "PM10"),
               EmissionFactors(c(0.326, 0.177, 0.08, 0.049, 0.049, 0.049)))})
# MC G
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "PV", t = "Motorcycles", f = "G", standard = df_st[1:4], p = "CO"),
               EmissionFactors(c(14.2, 8.96, 2.58, 1.11)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "PV", t = "Motorcycles", f = "G", standard = df_st[1:4], p = "HC"),
               EmissionFactors(c(2.01, 0.99, 0.53, 0.21)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "PV", t = "Motorcycles", f = "G", standard = df_st[1:4], p = "NOx"),
               EmissionFactors(c(0.13, 0.14, 0.15, 0.1)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "PV", t = "Motorcycles", f = "G", standard = df_st[1:4], p = "PM2.5"),
               EmissionFactors(c(0.03, 0.018, 0.008, 0.003)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "PV", t = "Motorcycles", f = "G", standard = df_st[1:4], p = "PM10"),
               EmissionFactors(c(0.033, 0.02, 0.009, 0.003)))})
# Moped G
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "PV", t = "Moped", f = "G", standard = df_st[1:4], p = "CO"),
               EmissionFactors(c(9.6, 4.18, 1.97, 0.82)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "PV", t = "Moped", f = "G", standard = df_st[1:4], p = "HC"),
               EmissionFactors(c(5.4, 2.15, 1.65, 0.88)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "PV", t = "Moped", f = "G", standard = df_st[1:4], p = "NOx"),
               EmissionFactors(c(0.12, 0.11, 0.11, 0.07)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "PV", t = "Moped", f = "G", standard = df_st[1:4], p = "PM2.5"),
               EmissionFactors(c(0.03, 0.018, 0.008, 0.003)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "PV", t = "Moped", f = "G", standard = df_st[1:4], p = "PM10"),
               EmissionFactors(c(0.033, 0.02, 0.009, 0.003)))})
# Mini D
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "PV", t = "Mini", f = "D", standard = df_st, p = "CO", sulphur = 350),
               EmissionFactors(c(1.34, 0.36, 0.45, 0.14, 0.13, 0.13)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "PV", t = "Mini", f = "D", standard = df_st, p = "HC", sulphur = 350),
               EmissionFactors(c(0.785, 0.071, 0.046, 0.024, 0.016, 0.016)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "PV", t = "Mini", f = "D", standard = df_st, p = "NOx", sulphur = 350),
               EmissionFactors(c(1.324, 0.976, 0.976, 0.841, 0.679, 0.679)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "PV", t = "Mini", f = "D", standard = df_st, p = "PM2.5", sulphur = 350),
               EmissionFactors(c(0.179, 0.063, 0.052, 0.032, 0.031, 0.031)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "PV", t = "Mini", f = "D", standard = df_st, p = "PM10", sulphur = 350),
               EmissionFactors(c(0.199, 0.07, 0.058, 0.036, 0.034, 0.034)))})
# Small D
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "PV", t = "Small", f = "D", standard = df_st, p = "CO", sulphur = 350),
               EmissionFactors(c(3.91, 3.44, 2.82, 2.12, 1.84, 1.84)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "PV", t = "Small", f = "D", standard = df_st, p = "HC", sulphur = 350),
               EmissionFactors(c(1.493, 1.425, 0.425, 0.364, 0.364, 0.364)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "PV", t = "Small", f = "D", standard = df_st, p = "NOx", sulphur = 350),
               EmissionFactors(c(5.74, 4.787, 5.693, 3.347, 2.678, 2.276)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "PV", t = "Small", f = "D", standard = df_st, p = "PM2.5", sulphur = 350),
               EmissionFactors(c(1.603, 0.464, 0.157, 0.148, 0.106, 0.053)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "PV", t = "Small", f = "D", standard = df_st, p = "PM10", sulphur = 350),
               EmissionFactors(c(1.781, 0.516, 0.174, 0.164, 0.118, 0.059)))})
# Mediumbus D
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "PV", t = "Mediumbus", f = "D", standard = df_st, p = "CO", sulphur = 350),
               EmissionFactors(c(3.91, 3.44, 2.82, 2.12, 1.84, 1.84)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "PV", t = "Mediumbus", f = "D", standard = df_st, p = "HC", sulphur = 350),
               EmissionFactors(c(1.493, 1.425, 0.425, 0.364, 0.364, 0.364)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "PV", t = "Mediumbus", f = "D", standard = df_st, p = "NOx", sulphur = 350),
               EmissionFactors(c(5.74, 4.787, 5.693, 3.347, 2.678, 2.276)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "PV", t = "Mediumbus", f = "D", standard = df_st, p = "PM2.5", sulphur = 350),
               EmissionFactors(c(1.603, 0.464, 0.157, 0.148, 0.106, 0.053)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "PV", t = "Mediumbus", f = "D", standard = df_st, p = "PM10", sulphur = 350),
               EmissionFactors(c(1.781, 0.516, 0.174, 0.164, 0.118, 0.059)))})
# Largebus D
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "PV", t = "Largebus", f = "D", standard = df_st, p = "CO", sulphur = 350),
               EmissionFactors(c(10.53, 9.86, 8.68, 6.74, 3.25, 1.62)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "PV", t = "Largebus", f = "D", standard = df_st, p = "HC", sulphur = 350),
               EmissionFactors(c(2.668, 0.576, 0.351, 0.283, 0.107, 0.054)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "PV", t = "Largebus", f = "D", standard = df_st, p = "NOx", sulphur = 350),
               EmissionFactors(c(12.421, 11.156, 9.892, 9.892, 9.892, 8.64)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "PV", t = "Largebus", f = "D", standard = df_st, p = "PM2.5", sulphur = 350),
               EmissionFactors(c(1.286, 0.983, 0.882, 0.395, 0.252, 0.126)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "PV", t = "Largebus", f = "D", standard = df_st, p = "PM10", sulphur = 350),
               EmissionFactors(c(1.429, 1.092, 0.98, 0.439, 0.28, 0.14)))})
# Trucks D
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "Trucks", t = "Light", f = "D", standard = df_st, p = "CO", sulphur = 350),
               EmissionFactors(c(3.28, 4.19, 3.22, 1.88, 1.48, 1.48)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "Trucks", t = "Light", f = "D", standard = df_st, p = "HC", sulphur = 350),
               EmissionFactors(c(2.097, 2.04, 1.305, 0.368, 0.186, 0.186)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "Trucks", t = "Light", f = "D", standard = df_st, p = "NOx", sulphur = 350),
               EmissionFactors(c(6.758, 5.578, 5.578, 3.765, 2.636, 2.24)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "Trucks", t = "Light", f = "D", standard = df_st, p = "PM2.5", sulphur = 350),
               EmissionFactors(c(0.435, 0.269, 0.261, 0.13, 0.058, 0.012)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "Trucks", t = "Light", f = "D", standard = df_st, p = "PM10", sulphur = 350),
               EmissionFactors(c(0.483, 0.299, 0.29, 0.144, 0.064, 0.013)))})
# Trucks D
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "Trucks", t = "Medium", f = "D", standard = df_st, p = "CO", sulphur = 350),
               EmissionFactors(c(12.05, 4.24, 4.63, 2.09, 1.65, 1.65)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "Trucks", t = "Medium", f = "D", standard = df_st, p = "HC", sulphur = 350),
               EmissionFactors(c(3.56, 1.612, 0.421, 0.203, 0.103, 0.103)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "Trucks", t = "Medium", f = "D", standard = df_st, p = "NOx", sulphur = 350),
               EmissionFactors(c(10.782, 7.479, 6.221, 6.221, 4.354, 3.701)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "Trucks", t = "Medium", f = "D", standard = df_st, p = "PM2.5", sulphur = 350),
               EmissionFactors(c(1.322, 0.905, 0.273, 0.171, 0.099, 0.02)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "Trucks", t = "Medium", f = "D", standard = df_st, p = "PM10", sulphur = 350),
               EmissionFactors(c(1.45, 1.006, 0.303, 0.19, 0.11, 0.022)))})
# Trucks D
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "Trucks", t = "Heavy", f = "D", standard = df_st, p = "CO", sulphur = 350),
               EmissionFactors(c(13.6, 5.79, 3.08, 2.79, 2.2, 2.2)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "Trucks", t = "Heavy", f = "D", standard = df_st, p = "HC", sulphur = 350),
               EmissionFactors(c(4.083, 0.897, 0.52, 0.255, 0.129, 0.129)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "Trucks", t = "Heavy", f = "D", standard = df_st, p = "NOx", sulphur = 350),
               EmissionFactors(c(13.823, 9.589, 7.934, 7.934, 5.554, 4.721)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "Trucks", t = "Heavy", f = "D", standard = df_st, p = "PM2.5", sulphur = 350),
               EmissionFactors(c(1.322, 0.623, 0.502, 0.243, 0.138, 0.027)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "Trucks", t = "Heavy", f = "D", standard = df_st, p = "PM10", sulphur = 350),
               EmissionFactors(c(1.45, 0.692, 0.558, 0.27, 0.153, 0.03)))})
# BUS D
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "PV", t = "Bus", f = "D", standard = df_st, p = "CO", sulphur = 350),
               EmissionFactors(c(10.53, 9.86, 8.68, 6.74, 3.25, 1.62)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "PV", t = "Bus", f = "D", standard = df_st, p = "HC", sulphur = 350),
               EmissionFactors(c(2.668, 0.576, 0.351, 0.283, 0.107, 0.054)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "PV", t = "Bus", f = "D", standard = df_st, p = "NOx", sulphur = 350),
               EmissionFactors(c(12.421, 11.156, 9.892, 9.892, 9.892, 8.64)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "PV", t = "Bus", f = "D", standard = df_st, p = "PM2.5", sulphur = 350),
               EmissionFactors(c(1.286, 0.983, 0.882, 0.395, 0.252, 0.126)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "PV", t = "Bus", f = "D", standard = df_st, p = "PM10", sulphur = 350),
               EmissionFactors(c(1.429, 1.092, 0.98, 0.439, 0.28, 0.14)))})
# 3-Wheel D
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "PV", t = "3-Wheel", f = "D", standard = df_st[1:3], p = "CO", sulphur = 350),
               EmissionFactors(c(1.98, 0.95, 0.75)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "PV", t = "3-Wheel", f = "D", standard = df_st[1:3], p = "HC", sulphur = 350),
               EmissionFactors(c(0.4, 0.24, 0.16)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "PV", t = "3-Wheel", f = "D", standard = df_st[1:3], p = "NOx", sulphur = 350),
               EmissionFactors(c(1.08, 1.07, 0.87)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "PV", t = "3-Wheel", f = "D", standard = df_st[1:3], p = "PM2.5", sulphur = 350),
               EmissionFactors(c(0.074, 0.064, 0.049)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "PV", t = "3-Wheel", f = "D", standard = df_st[1:3], p = "PM10", sulphur = 350),
               EmissionFactors(c(0.078, 0.068, 0.053)))})
# Low Speed D
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "Trucks", t = "Low Speed", f = "D", standard = df_st[1:3], p = "CO", sulphur = 350),
               EmissionFactors(c(4.52, 2.62, 2.06)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "Trucks", t = "Low Speed", f = "D", standard = df_st[1:3], p = "HC", sulphur = 350),
               EmissionFactors(c(1.32, 1.16, 0.75)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "Trucks", t = "Low Speed", f = "D", standard = df_st[1:3], p = "NOx", sulphur = 350),
               EmissionFactors(c(3.95, 3.88, 3.14)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "Trucks", t = "Low Speed", f = "D", standard = df_st[1:3], p = "PM2.5", sulphur = 350),
               EmissionFactors(c(0.175, 0.157, 0.122)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "Trucks", t = "Low Speed", f = "D", standard = df_st[1:3], p = "PM10", sulphur = 350),
               EmissionFactors(c(0.185, 0.166, 0.131)))})
# Mini D
test_that("ef_china works", {# CO
  expect_equal(ef_china(v = "Trucks", t = "Mini", f = "D", standard = df_st, p = "CO", sulphur = 350),
               EmissionFactors(c(3.91, 3.44, 2.82, 2.12, 1.84, 1.84)))})
test_that("ef_china works", {# HC
  expect_equal(ef_china(v = "Trucks", t = "Mini", f = "D", standard = df_st, p = "HC", sulphur = 350),
               EmissionFactors(c(1.493, 1.425, 0.425, 0.364, 0.364, 0.364)))})
test_that("ef_china works", {# NOx
  expect_equal(ef_china(v = "Trucks", t = "Mini", f = "D", standard = df_st, p = "NOx", sulphur = 350),
               EmissionFactors(c(5.74, 4.787, 5.693, 3.347, 2.678, 2.276)))})
test_that("ef_china works", {# PM2.5
  expect_equal(ef_china(v = "Trucks", t = "Mini", f = "D", standard = df_st, p = "PM2.5", sulphur = 350),
               EmissionFactors(c(1.603, 0.464, 0.157, 0.148, 0.106, 0.053)))})
test_that("ef_china works", {# PM10
  expect_equal(ef_china(v = "Trucks", t = "Mini", f = "D", standard = df_st, p = "PM10", sulphur = 350),
               EmissionFactors(c(1.781, 0.516, 0.174, 0.164, 0.118, 0.059)))})
# Fuel ALL not included because correction factors depends on fuel
df <- as.data.frame(rbind(df_st, df_st))
names(df) <- letters[1:6]
test_that("ef_china stops", {# CO
  expect_error(ef_china(t = "Mini", f = "G", standard = df, p = "CO"),
               "l.?")
  })

test_that("ef_china stops", {# CO
  expect_error(ef_china(t = "Mini", f = "G", standard = df, p = "CO",
                        ta = 1:2),
               "t.?")
})

test_that("ef_china stops", {# CO
  expect_error(ef_china(t = "Mini", f = "G", standard = df, p = "CO",
                        ta = celsius(1:2)),
               "t.?")
})

test_that("ef_china stops", {# CO
  expect_error(ef_china(t = "Mini", f = "G", standard = df, p = "CO",
                        ta = celsius(1:2),
                        altitude = c(1000, 1000)),
               "s.?")
})

test_that("ef_china stops", {# CO
  expect_error(ef_china(t = "Mini", f = "G", standard = df, p = "CO",
                        ta = celsius(1:2),
                        altitude = c(1000, 1000),
                        speed = Speed(55:56)),
               "s.?")
})


test_that("ef_china works", {# CO
  expect_equal(round(ef_china(t = "Mini", f = "G", standard = df, p = "CO",
                        ta = celsius(1:2),
                        altitude = c(1000, 1000),
                        speed = Speed(55:56),
                        sulphur = 400:401)$V1[1]),
               EmissionFactors(20))
})
