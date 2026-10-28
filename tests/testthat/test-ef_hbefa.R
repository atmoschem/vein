context("ef_hbefa")
# Reference values: Eclipse SUMO (commit 25bdc3b9f7b1ee05e9dda40f3b8f6266736f005b)
# - instantaneous points: SUMO's original C++ code (PollutantsInterface.cpp,
#   HelpersHBEFA{,3,4}) compiled in a harness, default EnergyParams
#   (1500 kg, 1.8 * 1.5 * pi / 4 m2); values in g/s (SUMO mg/s / 1000), EC in Wh/s
# - NEDC totals: tests/complex/emissions/nefz_hbefa{2,3,4}/output.complex of
#   SUMO (emissionsDrivingCycle --kmh --compute-a), mg / Wh with 6 digits,
#   length 11041.9 m

skip_if(is.null(vein:::sysdata$hbefa), "sysdata$hbefa not available")

test_that("hbefa_classes lists all SUMO classes", {
  expect_equal(nrow(hbefa_classes()), 833)
  expect_equal(nrow(hbefa_classes("HBEFA3")), 45)
  expect_equal(nrow(hbefa_classes("HBEFA2")), 84)
  cl <- hbefa_classes(c("HBEFA2", "HBEFA3", "HBEFA4"))
  expect_equal(nrow(cl), 962)
  expect_true(all(c("class", "category", "fuel", "euro", "size", "technology",
                    "hbefa_id", "sumo_fuel") %in% names(cl)))
  expect_true("HBEFA4/PC_petrol_Euro-4" %in% cl$class)
  expect_equal(cl$sumo_fuel[cl$class == "HBEFA4/PC_BEV"], "Electricity")
  expect_error(hbefa_classes("HBEFA5"))
})

test_that("emis_hbefa reproduces SUMO instantaneous values", {
  v1 <- 13.888888888888889; a1 <- 0.5
  ref <- data.frame(
    class = c("HBEFA2/P_7_7", "HBEFA2/P_7_7", "HBEFA2/P_A0_7_3", "HBEFA2/HDV_12_4",
              "HBEFA3/PC_G_EU4", "HBEFA3/PC_G_EU4", "HBEFA3/HDV_D_EU6",
              "HBEFA3/PC_D_EU6", "HBEFA4/PC_petrol_Euro-4",
              "HBEFA4/PC_petrol_Euro-4", "HBEFA4/PC_diesel_Euro-6ab",
              "HBEFA4/TT_AT_gt34-40t_Euro-VI_A-C",
              "HBEFA4/UBus_Std_gt15-18t_Euro-VI_A-C", "HBEFA4/PC_BEV"),
    pol = c("CO2", "NOx", "NOx", "CO2", "CO2", "NOx", "CO2", "NOx", "CO2", "PM",
            "NOx", "CO2", "NOx", "EC"),
    value = c(3.07006185304, 0.00563518140487, 0.00922339972222, 28.5721898786,
              4.14709876543, 0.00155713220165, 27.7648919753, 0.00479890260631,
              3.65766222565, 0.000433488083, 0.00926219598765, 39.0503888889,
              0.00766672582305, 3.93704549897),
    stringsAsFactors = FALSE)
  for (i in seq_len(nrow(ref))) {
    expect_equal(as.numeric(emis_hbefa(ref$class[i], ref$pol[i], v = v1, a = a1)),
                 ref$value[i], tolerance = 1e-10, info = paste(ref$class[i], ref$pol[i]))
  }
  # with slope (2 deg), v = 20 m/s, a = 0.3 m/s2
  expect_equal(as.numeric(emis_hbefa("HBEFA2/P_7_7", "CO2", 20, 0.3, slope = 2)),
               3.86680995435, tolerance = 1e-10)
  expect_equal(as.numeric(emis_hbefa("HBEFA3/PC_G_EU4", "CO2", 20, 0.3, slope = 2)),
               6.5179706941, tolerance = 1e-10)
  expect_equal(as.numeric(emis_hbefa("HBEFA4/PC_diesel_Euro-6ab", "CO2", 20, 0.3,
                                     slope = 2)),
               6.50384588185, tolerance = 1e-10)
  expect_equal(as.numeric(emis_hbefa("HBEFA4/PC_BEV", "EC", 20, 0.3, slope = 2)),
               6.58954287748, tolerance = 1e-10)
  # vectorised over class and speed
  x <- emis_hbefa(c("HBEFA3/PC_G_EU4", "HBEFA4/PC_petrol_Euro-4"), "CO2",
                  v = v1, a = a1)
  expect_equal(as.numeric(x), c(4.14709876543, 3.65766222565), tolerance = 1e-10)
})

test_that("idle emissions", {
  expect_equal(as.numeric(emis_hbefa("HBEFA2/P_7_7", "CO2", 0, 0)), 0.6016927778,
               tolerance = 1e-9)
  expect_equal(as.numeric(emis_hbefa("HBEFA3/PC_G_EU4", "CO2", 0, 0)), 2.624722222,
               tolerance = 1e-9)
  expect_equal(as.numeric(emis_hbefa("HBEFA3/HDV_D_EU6", "FC", 0, 0)), 2.055555556,
               tolerance = 1e-9)
  expect_equal(as.numeric(emis_hbefa("HBEFA4/PC_petrol_Euro-4", "CO2", 0, 0)), 1.521)
  expect_equal(as.numeric(emis_hbefa("HBEFA4/PC_BEV", "EC", 0, 0)), 0.8055)
  # no coasting rule when standing (v <= 0.5 m/s)
  expect_gt(as.numeric(emis_hbefa("HBEFA3/PC_G_EU4", "CO2", 0, -2)), 0)
})

test_that("coasting cut and electricity", {
  # decelerating at -1 m/s2 at 50 km/h: below the coasting deceleration
  for (p in c("CO2", "CO", "HC", "FC", "NOx", "PM")) {
    expect_equal(as.numeric(emis_hbefa("HBEFA3/PC_G_EU4", p, 13.888888888888889, -1)), 0)
    expect_equal(as.numeric(emis_hbefa("HBEFA4/PC_petrol_Euro-4", p, 13.888888888888889, -1)), 0)
  }
  # coasting deceleration at 50 km/h is -0.288 m/s2 (1500 kg, 2.12 m2)
  expect_gt(as.numeric(emis_hbefa("HBEFA4/PC_petrol_Euro-4", "CO2", 50 / 3.6, -0.28)), 0)
  expect_equal(as.numeric(emis_hbefa("HBEFA4/PC_petrol_Euro-4", "CO2", 50 / 3.6, -0.29)), 0)
  expect_gt(as.numeric(emis_hbefa("HBEFA4/PC_petrol_Euro-4", "CO2", 50 / 3.6, -0.29,
                                  coasting = FALSE)), 0)
  # a heavier vehicle (bus-like defaults) has a less negative coasting deceleration
  expect_gt(as.numeric(emis_hbefa("HBEFA4/PC_petrol_Euro-4", "CO2", 50 / 3.6, -0.25)), 0)
  expect_equal(as.numeric(emis_hbefa("HBEFA4/PC_petrol_Euro-4", "CO2", 50 / 3.6, -0.25,
                                     mass = 12000, frontal_area = 6.675884388878311)), 0)
  # battery electric: no coasting cut, negative (recuperation) allowed
  expect_equal(as.numeric(emis_hbefa("HBEFA4/PC_BEV", "EC", 13.888888888888889, -1)),
               -2.8824424639917696, tolerance = 1e-12)
  expect_equal(as.numeric(emis_hbefa("HBEFA4/PC_BEV", "CO2", 15, 0)), 0)
  # no electricity in HBEFA2/3 nor in combustion classes
  expect_equal(as.numeric(emis_hbefa("HBEFA3/PC_G_EU4", "EC", 10, 0)), 0)
  expect_equal(as.numeric(emis_hbefa("HBEFA4/PC_petrol_Euro-4", "EC", 10, 0)), 0)
  # never negative for pollutants
  expect_true(all(as.numeric(emis_hbefa("HBEFA4/PC_diesel_Euro-6ab", "NOx",
                                        v = 0:40, a = -0.25, slope = -5)) >= 0))
})

test_that("units are converted", {
  x1 <- emis_hbefa("HBEFA4/PC_petrol_Euro-4", "NOx", v = 50 / 3.6, a = 0.5)
  x2 <- emis_hbefa("HBEFA4/PC_petrol_Euro-4", "NOx",
                   v = units::set_units(50, "km/h"),
                   a = units::set_units(1.8, "km/h/s"))
  expect_equal(as.numeric(x1), as.numeric(x2), tolerance = 1e-12)
  expect_true(inherits(x1, "units"))
  expect_equal(units::deparse_unit(x1), "g s-1")
  ec <- emis_hbefa("HBEFA4/PC_BEV", "EC", 10, 0)
  expect_equal(as.numeric(units::set_units(ec, "kW")), as.numeric(ec) * 3.6,
               tolerance = 1e-12)
  s1 <- emis_hbefa("HBEFA3/PC_G_EU4", "CO2", 20, 0.3, slope = 2)
  s2 <- emis_hbefa("HBEFA3/PC_G_EU4", "CO2", 20, 0.3,
                   slope = units::set_units(2 * pi / 180, "rad"))
  expect_equal(as.numeric(s1), as.numeric(s2), tolerance = 1e-12)
  # volumetric fuel as in SUMO --emissions.volumetric-fuel (742 g/L petrol)
  f1 <- emis_hbefa("HBEFA3/PC_G_EU4", "FC", 20, 0.3)
  f2 <- emis_hbefa("HBEFA3/PC_G_EU4", "FC", 20, 0.3, volumetric_fuel = TRUE)
  expect_equal(as.numeric(f2), as.numeric(f1) / 742, tolerance = 1e-12)
  expect_equal(units::deparse_unit(f2), "L s-1")
})

test_that("class names and errors", {
  a <- emis_hbefa("HBEFA4/PC_petrol_Euro-4", "CO2", 10, 0)
  expect_equal(emis_hbefa("PC_petrol_Euro-4", "CO2", 10, 0, model = "HBEFA4"), a)
  expect_equal(emis_hbefa("HBEFA4/default", "CO2", 10, 0), a)
  expect_equal(emis_hbefa("HBEFA3/pc_g_eu4", "CO2", 10, 0),
               emis_hbefa("HBEFA3/PC_G_EU4", "CO2", 10, 0))
  expect_equal(as.numeric(emis_hbefa("HBEFA4/zero", "CO2", 10, 0)), 0)
  expect_equal(emis_hbefa("HBEFA4/PC_petrol_Euro-4", "fuel", 10, 0),
               emis_hbefa("HBEFA4/PC_petrol_Euro-4", "FC", 10, 0))
  expect_error(emis_hbefa("HBEFA4/PC_petrol_Euro-9", "CO2", 10, 0), "Unknown emission class")
  expect_error(emis_hbefa("HBEFA4/pc_petrol_euro-4", "CO2", 10, 0), "Unknown emission class")
  expect_error(emis_hbefa("HBEFA4/PC_petrol_Euro-4", "SO2", 10, 0), "Unknown pollutant")
  expect_error(emis_hbefa("COPERT/PC", "CO2", 10, 0), "Unknown model")
})

test_that("ef_hbefa gives cruise emission factors", {
  ef <- ef_hbefa("HBEFA4/PC_petrol_Euro-4", "CO2")
  expect_true(is.function(ef))
  r <- as.numeric(emis_hbefa("HBEFA4/PC_petrol_Euro-4", "CO2", 50 / 3.6, 0))
  expect_equal(ef(50), r / (50 / 3600), tolerance = 1e-12)
  expect_equal(ef(50), 148.2309, tolerance = 1e-6)
  expect_equal(ef(0), ef(5))
  expect_equal(ef(2), ef(5))
  x <- ef_hbefa("HBEFA4/PC_petrol_Euro-4", "CO2", speed = Speed(c(20, 50)))
  expect_true(inherits(x, "units"))
  expect_equal(as.numeric(x), ef(c(20, 50)))
  y <- ef_hbefa(c("HBEFA4/PC_petrol_Euro-4", "HBEFA4/PC_diesel_Euro-6ab"), "NOx",
                speed = Speed(c(30, 60, 90)))
  expect_equal(dim(y), c(3, 2))
  expect_equal(as.numeric(y[[2]]), c(0.46582, 0.36766, 0.48894), tolerance = 1e-4)
  expect_equal(ef_hbefa("HBEFA4/PC_BEV", "EC")(50), 119.7996, tolerance = 1e-6)
})

test_that("ef_hbefa_cycle reproduces SUMO emissionsDrivingCycle on the NEDC", {
  nedc_v <- c(0, 1.88, 5.62, 9.37, 13.12, 15, 13.76, 11.26, 8.33, 4.98, 1.66, 0, 1.5,
    4.49, 7.48, 10.46, 13.48, 15, 16.7, 20.08, 23.47, 26.85, 30.27, 32,
    30.64, 27.9, 25.16, 22.43, 19.69, 16.96, 14.22, 11.43, 8.33, 4.98, 1.66,
    0, 1.5, 4.49, 7.48, 10.46, 13.48, 15, 16.12, 18.35, 20.59, 22.82, 25.05,
    27.28, 29.51, 31.74, 33.93, 35, 35.94, 37.81, 39.69, 41.56, 43.43, 45.3,
    47.17, 49.06, 50, 49.07, 47.2, 45.33, 43.45, 41.58, 39.71, 37.84, 35.95,
    35, 33.44, 30.31, 27.17, 24.04, 20.91, 17.78, 13.11, 8.33, 4.98, 1.66, 0,
    1.88, 5.62, 9.37, 13.12, 15, 13.76, 11.26, 8.33, 4.98, 1.66, 0, 1.5,
    4.49, 7.48, 10.46, 13.48, 15, 16.7, 20.08, 23.47, 26.85, 30.27, 32,
    30.64, 27.9, 25.16, 22.43, 19.69, 16.96, 14.22, 11.43, 8.33, 4.98, 1.66,
    0, 1.5, 4.49, 7.48, 10.46, 13.48, 15, 16.12, 18.35, 20.59, 22.82, 25.05,
    27.28, 29.51, 31.74, 33.93, 35, 35.94, 37.81, 39.69, 41.56, 43.43, 45.3,
    47.17, 49.06, 50, 49.07, 47.2, 45.33, 43.45, 41.58, 39.71, 37.84, 35.95,
    35, 33.44, 30.31, 27.17, 24.04, 20.91, 17.78, 13.11, 8.33, 4.98, 1.66, 0,
    1.88, 5.62, 9.37, 13.12, 15, 13.76, 11.26, 8.33, 4.98, 1.66, 0, 1.5,
    4.49, 7.48, 10.46, 13.48, 15, 16.7, 20.08, 23.47, 26.85, 30.27, 32,
    30.64, 27.9, 25.16, 22.43, 19.69, 16.96, 14.22, 11.43, 8.33, 4.98, 1.66,
    0, 1.5, 4.49, 7.48, 10.46, 13.48, 15, 16.12, 18.35, 20.59, 22.82, 25.05,
    27.28, 29.51, 31.74, 33.93, 35, 35.94, 37.81, 39.69, 41.56, 43.43, 45.3,
    47.17, 49.06, 50, 49.07, 47.2, 45.33, 43.45, 41.58, 39.71, 37.84, 35.95,
    35, 33.44, 30.31, 27.17, 24.04, 20.91, 17.78, 13.11, 8.33, 4.98, 1.66, 0,
    1.88, 5.62, 9.37, 13.12, 15, 13.76, 11.26, 8.33, 4.98, 1.66, 0, 1.5,
    4.49, 7.48, 10.46, 13.48, 15, 16.7, 20.08, 23.47, 26.85, 30.27, 32,
    30.64, 27.9, 25.16, 22.43, 19.69, 16.96, 14.22, 11.43, 8.33, 4.98, 1.66,
    0, 1.5, 4.49, 7.48, 10.46, 13.48, 15, 16.12, 18.35, 20.59, 22.82, 25.05,
    27.28, 29.51, 31.74, 33.93, 35, 35.94, 37.81, 39.69, 41.56, 43.43, 45.3,
    47.17, 49.06, 50, 49.07, 47.2, 45.33, 43.45, 41.58, 39.71, 37.84, 35.95,
    35, 33.44, 30.31, 27.17, 24.04, 20.91, 17.78, 13.11, 8.33, 4.98, 1.66, 0,
    1.5, 4.49, 7.48, 10.46, 13.48, 15, 16.12, 18.35, 20.59, 22.82, 25.05,
    27.28, 29.51, 31.74, 33.93, 35, 35.94, 37.81, 39.69, 41.56, 43.43, 45.3,
    47.17, 49.06, 50, 50.78, 52.33, 53.88, 55.42, 56.97, 58.52, 60.07, 61.61,
    63.16, 64.71, 66.26, 67.81, 69.29, 70, 68.76, 66.28, 63.79, 61.31, 58.83,
    56.34, 53.86, 51.31, 50, 50.78, 52.33, 53.88, 55.42, 56.97, 58.52, 60.07,
    61.61, 63.16, 64.71, 66.26, 67.81, 69.29, 70, 70.44, 71.3, 72.17, 73.03,
    73.89, 74.76, 75.62, 76.48, 77.35, 78.21, 79.08, 79.94, 80.81, 81.67,
    82.53, 83.4, 84.26, 85.13, 85.99, 86.85, 87.72, 88.58, 89.44, 90.31,
    91.17, 92.04, 92.9, 93.77, 94.63, 95.49, 96.36, 97.22, 98.09, 98.95,
    99.69, 100, 100.51, 101.52, 102.52, 103.53, 104.54, 105.55, 106.56,
    107.57, 108.57, 109.58, 110.59, 111.6, 112.6, 113.61, 114.62, 115.63,
    116.64, 117.64, 118.65, 119.58, 120, 118.76, 116.28, 113.79, 111.31,
    108.83, 106.34, 103.86, 101.37, 98.89, 96.41, 93.92, 91.44, 88.96, 86.47,
    83.99, 81.37, 78.13, 74.39, 70.65, 66.9, 63.16, 59.41, 55.67, 51.9, 47.5,
    42.5, 37.5, 32.49, 27.49, 22.48, 17.48, 12.47, 7.47, 2.49, 0)
  nedc_n <- c(12, 1, 1, 1, 1, 8, 1, 1, 1, 1, 1, 21, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 24,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 21, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 12, 1, 1, 1, 1, 1, 1, 1, 1, 15, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 18, 1, 1, 1, 1, 8, 1, 1, 1, 1, 1, 21, 1, 1, 1, 1,
    1, 2, 1, 1, 1, 1, 1, 24, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 21, 1, 1, 1, 1,
    1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 12, 1, 1, 1,
    1, 1, 1, 1, 1, 15, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 18, 1, 1, 1, 1, 8, 1, 1,
    1, 1, 1, 21, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 24, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 21, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1,
    1, 1, 1, 1, 12, 1, 1, 1, 1, 1, 1, 1, 1, 15, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    18, 1, 1, 1, 1, 8, 1, 1, 1, 1, 1, 21, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1,
    24, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 21, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 12, 1, 1, 1, 1, 1, 1, 1, 1, 15, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 27, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    50, 1, 1, 1, 1, 1, 1, 1, 1, 69, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    50, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 30, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 19)
  nedc <- inverse.rle(list(values = nedc_v, lengths = nedc_n))
  expect_equal(length(nedc), 1180)
  x <- ef_hbefa_cycle("HBEFA4/PC_petrol_Euro-4", "CO2", speed = nedc, full = TRUE)
  expect_equal(as.numeric(x$distance), 11.0419, tolerance = 1e-5)
  expect_equal(as.numeric(x$ef), 219.8859, tolerance = 1e-6)
  sumo <- list(  # class, pollutant, total in mg (EC: Wh)
    c("HBEFA4/PC_petrol_Euro-4", "CO2", 2.42796e+06),
    c("HBEFA4/PC_petrol_Euro-4", "NOx", 917.2),
    c("HBEFA4/PC_BEV", "EC", 1846.3),
    c("HBEFA4/PC_BEV", "PM", 309.409),
    c("HBEFA3/PC_G_EU4", "CO2", 3.06227e+06),
    c("HBEFA3/PC_G_EU4", "NOx", 1164.47),
    c("HBEFA2/P_7_7", "CO2", 1.99663e+06),
    c("HBEFA2/P_7_7", "NOx", 3595.37))
  for (s in sumo) {
    tot <- ef_hbefa_cycle(s[1], s[2], speed = nedc, full = TRUE)$total
    k <- if (s[2] == "EC") 1 else 1000
    expect_equal(as.numeric(tot) * k, as.numeric(s[3]), tolerance = 1e-5,
                 info = paste(s[1], s[2]))
  }
  # SUMO nefz_hbefa3_volumetric: 1316.38 ml of fuel
  tot <- ef_hbefa_cycle("HBEFA3/PC_G_EU4", "FC", speed = nedc, full = TRUE,
                        volumetric_fuel = TRUE)$total
  expect_equal(as.numeric(tot) * 1000, 1316.38, tolerance = 1e-5)
  # units input and several classes
  y <- ef_hbefa_cycle(c("HBEFA4/PC_petrol_Euro-4", "HBEFA3/PC_G_EU4"), "CO2",
                      speed = units::set_units(nedc, "km/h"))
  expect_equal(as.numeric(y), c(219.8859, 3062.27 / 11.0419), tolerance = 1e-5)
  expect_error(ef_hbefa_cycle("HBEFA4/PC_petrol_Euro-4", "CO2", speed = 10))
})
