context("ef_ldv_speed")
pol <- c("CO", "NOx", "HC", "NMHC", "CH4", "FC", "PM", "CO2", "Pb", "SO2")
test_that("ef_ldv_speed works", {
  expect_equal(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
                            p = "CO", show.equation = FALSE)(10), 65.87283)
})

test_that("ef_ldv_speed works", {
  expect_equal(sapply(1:length(pol), function(i){
    ef_ldv_speed("PC", "4S", "<=1400", "G", "PRE", pol[i], x = 10)(30)}),
    c(3.297007e+01 + 4.57e-06,
      1.722000e+00,
      2.873231e+00 + 2.27e-08,
      2.742231e+00 + 2.27e-08,
      1.310000e-01,
      7.916123e+01 + 2.66e-06,
      0.000000e+00,
      2.519344e+02 + 2.77e-05,
      3.320000e-05 + 0.000561,
      1.583225e-03))
})

# NMHC
#LDV
pol <- c(
  # alkanes
  "ethane", "propane", "butane", "isobutane", "pentane", "isopentane", "hexane",
  "heptane", "octane", "2-methylhexane", "nonane", "2-methylheptane",
  "3-methylhexane", "decane", "3-methylheptane", "alkanes_C10_C12",
  "alkanes_C13", "cycloalkanes",
  # alkenes
  "ethylene", "propylene", "propadiene", "1-butene", "isobutene", "2-butene",
  "1,3-butadiene", "1-pentene", "2-pentene", "1-hexene", "dimethylhexene",
  # alkynes
  "1-butyne", "propyne", "acetylene",
  # aldehydes
  "formaldehyde", "acetaldehyde", "acrolein", "benzaldehyde",
  "crotonaldehyde", "methacrolein", "butyraldehyde", "isobutanaldehyde",
  "propionaldehyde", "hexanal", "i-valeraldehyde", "valeraldehyde",
  "o-tolualdehyde", "m-tolualdehyde", "p-tolualdehyde",
  # ketones
  "acetone", "methylethlketone",
  # aromatics
  "toluene", "ethylbenzene", "m-xylene", "p-xylene", "o-xylene",
  "1,2,3-trimethylbenzene", "1,2,4-trimethylbenzene", "1,3,5-trimethylbenzene",
  "styrene", "benzene", "C9", "C10", "C13"
)
# PRE EURO
vect <- c(1.65, 0.47, 2.9, 1.29, 1.78, 4.86, 1.29, 0.36, 0.56, 0.8, 0.06, 0,
          0.56, 0.22, 0.4, 0.03, 0.06, 0.88, 8.71, 4.87, 0, 0.5, 4.21, 1.27,
          1.42, 0.09, 0.23, 0, 0, 0.05, 0.76, 5.5, 2.08, 0.59, 0.16, 0.6, 0.02,
          0, 0, 0, 0.11, 0, 0, 0, 0.19, 0.38, 0.19, 0.21, 0.11, 12.84, 4.78,
          3.33, 3.33, 4.52, 0.59, 2.53, 1.11, 0.57, 6.83, 3.12, 0, 6.01)
expect_equal(
  sapply(1:length(pol), function(i){
    round(ef_ldv_speed("LCV", "4S", "<3.5", "G", "PRE",
                       pol[i], k = 0.4096)(30), 4)*100
  }),
  c(vect[1:18],
    vect[19:29],
    vect[30:32],
    vect[33:47],
    vect[48:49],
    vect[50:62])
)
ef_ldv_speed("PC", "4S", "<=1400", "G", "I", "NMHC", k = 5.136505)(30)
#EURO I
vect <-
  c(3.19, 0.65, 5.24, 1.59, 2.15, 6.81, 1.61, 0.74, 0.53, 1.48, 0.16, 0.57,
    1.14, 0.19, 0.54, 1.76, 1.45, 1.14, 7.3, 3.82, 0.05, 0.73, 2.22, 1.42,
    0.91, 0.11, 0.34, 0.17, 0.15, 0.21, 0.08, 2.81, 1.7, 0.75, 0.19, 0.22,
    0.04, 0.05, 0.05, 0, 0.05, 0, 0, 0.01, 0.07, 0.13, 0.06, 0.61, 0.05,
    10.98, 1.89, 2.72, 2.72, 2.26, 0.86, 4.21, 1.42, 1.01, 5.61, 4.21,
    3.07, 3.46)
expect_equal(
  sapply(1:length(pol), function(i){
    round(ef_ldv_speed("PC", "4S", "<=1400", "G", "I",
                       pol[i], k = 5.136505)(30), 4)*100
  }),
  c(vect[1:18],
    vect[19:29],
    vect[30:32],
    vect[33:47],
    vect[48:49],
    vect[50:62])
)
# Diesel
ef_ldv_speed("PC", "4S", "<=1400", "D", "I", "NMHC", k = 18.2163)(30)

vect <-
  c(0.33, 0.11, 0.11, 0.07, 0.04, 0.52, 0, 0.2, 0.25, 0.45, 0.67, 0.12,
    0.22, 1.18, 0.2, 2.15, 17.91, 0.65, 10.97, 3.6, 0, 0, 1.11, 0.52, 0.97,
    0, 0, 0, 0, 0, 0, 2.34, 12, 6.47, 3.58, 0.86, 1.1, 0.77, 0.85, 2.09, 1.77,
    0.16, 0.11, 0.41, 0.24, 0.34, 0.35, 2.94, 1.2, 0.69, 0.29, 0.31, 0.31,
    0.27, 0.25, 0.57, 0.31, 0.37, 1.98, 0.78, 0, 13.37)

expect_equal(
  sapply(1:length(pol), function(i){
    round(ef_ldv_speed("PC", "4S", "<=1400", "D", "I",
                       pol[i], k = 18.2163)(30), 4)*100
  }),
  c(vect[1:18],
    vect[19:29],
    vect[30:32],
    vect[33:47],
    vect[48:49],
    vect[50:62])
)
# LPG
ef_ldv_speed("PC", "4S", "<=1400", "LPG", "I", "NMHC", k = 3.920033)(30)

vect <-
c(2.34, 49.85, 15.5, 6.95, 0.35, 1.26, 0, 0.18, 0.04, 0.25, 0.01, 0.09, 0.19,
  0, 0.08, 0.01, 0, 0.1, 5.2, 5.19, 0, 0, 0.63, 0.53, 0.15, 0, 0, 0, 0, 0, 0,
  1.28, 1.56, 1.81, 0.59, 0.03, 0.36, 0.1, 0.11, 0, 0.7, 0, 0.01, 0, 0, 0, 0,
  0.78, 0, 1.22, 0.24, 0.38, 0.38, 0.26, 0.05, 0.25, 0.08, 0.02, 0.63, 0.25,
  0, 0)


expect_equal(
  sapply(1:length(pol), function(i){
    round(ef_ldv_speed("PC", "4S", "<=1400", "LPG", "I",
                       pol[i], k = 3.920033)(30), 4)*100
  }),
  c(vect[1:18],
    vect[19:29],
    vect[30:32],
    vect[33:47],
    vect[48:49],
    vect[50:62])
)

#PC
df_st <- rev(c("VIc", as.character(as.roman(6:1)), "PRE"))
# "PCDD", "PCDF", "PCB"
pol <- c("PCDD", "PCDF", "PCB")
EF <- sapply(1:3, function(j){
  sapply(1:8, function(i){
    ef_ldv_speed("PC", "4S", "<=1400", "G", df_st[i], pol[j])(100) })
})

ef <- units::set_units(c(10.3, rep(13, 2), rep(2.7, 5)), pg)
ef <- units::set_units(ef, g)
ef1 <- as.numeric(ef)
ef <- units::set_units(c(21.12, rep(19, 2), rep(4.1, 5)), pg)
ef <- units::set_units(ef, g)
ef2 <- as.numeric(ef)
ef <- units::set_units(c(rep(6.4, 2), rep(1.36, 5)), pg)
ef <- units::set_units(ef, g)
ef3 <- as.numeric(ef)

test_that("ef_ldv_speed works", {
  expect_equal(EF,
    matrix(c(ef1, ef2, ef3), ncol =3))
})


test_that("ef_ldv_speed works", {
  expect_equal(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G",
                            eu = data.frame("PRE"),
                            p = "CO", show.equation = FALSE,
                            speed = Speed(10))$V1, EmissionFactors(65.87283))
})

test_that("ef_ldv_speed works", {
  expect_error(ef_ldv_speed(v = "PC",
                            t = "4S",
                            cc = "<=1400",
                            f = "G",
                            eu = data.frame("PRE"),
                            p = "CO"),
               "A.?")
})

test_that("ef_ldv_speed works", {
  expect_error(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G",
                            eu = data.frame("PRE"),
                            p = "CO", show.equation = FALSE,
                            speed = 10),
               "s.?")
})


test_that("ef_ldv_speed works", {
  expect_error(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G",
                            eu = data.frame("PRE"),
                            p = "CO", show.equation = FALSE,
                            speed = units::set_units(10, "m/m")),
               "U.?")
})


test_that("ef_ldv_speed works", {
  expect_output(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "D",
                            eu = data.frame("PRE"),
                            p = "AS_urban", show.equation = FALSE,
                            speed = Speed(10)),
                "A.?")
  expect_output(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "D",
                             eu = data.frame("PRE"),
                             p = "N_urban", show.equation = FALSE,
                             speed = Speed(10))$V1,
                "U.?")
  expect_output(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "D",
                             eu = "PRE",
                             p = "CO", show.equation = TRUE),
                "a.?")
  expect_output(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "D",
                             eu = "PRE",
                             p = "CO", show.equation = TRUE),
                "E.?")
})


test_that("ef_ldv_speed works", {
  expect_equal(round(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G",
                             eu = "PRE",
                             p = "CO",
                             speed = Speed(10))),
                EmissionFactors(66))
})

test_that("ef_ldv_speed works", {
  expect_equal(round(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G",
                                  eu = c("PRE", "I"),
                                  p = "CO",
                                  speed = Speed(10))$PRE1[1]),
               EmissionFactors(66))
})

test_that("ef_ldv_speed works", {
  expect_equal(round(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G",
                                  eu = c("PRE", "I"),
                                  p = "SO2",
                                  speed = Speed(10),
                                  x = 10)$PRE1[1]),
               EmissionFactors(0))
})

test_that("ef_ldv_speed works", {
  expect_equal(round(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G",
                                  eu = c("PRE", "I"),
                                  p = "SO2",
                                  x = 10)[[1]](0)),
               0)
})

test_that("ef_ldv_speed works", {
  expect_equal(round(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G",
                                  eu = data.frame("PRE"),
                                  p = "SO2",
                                  speed = Speed(0),
                                  x = 10)$V1),
               EmissionFactors(0))
})

test_that("ef_ldv_speed works", {
  expect_equal(round(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G",
                                  eu = c("PRE", "I"),
                                  p = "HC")[[1]](0)),
               6)
})
