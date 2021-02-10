context("ef_cetesb")

test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO_0km", "PC_G")[1]), 0.1612112)
})

test_that("ef_cetesb works", {
  expect_equal(round(as.numeric(ef_cetesb("CO", "SLT", scale = "tunnel")[1])), 0)
})

test_that("ef_cetesb message", {
  expect_message(ef_cetesb("CO", "SLT", scale = "tunnel"), ".?")
})


test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("SO2", "PC_G", sppm = 300)[1]), 0.03518138)
})

test_that("ef_cetesb stops", {
  expect_error(ef_cetesb("CO", "PC_G", year = 10),
               ".?")
})

test_that("ef_cetesb stops", {
  expect_error(ef_cetesb("caca", "PC_G"),
               ".?")
})

test_that("ef_cetesb message", {
  expect_message(ef_cetesb("D_10_25", "PC_G", verbose = TRUE),
                 "Units.?\\(?")
})

test_that("ef_cetesb message", {
  expect_message(ef_cetesb("R_10_25", "PC_G", verbose = TRUE),
                 "?.")
})

test_that("ef_cetesb stops", {
  expect_error(ef_cetesb("CO", "caca"),
               "")
})


test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO_0km", "PC_G", full = TRUE)$CO[1]), 0.1612112)
})

test_that("ef_cetesb works", {
  expect_equal(round(as.numeric(ef_cetesb("R_10_25",
                                          "PC_G",
                                          full = TRUE)$R_10_25[1]), 2),
               0.02)
})

test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO_0km",
                                    "PC_G",
                                    full = TRUE)$CO[1]),
               0.1612112)
})

test_that("ef_cetesb works", {
  expect_error(ef_cetesb("CO_0km",
                                    "PC_G",
                                    year = 2020),
               ".?")
})

test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO",
                                    "PC_G",
                                    year = 1980)[1]),
               33)
})

test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO",
                                    c("PC_G"),
                                    year = 1980,
                                    agemax = 100)[1]),
               33)
})

test_that("ef_cetesb works", {
  expect_error(ef_cetesb("SO2",
                                    c("PC_G", "LCV_G"),
                                    year = 2020,
                                    agemax = 10, sppm = 300),
               "?")
})

test_that("ef_cetesb works", {
  expect_error(ef_cetesb("SO2",
                           c("PC_G", "LCV_G"),
                           year = 2020,
                           agemax = 10),
                 "?")
})

