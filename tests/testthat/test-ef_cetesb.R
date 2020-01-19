context("ef_cetesb")

test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO", "PC_G")[1]), 0.21)
})

test_that("ef_cetesb stops", {
  expect_error(ef_cetesb("CO", "PC_G", year = 10),
               "Choose.?\\(?")
})

test_that("ef_cetesb stops", {
  expect_error(ef_cetesb("caca", "PC_G"),
               "Please.?\\(?")
})

test_that("ef_cetesb message", {
  expect_message(ef_cetesb("D_10_25", "PC_G"),
                 "Units.?\\(?")
})

test_that("ef_cetesb message", {
  expect_message(ef_cetesb("R_10_25", "PC_G"),
                 "Units.?\\(?")
})

test_that("ef_cetesb stops", {
  expect_error(ef_cetesb("CO", "caca"),
               "Please.?\\(?")
})


test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO", "PC_G", full = TRUE)$CO[1]), 0.21)
})

test_that("ef_cetesb works", {
  expect_equal(round(as.numeric(ef_cetesb("R_10_25",
                                          "PC_G",
                                          full = TRUE)$R_10_25[1]), 2),
               0.02)
})

test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO",
                                    "PC_G",
                                    full = TRUE)$CO[1]),
               0.21)
})

test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO",
                                    "PC_G",
                                    year = 2020)[1]),
               0.19)
})

test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO",
                                    "PC_G",
                                    year = 1980)[1]),
               33.6)
})

test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO",
                                    c("PC_G", "LCV_G"),
                                    year = 2020,
                                    agemax = 10)[1,1]),
               0.19)
})

test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO",
                                    c("PC_G", "LCV_G"),
                                    year = 1980,
                                    agemax = 10)[1,1]),
               33.6)
})

test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO",
                                    c("PC_G", "LCV_G"),
                                    year = 2020,
                                    agemax = 100)[1,1]),
               0.19)
})


test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO",
                                    c("PC_G", "LCV_G"),
                                    year = 1980,
                                    agemax = 100)[1,1]),
               33.6)
})

test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO",
                                    c("PC_G"),
                                    year = 2020,
                                    agemax = 100)[1]),
               0.19)
})


test_that("ef_cetesb works", {
  expect_equal(as.numeric(ef_cetesb("CO",
                                    c("PC_G"),
                                    year = 1980,
                                    agemax = 100)[1]),
               33.6)
})

