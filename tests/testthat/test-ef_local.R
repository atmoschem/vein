context("ef_local")

data("fe2015")
ef <- fe2015
names(ef)

test_that("ef_local works", {
  expect_equal(as.numeric(ef_local("CO", "PC_G", ef = ef)[1]), 0.155)
})


test_that("ef_local stops", {
  expect_error(ef_local("caca", "PC_G", ef = ef),
               ".?")
})

test_that("ef_local stops", {
  expect_error(ef_local("CO", "PC_G", ef = ef, year = 0),
               ".?")
})


test_that("ef_local works", {
  expect_equal(as.numeric(ef_local("CO", "PC_G", ef = ef, full = TRUE)$CO[1]), 0.155)
})


test_that("ef_local works", {
  expect_equal(as.numeric(ef_local("CO",
                                    "PC_G", ef = ef,
                                    year = 2020)[1]),
               0.155)
})

test_that("ef_local works", {
  expect_equal(as.numeric(ef_local("CO",
                                    "PC_G", ef = ef,
                                    year = 1980)[1]),
               33)
})
