context("ef_im")


test_that("multiplication works", {
  expect_equal(as.numeric(ef_im(ef = seq(0.1, 2, 0.2),
                                seq(0.1, 1, 0.1),
                                10)[1]),
               0.2)
})


test_that("ef_im stops", {
  expect_error(ef_im(tc = 1),
               "No.?\\(?")
})

test_that("ef_im stops", {
  expect_error(ef_im(ef = 1),
               "No.?\\(?")
})

test_that("multiplication works", {
  expect_equal(as.numeric(ef_im(ef = seq(0.1, 2, 0.2),
                                tc = seq(0.1, 1, 0.1),
                                amileage = 10,
                                max_amileage = 1000,
                                verbose = FALSE)[1]),
               0.2)
})

test_that("multiplication works", {
  expect_equal(as.numeric(ef_im(ef = seq(0.1, 2, 0.2),
                                tc = seq(0.1, 1, 0.1),
                                amileage = 10,
                                max_amileage = 1000,
                                max_ef = 3)[1]),
               0.2)
})

