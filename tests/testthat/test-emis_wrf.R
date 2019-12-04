context("emis_wrf")

test_that("emis_wrf works", {
  expect_warning(emis_wrf(1),
                 ".?")
})

