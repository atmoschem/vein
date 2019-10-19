context("emis_wrf")

test_that("emis_wrf works", {
  expect_message(emis_wrf(1),
               ".?")
  expect_warning(emis_wrf(1),
                 ".?")
})
1
