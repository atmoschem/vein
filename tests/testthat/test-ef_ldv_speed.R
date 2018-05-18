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
      5.937092e-04,
      1.583225e-03))
})
