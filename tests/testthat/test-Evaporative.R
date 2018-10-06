context("Evaporative")


test_that("Evaporative works", {
  expect_equal(as.numeric(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       ca = "no")),
               2.39)
})
