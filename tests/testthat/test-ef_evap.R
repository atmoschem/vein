context("ef_evap")

test_that("ef_evap works", {
  expect_equal(ef_evap(ef = "erhotc",
                       v = "PC",
                       cc = "<=1400",
                       dt = "0_15",
                       ca = "no",
                       show = FALSE),
               units::as_units(2.39, "g"))
})
