context("ef_ldv_speed")

test_that("ef_ldv_speed works", {
  expect_equal(ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
                            p = "CO", show.equation = FALSE)(10), 65.87283)
})
