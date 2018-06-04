context("ef_ldv_cold")

test_that("ef_ldv_cold works", {
  expect_equal(ef_ldv_cold(ta = 15,
                           cc = "<=1400",
                           f = "G",
                           eu = "I",
                           p = "CO")(10),
               2.754)
})
