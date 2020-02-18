context("ef_ldv_cold_list")

test_that("multiplication works", {
  expect_equal(ef_ldv_cold(t = 17,
                           cc = "<=1400",
                           f = "G",
                           eu = "I",
                           p = "CO")(20),
               3.884)
})
