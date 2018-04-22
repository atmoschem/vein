context("ef_ldv_scaled")
data(fe2015)
co1 <- fe2015[fe2015$Pollutant=="CO", ]

test_that("ef_ldv_scaled works", {
  expect_equal(ef_ldv_scaled(dfcol = co1$PC_G[1], v = "PC", t = "4S",
                             cc = "<=1400", f = "G",
                             eu = co1$Euro_LDV, p = "CO")[[1]](10),
               0.1873607)
})
