context("ef_hdv_scaled")
data(fe2015)
co1 <- fe2015[fe2015$Pollutant=="CO", ]

test_that("ef_hdv_scaled works", {
  expect_equal(ef_hdv_scaled(dfcol = co1$LT, v = "Trucks", t = "RT",
                             g = "<=7.5", eu = co1$Euro_HDV, gr = 0,
                             l = 0.5, p = "CO")[[1]](10),
               0.1309584 + 2.5e-08)
})
