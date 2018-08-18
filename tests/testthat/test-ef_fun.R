context("ef_fun")
data(fe2015)
CO <- vein::EmissionFactors(fe2015[fe2015$Pollutant == "CO", 11])

test_that("ef_fun works", {
  expect_equal(ef_fun(ef = CO, x0 = 27, k = 0.4, L = 33)[33],
               EmissionFactors(30.255301015700563738))
})
