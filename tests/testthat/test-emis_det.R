context("emis_det")
data(fkm)
pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)

km <- units::set_units(cumsum(pckma[1:11]), km)
test_that("emis_det works", {
  expect_equal(emis_det(po = "CO",
                        cc = "<=1400",
                        eu = "III",
                        km = km)[10],
               1.91)
})
