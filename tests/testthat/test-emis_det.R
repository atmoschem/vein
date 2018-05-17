context("emis_det")
data(fkm)
pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)


test_that("emis_det works", {
  expect_equal(emis_det(po = "CO",
                        cc = 1000,
                        eu = "III",
                        km = pckma[1:11])[10],
               1.756295 + 6.74e-08)
})
