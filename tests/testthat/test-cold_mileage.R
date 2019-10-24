context("emis_cold")

lkm <- units::set_units(1:10, km)
ta <- celsius(matrix(0:9, ncol = 12, nrow = 10))
a <- cold_mileage(lkm, rbind(ta, ta))

test_that("cold_mileage works", {
  expect_equal(round(cold_mileage(lkm, rbind(ta, ta))$V1[1]),
  1)
  expect_error(cold_mileage(ltrip = as.numeric(lkm), rbind(ta, ta)),
               ".?")

  expect_error(cold_mileage(ltrip = units::set_units(as.numeric(lkm), "m"),
                            rbind(ta, ta)),
               ".?")
  expect_error(cold_mileage(ltrip = units::set_units(as.numeric(lkm), "m"),
                            remove_units(rbind(ta, ta))),
               ".?")


})


test_that("cold_mileage works", {
  expect_equal(round(cold_mileage(lkm, ta[1])$V1[1]),
               1)
  expect_error(cold_mileage(lkm, as.numeric(ta$V1)),
               ".?")

})

