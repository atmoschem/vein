context("EmissionFactors fixes")

test_that("units are converted to g/km", {
  r <- suppressMessages(EmissionFactors(units::set_units(1, "kg/km")))
  expect_equal(as.numeric(r), 1000)
  expect_equal(as.character(units(r)), "g/km")
})

test_that("EmissionFactors is idempotent on data.frames", {
  d <- EmissionFactors(data.frame(a = 1:2))
  d2 <- EmissionFactors(d)
  expect_equal(as.character(units(d2$a)), "g/km")
  expect_equal(as.numeric(d2$a), 1:2)
  expect_equal(sum(class(d2) == "EmissionFactors"), 1)
})

test_that("NA and unsupported inputs", {
  expect_true(is.na(as.numeric(EmissionFactors(NA))))
  expect_error(EmissionFactors("a"), "numeric")
})
