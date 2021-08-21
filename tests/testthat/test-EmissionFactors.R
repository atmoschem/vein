context("EmissionFactors")

test_that("EmissionFactors works", {
  expect_equal(EmissionFactors(as.numeric(ef_cetesb("CO_0km", "PC_G", year = 2017)))[[1]],
               0.1612112 )
})

test_that("EmissionFactors works", {
  expect_warning(EmissionFactors(units::set_units(1, "g")),
                 "Check.?\\(?")
})

test_that("EmissionFactors prints", {
  expect_output(print(EmissionFactors(data.frame(a = 1:5, b = 1:5))),
                "...?")
  expect_output(print(EmissionFactors(data.frame(a = 1:11, b = 1:11))),
                "...?")
  expect_output(print(EmissionFactors(matrix(0, ncol = 11))),
                "...?")
  expect_output(print(EmissionFactors(matrix(1:110, ncol = 11))),
                "...?")
})

test_that("EmissionFactors prints", {
  expect_output(summary(EmissionFactors(data.frame(a = 1:11, b = 1:11))),
                "Mean?")
  expect_output(summary(EmissionFactors(data.frame(a = 1:11, b = 1:11)))[1,1],
                "Min.?")
})


test_that("EmissionFactors works", {
  # expect_equal(plot(EmissionFactors(data.frame(a = 1:5)))$mfrow[1],
               # 1)
  expect_equal(plot(EmissionFactors(data.frame(a = 1:11, b = 1:11)))$mfrow[1],
               NULL)
  # expect_equal(plot(EmissionFactors(matrix(0, ncol = 4)))$mfrow[1],
                 # 2)
  # expect_equal(plot(EmissionFactors(matrix(0, ncol = 6)))$mfrow[1],
                 # 2)
  # expect_equal(plot(EmissionFactors(matrix(0, ncol = 8)))$mfrow[1],
                 # 3)
  # expect_message(plot(EmissionFactors(matrix(0, ncol = 11))),
                # "Plo?")

})

