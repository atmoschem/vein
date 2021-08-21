context("Vehicles")

data(net)
nett <- net[1, "ldv"]
nett$ldv <- remove_units(nett$ldv)

test_that("Vehicles works", {
  expect_equal(Vehicles(as.numeric(ef_cetesb("CO_0km", "PC_G")))[[1]],
               0.1612112)
  expect_error(Vehicles(units::set_units(1, "g")),
                 "u.?")
  expect_output(print(Vehicles(data.frame(a = 1:5, b = 1:5))),
                "...?")
  expect_output(print(Vehicles(matrix(0, ncol = 11))),
                "...?")
  expect_output(summary(Vehicles(data.frame(a = 1:11, b = 1:11))),
                "Mean?")
  expect_output(summary(Vehicles(data.frame(a = 1:11, b = 1:11)))[1,1],
                "Min.?")
  expect_equal(plot(Vehicles(data.frame(a = 1:5))),
               NULL)
  expect_equal(plot(Vehicles(data.frame(a = 1:11, b = 1:11))),
               NULL)
  expect_equal(Vehicles(nett)$ldv,
               Vehicles(4350))
})

