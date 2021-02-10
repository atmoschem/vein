context("Emissions")

data(net)
data(pc_profile)
data(fe2015)
data(fkm)
PC_G <- c(33491,22340,24818,31808,46458,28574,24856,28972,37818,49050,87923,
          133833,138441,142682,171029,151048,115228,98664,126444,101027,
          84771,55864,36306,21079,20138,17439, 7854,2215,656,1262,476,512,
          1181, 4991, 3711, 5653, 7039, 5839, 4257,3824, 3068)
veh <- data.frame(PC_G = PC_G)
pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")

test_that("Emissions works", {
  expect_equal(Emissions(data.frame(a = 1))[1,1],
               Emissions(1))
})

test_that("Emissions stops", {
  expect_error(Emissions(units::set_units(1, "km")),
               "units.?")
})

test_that("Emissions prints", {
  expect_output(print(Emissions(data.frame(a = 1:5, b = 1:5))),
                "...?")
  expect_output(print(Emissions(data.frame(a = 1:11, b = 1:11))),
                "...?")
  expect_output(print(Emissions(matrix(0, ncol = 11))),
                "...?")
  expect_output(print(Emissions(matrix(1:110, ncol = 11))),
                "...?")
})


test_that("Emissions prints", {
  expect_output(summary(Emissions(data.frame(a = 1:11, b = 1:11))),
                "Total?")
  expect_output(summary(Emissions(data.frame(a = 1:11, b = 1:11)))[1],
                "Min.?")
})


test_that("Emissions works", {
  expect_equal(plot(Emissions(data.frame(a = 1:5)))$mfrow[1],
               NULL)
  expect_output(plot(Emissions(data.frame(a = 1:5)))$mfrow[1],
                "Av.?")
})
