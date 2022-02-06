context("test-my_age")

data(net)
dpc <- c(seq(1,20,3), 20:10)
yy <- data.frame(a = 1:5, b = 5:1)    # perfiles por categoria de calle
pro_street <- c("a", "b", "a")         # categorias de cada calle
x <- c(100,5000, 3)                               # vehiculos

test_that("my_age works", {
  expect_equal(round(my_age(x = net$ldv,
                            y = dpc,
                            name = "PC_E25_1400")[1,1]),
               Vehicles(19))
})


test_that("my_age works", {
  expect_equal(round(my_age(x = net$ldv,
                            y = dpc,
                            name = "PC_E25_1400",
                            net = net)$PC_E25_1400_1[1]),
               Vehicles(19))
})


test_that("my_age works", {
  expect_equal(round(my_age(x = x,
                            y =  yy,
                            pro_street = pro_street)$vehicle_1[1]),
               Vehicles(7))
})

test_that("my_age stops", {
  expect_error(my_age(x = NULL, y = dpc),
               "M.?")
})

test_that("my_age stops", {
  expect_error(my_age(x = x,
                      y =  1,
                      pro_street = pro_street),
               "'y'.?")
})

test_that("my_age stops", {
  expect_error(my_age(x = x,
                      y =  data.frame(1)),
               "W.?")
})


test_that("my_age message", {
  expect_message(my_age(x = net$ldv,
                            y = dpc,
                            name = "PC_E25_1400",
                       verbose = TRUE),
               "Av.?")
  expect_message(my_age(x = net$ldv,
                       y = dpc,
                       name = "PC_E25_1400",
                       verbose = TRUE),
                "Nu.?")
})


test_that("my_age works", {
  expect_error(my_age(x = net$ldv,
                            y = dpc,
                            namerows = "s"),
               "l.?")
})


test_that("my_age works", {
  expect_equal(round(my_age(x = net$ldv,
                            y = dpc,
                            name = "PC_E25_1400",
                            agemax = 5)[1,1]),
               Vehicles(19))
})


test_that("my_age stops", {
  expect_error(round(my_age(x = net$ldv,
                            y = dpc,
                            name = "PC_E25_1400",
                            k = 1:2)),
               ".?")
})

test_that("my_age works", {
  expect_equal(round(my_age(x = net$ldv,
                      y = dpc,
                      namerows = 1:nrow(net))[1,1]),
               Vehicles(19))
})

