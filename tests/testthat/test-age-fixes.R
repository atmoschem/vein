context("age fixes")

data(net)

test_that("age_veh applies k only once", {
  expect_equal(sum(remove_units(age_veh(x = 100, agemax = 10, k = 0.5))), 50)
  x <- age_veh(x = c(10, 20), agemax = 3, k = c(1, 2))
  expect_equal(unname(rowSums(remove_units(x))), c(10, 40))
  x <- age_veh(x = c(10, 20), a = c(1.7, 1.7), b = c(-0.2, -0.2),
    bystreet = TRUE, agemax = 3, k = 0.5)
  expect_equal(unname(rowSums(remove_units(x))), c(5, 10))
})

test_that("age_veh with time and net", {
  x <- age_veh(x = 100, agemax = 5, time = "1/h")
  expect_equal(as.character(units(x[[1]])), "veh/h")
  x <- age_veh(x = net$ldv[1:3], agemax = 5, time = "1/h", net = net[1:3, ],
    k = 0.5)
  expect_true(inherits(x, "sf"))
  expect_equal(sum(as.numeric(unlist(sf::st_drop_geometry(x)))),
    sum(as.numeric(net$ldv[1:3])) * 0.5)
})

test_that("age_veh with agemin == agemax", {
  expect_equal(as.numeric(age_veh(x = 100, agemax = 1)[[1]]), 100)
  x <- remove_units(age_veh(x = 100, agemin = 3, agemax = 3))
  expect_equal(unname(unlist(x)), c(0, 0, 100))
  expect_error(age_veh(x = 100, agemin = 4, agemax = 3), "agemin")
  expect_error(age_veh(x = 1:2, k = 1:3), "k")
})

test_that("age_veh agemin fills zeros and keeps total", {
  x <- remove_units(age_veh(x = 100, agemin = 3, agemax = 10))
  expect_equal(unname(unlist(x[1:2])), c(0, 0))
  expect_equal(sum(x), 100)
})

test_that("deprecated age_ldv, age_hdv and age_moto call age_veh", {
  expect_warning(x <- age_ldv(x = 1:3, name = "PC", agemax = 4, k = 2),
    "deprecated")
  expect_equal(x, age_veh(x = 1:3, name = "PC", agemax = 4, k = 2))
  expect_warning(x <- age_hdv(x = 1:3, agemax = 4), "deprecated")
  expect_equal(x, age_veh(x = 1:3, type = "hdv", agemax = 4))
  expect_warning(x <- age_moto(x = 1:3, agemax = 4), "deprecated")
  expect_equal(x, age_veh(x = 1:3, type = "mc", agemax = 4))
  expect_warning(x <- age_hdv(x = net$hdv[1:3], net = net[1:3, ]),
    "deprecated")
  expect_true(inherits(x, "sf"))
})

test_that("age uses defaults by type and validates type", {
  expect_equal(age(rep(1, 3), type = "gompertz"),
    age(rep(1, 3), type = "gompertz", a = 1.798, b = -0.137))
  expect_equal(age(rep(1, 3), type = "double_logistic"),
    age(rep(1, 3), type = "double_logistic", a = 0.19, b = 21))
  expect_equal(age(rep(1, 3), type = "weibull2"),
    age(rep(1, 3), type = "weibull2", a = 26, b = 11))
  expect_equal(age(rep(1, 3)),
    age(rep(1, 3), type = "weibull", a = 14.46, b = 4.79))
  expect_true(all(as.numeric(age(rep(1, 5), type = "gompertz")) < 1))
  expect_error(age(1:5, type = "Weibull"))
})
