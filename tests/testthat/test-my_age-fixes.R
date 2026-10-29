context("my_age fixes")

yy <- data.frame(a = 1:5, b = 5:1)

test_that("my_age stops with unknown pro_street categories", {
  expect_error(my_age(x = c(100, 5000, 3), y = yy,
    pro_street = c("a", "zz", "b")),
  "zz")
})

test_that("my_age keeps one row per street with pro_street", {
  x <- my_age(x = c(100, 5000, 3), y = yy, pro_street = c("a", "b", "b"))
  expect_equal(nrow(x), 3)
  expect_equal(unname(rowSums(remove_units(x))), c(100, 5000, 3))
  expect_equal(as.numeric(x$vehicle_1), c(100 / 15, 5000 * 5 / 15, 3 * 5 / 15))
})

test_that("my_age accepts factor pro_street", {
  y <- data.frame(b = 5:1, a = 1:5)
  expect_equal(my_age(x = c(100, 100), y = y, pro_street = factor(c("a", "b"))),
    my_age(x = c(100, 100), y = y, pro_street = c("a", "b")))
  expect_equal(as.numeric(my_age(x = 100, y = y, pro_street = factor("a"))[1, 1]),
    100 / 15)
})

test_that("my_age agemax keeps the total of vehicles", {
  x <- my_age(x = 100, y = rep(1, 10), agemax = 5)
  expect_equal(ncol(x), 5)
  expect_equal(sum(remove_units(x)), 100)
  expect_message(my_age(x = 100, y = rep(1, 10), agemax = 5, verbose = TRUE),
    "is 100 ")
  x <- my_age(x = c(100, 200), y = c(1, 1), agemax = 1)
  expect_true(inherits(x, "Vehicles"))
  expect_equal(as.numeric(x[[1]]), c(100, 200))
  x <- my_age(x = 100, y = 1:3, agemax = 5)
  expect_equal(unname(unlist(remove_units(x))), c(100 / 6, 200 / 6, 50, 0, 0))
  x <- my_age(x = c(1, 2), y = yy, pro_street = c("a", "b"), agemax = 2)
  expect_equal(unname(rowSums(remove_units(x))), c(1, 2))
})

test_that("my_age agemax with net", {
  data(net)
  x <- my_age(x = net$ldv[1:3], y = 1:4, net = net[1:3, ], agemax = 2)
  expect_true(inherits(x, "sf"))
  expect_equal(sum(as.numeric(unlist(sf::st_drop_geometry(x)))),
    sum(as.numeric(net$ldv[1:3])))
})

test_that("my_age checks length of k", {
  expect_error(my_age(x = c(1, 2), y = c(1, 1), k = 1:3), "k")
  x <- my_age(x = c(1, 2), y = c(1, 1), k = c(1, 2))
  expect_equal(unname(rowSums(remove_units(x))), c(1, 4))
})
