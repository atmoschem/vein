context("age_ldv")
data(net)

test_that("age_ldv works 2", {
  expect_equal(age_ldv(x = 10, name = "MC", agemax = 2),
               Vehicles(data.frame(MC_1 = 5,
                                   MC_2 = 5)))
})

test_that("age_ldv stop 1", {
  expect_error(age_ldv(x = 10, name = "MC", agemax = 0),
               "A.?")
})

test_that("age_ldv stop 2", {
  expect_error(age_ldv(x = 10:11, name = "MC", agemax = 1, bystreet = TRUE),
               "L.?")
})

test_that("age_ldv works 2", {
  expect_equal(age_ldv(x = net$ldv,
                       a = rep(1, nrow(net)),
                       name = "MC", agemax = 2, net = net, bystreet = TRUE)$MC_1[1],
               Vehicles(2175))
})

test_that("age_ldv works 2", {
  expect_equal(age_ldv(x = net$ldv,
                       a = rep(1, nrow(net)),
                       name = "MC", agemax = 2,
                       k = rep(1, nrow(net)),
                       net = net, bystreet = TRUE)$MC_1[1],
               Vehicles(2175))
})

test_that("age_ldv works 2", {
  expect_equal(age_ldv(x = net$ldv,
                       a = rep(1, nrow(net)),
                       name = "MC", agemax = 2,
                       k = rep(1, nrow(net)),bystreet = TRUE)$MC_1[1],
               Vehicles(2175))
})

test_that("age_ldv works 2", {
  expect_message(age_ldv(x = net$ldv,
                         a = rep(1, nrow(net)),
                         name = "MC", agemax = 2,
                         k = rep(1, nrow(net)),
                         net = net, bystreet = TRUE,
                         verbose = TRUE)$MC_1[1],
                 "A.?")
})


test_that("age_ldv works 2", {
  expect_error(age_ldv(x = net$ldv,
                       namerows = 1,
                       a = rep(1, nrow(net)),
                       name = "MC", agemax = 2,
                       k = rep(1, nrow(net)),
                       net = net, bystreet = TRUE,
                       verbose = TRUE)$MC_1[1],
               "l.?")
})

test_that("age_ldv works 2", {
  expect_equal(age_ldv(x = net$ldv,
                       namerows = 1:nrow(net),
                       a = rep(1, nrow(net)),
                       name = "MC", agemax = 2,
                       k = rep(1, nrow(net)),
                       bystreet = TRUE)$MC_1[1],
               Vehicles(2175))
})


test_that("age_ldv works 2", {
  expect_equal(age_ldv(x = net$ldv, name = "MC", agemax = 2, net = net)$MC_1[1],
               Vehicles(2175))
})

test_that("age_ldv warns 1", {
  expect_warning(age_ldv(x = net$ldv,
                         name = "MC",
                         agemax = 2,
                         a = rep(1, nrow(net)),
                         b = rep(0, nrow(net)))$MC_1[1],
                 "l.?")
})

test_that("age_ldv works", {
  expect_equal(round(age_ldv(x = net$ldv,
                             name = "MC",
                             agemax = 2,
                             a = rep(1, nrow(net)),
                             b = rep(0, nrow(net)), bystreet = T)$MC_1[1]),
               Vehicles(2175))
})

test_that("age_ldv mes 1", {
  expect_message(age_ldv(x = net$ldv,
                         name = "MC",
                         agemax = 2,
                         a = rep(1, nrow(net)),
                         b = rep(0, nrow(net)), bystreet = T,
                         verbose = TRUE),
                 "A.?")
})


test_that("age_ldv works", {
  expect_equal(round(age_ldv(x = net$ldv,
                             name = "MC",
                             agemax = 2,
                             a = rep(1, nrow(net)),
                             b = rep(0, nrow(net)), net = net, bystreet = T)$MC_1[1]),
               Vehicles(2175))
})


test_that("age_ldv works 2", {
  expect_equal(age_ldv(x = 10:11, name = "MC", agemax = 2, k = 1:2)$MC_1[1],
               Vehicles(5))
})


test_that("age_ldv works 2", {
  expect_equal(age_ldv(x = 10:11, namerows = 1:2, name = "MC", agemax = 2, k = 1:2)$MC_1[1],
               Vehicles(5))
})


test_that("age_ldv works 2", {
  expect_message(age_ldv(x = 10:11, name = "MC", agemax = 2, k = 1:2, verbose = TRUE)$MC_1[1],
                 "A.?")
})

test_that("age_ldv works 2", {
  expect_error(age_ldv(x = 10:11, name = "MC", agemax = 2, k = 1:2, namerows = "1")$MC_1[1],
               "l.?")
})

