context("age_veh")
data(net)
sf::st_crs(net) <- 4326


test_that("age_veh works 2", {
  expect_equal(age_veh(x = 10, type = "hdv", name = "MC", agemax = 2, k = 1),
               Vehicles(data.frame(MC_1 = 5,
                                   MC_2 = 5)))
})


test_that("age_veh works 2", {
  expect_equal(age_veh(x = net$ldv,
                       type = "mc",
                        a = rep(1, nrow(net)),
                        name = "MC", agemax = 2, net = net, bystreet = TRUE)$MC_1[1],
               Vehicles(2175))
})

test_that("age_veh works 2", {
  expect_equal(age_veh(x = net$ldv,
                       type = "mc",
                        a = rep(1, nrow(net)),
                        name = "MC", agemax = 2,
                        k = rep(1, nrow(net)),
                        net = net, bystreet = TRUE)$MC_1[1],
               Vehicles(2175))
})

test_that("age_veh works 2", {
  expect_equal(age_veh(x = net$ldv,
                       type = "mc",
                        a = rep(1, nrow(net)),
                        name = "MC", agemax = 2,
                        k = rep(1, nrow(net)),bystreet = TRUE)$MC_1[1],
               Vehicles(2175))
})

test_that("age_veh works 2", {
  expect_message(age_veh(x = net$ldv,
                          a = rep(1, nrow(net)),
                          name = "MC", agemax = 2,
                          k = rep(1, nrow(net)),
                          net = net, bystreet = TRUE,
                          verbose = TRUE)$MC_1[1],
                 "A.?")
})


test_that("age_veh error 2", {
  expect_error(age_veh(x = net$ldv,
                       type = "caca",
                        namerows = 1,
                        a = rep(1, nrow(net)),
                        name = "MC", agemax = 2,
                        k = rep(1, nrow(net)),
                        net = net, bystreet = TRUE,
                        verbose = TRUE)$MC_1[1],
               "?")
})


test_that("age_veh works 2", {
  expect_equal(age_veh(x = 10, name = "MC", agemax = 2),
               Vehicles(data.frame(MC_1 = 5,
                                   MC_2 = 5)))
})

test_that("age_veh works 2", {
  expect_equal(age_veh(x = net$ldv,
                       a = rep(1, nrow(net)),
                       name = "MC", agemax = 2, net = net, bystreet = TRUE)$MC_1[1],
               Vehicles(2175))
})

