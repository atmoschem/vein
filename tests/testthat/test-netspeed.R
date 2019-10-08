context("test-netspeed")
data(net)
data(pc_profile)
pc_week <- temp_fact(net$ldv + net$hdv, pc_profile)

test_that("netspeed works", {
  expect_equal(round(netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)[1,1]),
               round(Speed(59.91954)))
})

test_that("netspeed stops", {
  expect_error(netspeed(ps = net$ps, ffs = net$ffs, scheme = FALSE),
               "No.?\\(?")
})

test_that("netspeed works", {
  expect_equal(round(netspeed(pc_week,
                              net$ps,
                              net$ffs,
                              net$capacity,
                              net$lkm,
                              alpha = 1,
                              net = net)$S1[1]),
               round(Speed(59.91954)))
})

test_that("netspeed stops", {
  expect_equal(netspeed(ps = net$ps, ffs = net$ffs, scheme = TRUE)$V1[1],
               round(Speed(60)))
})

test_that("netspeed stops", {
  expect_equal(netspeed(ps = net$ps, ffs = net$ffs, scheme = TRUE,
                        net = net)$V1[1],
               round(Speed(60)))
})
