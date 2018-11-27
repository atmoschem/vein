context("test-netspeed")
data(net)
data(pc_profile)
pc_week <- temp_fact(net$ldv + net$hdv, pc_profile)

test_that("multiplication works", {
  expect_equal(round(netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)[1,1]),
               round(Speed(59.91954)))
})
