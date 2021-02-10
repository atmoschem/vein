context("ef_wear")
data(net)
data(pc_profile)
pc_week <- temp_fact(net$ldv[1], matrix(1))
df <- netspeed(pc_week, net$ps[1], net$ffs[1], net$capacity[1], net$lkm[1], alpha = 1)
as.numeric(ef_wear(wear = "tyre", type = "PC", pol = "PM10", speed = df))

test_that("ef_wear works", {
  expect_equal(as.numeric(ef_wear(wear = "tyre", type = "PC", pol = "PM10",
                                  speed = df)),
               0.00579084)
})

test_that("ef_wear works", {
  expect_equal(as.numeric(ef_wear(wear = "tyre", type = "PC", pol = "PM10",
                                  speed = unlist(df))),
               0.00579084)
})


test_that("ef_wear works", {
  expect_equal(as.numeric(ef_wear(wear = "break", type = "PC", pol = "PM10",
                                  speed = df)),
               0.00135975)
})

test_that("ef_wear works", {
  expect_equal(as.numeric(ef_wear(wear = "road", type = "PC", pol = "PM10",
                                  speed = df)),
               0.0075)
})
