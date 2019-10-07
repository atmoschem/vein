context("emis_wear")
data(net)
data(pc_profile)
pc_week <- temp_fact(net$ldv+net$hdv, pc_profile[, 1])
df <- netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
ef <- ef_wear(wear = "tyre", type = "PC", pol = "PM10", speed = df)

test_that("emis_wear works", {
  expect_equal(emis_wear(veh = age_ldv(net$ldv, name = "VEH"),
                         lkm = net$lkm, ef = ef, speed = df,
                         profile = pc_profile[, 1])[1],
               0.03503776)
})

test_that("emis_wear works", {
  expect_equal(emis_wear(veh = age_ldv(net$ldv, name = "VEH"),
                         lkm = net$lkm, ef = ef, speed = df,
                         what = "break",
                         profile = pc_profile[, 1])[1],
               0.03702493)
})

test_that("emis_wear works", {
  expect_equal(emis_wear(veh = age_ldv(net$ldv, name = "VEH"),
                         lkm = net$lkm, ef = ef, speed = df,
                         what = "road",
                         profile = pc_profile[, 1])[1],
               0.03094737)
})

test_that("emis_wear error", {
  expect_error(emis_wear(veh = age_ldv(net$ldv, name = "VEH"),
                         lkm = units::set_units(net$lkm, "m"), ef = ef, speed = df,
                         what = "road",
                         profile = pc_profile[, 1])[1],
               "Units.?\\(?")
})

ef2 <- ef
ef2$V25 <- ef$V24
test_that("emis_wear error", {
  expect_error(emis_wear(veh = age_ldv(net$ldv, name = "VEH"),
                         lkm = net$lkm, ef = ef2, speed = df,
                         what = "road",
                         profile = pc_profile[, 1])[1],
               "Number.?\\(?")
})
