context("emis")
data(net)
data(pc_profile)
data(fe2015)
lef <- EmissionFactorsList(fe2015[fe2015$Pollutant=="CO", "PC_G"])
pc_week <- temp_fact(net$ldv[1] + net$hdv[1], pc_profile[, 1])
df <- netspeed(pc_week, net$ps[1], net$ffs[1],
               net$capacity[1], net$lkm[1], alpha = 1)

test_that("emis works", {
  expect_equal(emis(veh = age_ldv(net$ldv[1]),
                    lkm = net$lkm[1], ef = lef, speed = df,
                    profile = pc_profile[1])[1],
               0.5375336)
})
