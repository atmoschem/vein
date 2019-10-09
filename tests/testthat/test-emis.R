context("emis")
data(net)
data(pc_profile)
data(profiles)
data(fe2015)
data(fkm)
PC_G <- c(33491,22340,24818,31808,46458,28574,24856,28972,37818,49050,87923,
          133833,138441,142682,171029,151048,115228,98664,126444,101027,
          84771,55864,36306,21079,20138,17439, 7854,2215,656,1262,476,512,
          1181, 4991, 3711, 5653, 7039, 5839, 4257,3824, 3068)
veh <- data.frame(PC_G = PC_G)
pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
pcw <- temp_fact(net$ldv+net$hdv, pc_profile)
speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
lef <- EmissionFactorsList(fe2015[fe2015$Pollutant=="CO", "PC_G"])

test_that("emis stops", {
  expect_error(round(emis(veh = pc1,
                          lkm = as.numeric(net$lkm),
                          ef = lef,
                          speed = speed,
                          profile = profiles$PC_JUNE_2014)[1], 2),
               "lkm.?\\(?")
})


test_that("emis works", {
  expect_error(round(emis(veh = pc1,
                          lkm = units::set_units(net$lkm, "m"),
                          ef = lef,
                          speed = speed,
                          profile = profiles$PC_JUNE_2014)[1], 2),
               "Units.?\\(?")
})

test_that("emis works", {
  expect_error(round(emis(veh = pc1,
                          lkm = net$lkm,
                          ef = lef,
                          profile = profiles$PC_JUNE_2014)[1], 2),
               "Add.?\\(?")
})

test_that("emis works", {
  expect_error(round(emis(veh = pc1,
                          lkm = net$lkm,
                          ef = "le",
                          speed = speed,
                          profile = profiles$PC_JUNE_2014)[1], 2),
               "ef.?\\(?")
})

netsf <- sf::st_as_sf(net)
pc1sf <- sf::st_sf(pc1, geometry = netsf$geometry)
test_that("emis works", {
  expect_equal(round(emis(veh = pc1sf,
                          lkm = net$lkm,
                          ef = lef,
                          speed = speed,
                          profile = profiles$PC_JUNE_2014)[1], 2),
               1.2)
})

test_that("emis message", {
  expect_message(emis(veh = pc1sf,
                      lkm = net$lkm,
                      ef = lef,
                      speed = speed,
                      profile = profiles$PC_JUNE_2014,
                      verbose = TRUE),
                 "Conv.?\\(?")
})

test_that("emis warns", {
  expect_warning(emis(veh = pc1,
                      lkm = net$lkm,
                      ef = lef,
                      speed = speed,
                      profile = profiles$PC_JUNE_2014,
                      hour = 24)[1],
                 "Argu?")
})




test_that("emis message", {
  expect_message(emis(veh = pc1,
                      lkm = net$lkm,
                      ef = EmissionFactors(3),
                      verbose = TRUE),
                 "If.?\\(?")
})

nrow(pc1)
length(net$lkm)
test_that("emis stops", {
  expect_error(emis(veh = pc1,
                    lkm = net$lkm[1],
                    ef = EmissionFactors(3),
                    verbose = TRUE),
               "Number.?\\(?")
})

test_that("emis works", {
  expect_equal(round(emis(veh = pc1,
                          lkm = net$lkm,
                          ef = EmissionFactors(3))[1,1]),
               Emissions(81))
})


test_that("emis stops", {
  expect_error(emis(veh = pc1[1:3, ],
                    lkm = net$lkm,
                    ef = EmissionFactorsList(1:41),
                    speed = speed$S1,
                    verbose = TRUE),
               "Number.?\\(?")
})

test_that("emis stops", {
  expect_error(emis(veh = pc1,
                    lkm = net$lkm,
                    ef = EmissionFactorsList(1:4),
                    speed = speed$S1[1],
                    verbose = TRUE),
               "Number.?\\(?")
})


test_that("emis stops", {
  expect_error(emis(veh = pc1,
                    lkm = net$lkm,
                    ef = EmissionFactorsList(1:4)),
               "Add.?\\(?")
})

test_that("emis works", {
  expect_equal(round(emis(veh = pc1,
                    lkm = net$lkm,
                    ef = EmissionFactorsList(1:nrow(pc1)),
                    speed = speed$S1)$V1[1]),
               Emissions(27))
})


test_that("emis works", {
  expect_equal(round(emis(veh = pc1,
                          lkm = net$lkm,
                          ef = lef,
                          speed = speed,
                          profile = profiles$PC_JUNE_2014$Monday)[1], 2),
               1.2)
})


test_that("emis works", {
  expect_equal(round(emis(veh = pc1[, 1:2],
                          lkm = net$lkm,
                          ef = lef,
                          speed = speed,
                          profile = profiles$PC_JUNE_2014$Monday)[1], 2),
               1.2)
})

test_that("emis works", {
  expect_equal(round(emis(veh = list(pc1, pc1),
                          lkm = net$lkm,
                          ef = lef,
                          speed = speed,
                          profile = profiles$PC_JUNE_2014$Monday)[1], 2),
               4.16)
})

test_that("emis works", {
  expect_equal(round(emis(veh = list(pc1[, 1:2], pc1[, 1:2]),
                          lkm = net$lkm,
                          ef = lef,
                          speed = speed,
                          profile = profiles$PC_JUNE_2014$Monday)[1], 2),
               4.16)
})
test_that("emis message", {
  expect_message(emis(veh = list(pc1[, 1:2], pc1[, 1:2]),
                          lkm = net$lkm,
                          ef = lef,
                          speed = speed,
                          profile = profiles$PC_JUNE_2014$Monday,
                          verbose = TRUE),
                 "Number.?\\(?")
})
