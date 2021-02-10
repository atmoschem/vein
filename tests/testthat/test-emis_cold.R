context("emis_cold")

data(net)
# net <- sf::st_as_sf(net)
net <- net[1,]
data(pc_profile)
data(fe2015)
data(fkm)
data(pc_cold)
pcf <- as.data.frame(cbind(pc_cold,pc_cold,pc_cold,pc_cold,pc_cold,pc_cold,
                           pc_cold))
PC_G <- c(33491,22340,24818,31808,46458,28574,24856,28972,37818,49050,87923,
          133833,138441,142682,171029,151048,115228,98664,126444,101027,
          84771,55864,36306,21079,20138,17439, 7854,2215,656,1262,476,512,
          1181, 4991, 3711, 5653, 7039, 5839, 4257,3824, 3068)
veh <- data.frame(PC_G = PC_G)
pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
pcw <- temp_fact(net$ldv+net$hdv, pc_profile[, 1])
speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)
km <- units::set_units(pckma[1:11], km)
km2 <- units::set_units(pckma[12:24], km)
cod1 <- emis_det(po = "CO", cc = "<=1400", eu = "III", km = km)
cod2 <- emis_det(po = "CO", cc = "<=1400", eu = "I", km = km2)
#vehicles newer than pre-euro
co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
lef <- ef_ldv_scaled(dfcol = cod, v = "PC", cc = "<=1400",
                     f = "G",p = "CO", eu=co1$Euro_LDV)
# Mohtly average temperature 18 Celcius degrees
lefec <- ef_ldv_cold_list(df = co1, ta = 18, cc = "<=1400", f = "G",
                          eu = co1$Euro_LDV, p = "CO" )
lefec <- c(lefec,lefec[length(lefec)], lefec[length(lefec)],
           lefec[length(lefec)], lefec[length(lefec)],
           lefec[length(lefec)])
emis_cold(veh = pc1, lkm = net$lkm, ef = lef, efcold = lefec,
          beta = pcf, speed = speed, profile = pc_profile[, 1])[5]

test_that("emis_cold works", {
  expect_equal(
    emis_cold(veh = pc1,
              lkm = net$lkm,
              ef = lef,
              efcold = lefec,
              beta = pcf,
              speed = speed,
              profile = pc_profile[, 1])[10],
    0.1012673 + 4.51e-08)

  expect_equal(
    round(emis_cold(veh = list(pc1, pc1),
                    lkm = net$lkm,
                    ef = lef,
                    efcold = lefec,
                    beta = pcf,
                    speed = speed,
                    hour = 2,
                    day = 1)[10],2),
    0.64)

  expect_error(
    emis_cold(veh = pc1,
              lkm = as.numeric(net$lkm),
              ef = lef,
              efcold = lefec,
              beta = pcf,
              speed = speed,
              profile = pc_profile[, 1])[10],
    "lkm.?\\(?")

  expect_error(
    emis_cold(veh = pc1,
              lkm = units::set_units(net$lkm, "m"),
              ef = lef,
              efcold = lefec,
              beta = pcf,
              speed = speed,
              profile = pc_profile[, 1])[10],
    "Units.?\\(?")

  expect_error(
    emis_cold(veh = pc1,
              lkm = net$lkm,
              ef = "a",
              efcold = lefec,
              beta = pcf,
              speed = speed,
              profile = pc_profile[, 1])[10],
    "ef.?\\(?")

  expect_error(
    emis_cold(veh = pc1,
              lkm = as.numeric(net$lkm),
              ef = "a",
              efcold = lefec,
              beta = pcf,
              speed = speed,
              profile = pc_profile[, 1])[10],
    ".?")

  expect_error(
    emis_cold(veh = pc1,
              lkm = units::set_units(as.numeric(net$lkm), "m"),
              ef = "a",
              efcold = lefec,
              beta = pcf,
              speed = speed,
              profile = pc_profile[, 1])[10],
    ".?")
})




netsf <- sf::st_as_sf(net)
pc1sf <- sf::st_sf(pc1, geometry = netsf$geometry)
test_that("emis_cold stops", {
  expect_equal(
    emis_cold(veh = pc1sf,
              lkm = net$lkm,
              ef = lef,
              efcold = lefec,
              beta = pcf,
              speed = speed,
              profile = pc_profile[, 1])[10],
    0.1012673 + 4.51e-08)

    expect_error(
    emis_cold(veh = pc1,
              lkm = net$lkm,
              ef = lef,
              efcold = lefec,
              beta = pcf,
              speed = speed),
    "For.?\\(?")

  expect_message(
    emis_cold(veh = pc1sf,
              lkm = net$lkm,
              ef = lef,
              efcold = lefec,
              beta = pcf,
              speed = speed,
              profile = pc_profile[, 1],
              verbose = TRUE)[10],
    "Transforming.?\\(?")
})


ncol(pc1)
length(lef)
test_that("emis_cold message", {
  expect_equal(
    round(emis_cold(veh = pc1[, 1:30],
                    lkm = net$lkm,
                    ef = lef,
                    efcold = lefec,
                    beta = pcf,
                    speed = speed,
                    profile = pc_profile[, 1])[10], 3),
    0.101)
})


test_that("emis_cold works", {
  expect_equal(
    round(emis_cold(veh = list(pc1[, 1:30], pc1[, 1:30]),
                    lkm = net$lkm,
                    ef = lef,
                    efcold = lefec,
                    beta = pcf,
                    speed = speed,
                    hour = 2,
                    day = 1)[10],2),
    0.64)
})

test_that("emis_cold message", {
  expect_message(
    emis_cold(veh = list(pc1[, 1:30], pc1[, 1:30]),
              lkm = net$lkm,
              ef = lef,
              efcold = lefec,
              beta = pcf,
              speed = speed,
              hour = 2,
              day = 1,
              verbose = TRUE)[10],
    "Number.?\\(?")
})

test_that("emis_cold message", {
  expect_message(
    emis_cold(veh = list(pc1[, 1:36], pc1[, 1:36]),
              lkm = net$lkm,
              ef = lef,
              efcold = lefec,
              beta = pcf,
              speed = speed,
              hour = 2,
              day = 1,
              verbose = TRUE)[10],
    "0.?\\(?")
})

netsf <- sf::st_as_sf(net)
pc1sf <- sf::st_sf(pc1, geometry = netsf$geometry)
test_that("emis_cold stops", {
  expect_equal(
    round(emis_cold(veh = pc1sf,
              lkm = net$lkm,
              ef = lef,
              efcold = lefec,
              beta = pcf,
              speed = speed,
              profile = pc_profile[, 1],
              simplify = TRUE)[10]),
    0)
})
