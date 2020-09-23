context("GriddedEmissionsArray")

data(net)
net <- sf::st_as_sf(net)[1:10, ]
data(pc_profile)
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
pckm <- units::set_units(fkm[[1]](1:24), "km")
pckma <- cumsum(pckm)
cod1 <- emis_det(po = "CO", cc = 1000, eu = "III", km = pckma[1:11])
cod2 <- emis_det(po = "CO", cc = 1000, eu = "I", km = pckma[12:24])
#vehicles newer than pre-euro
co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
lef <- ef_ldv_scaled(co1, cod, v = "PC", t = "4S", cc = "<=1400",
                     f = "G",p = "CO", eu=co1$Euro_LDV)
E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed, agemax = 41,
             profile = pc_profile, simplify = T)
E_CO_STREETS <- emis_post(arra = E_CO, pollutant = "CO", by = "streets", net = net)
g <- make_grid(net, 1/102.47/2, 1/102.47/2) #500m in degrees
names(net)
E_CO_g <- emis_grid(spobj = E_CO_STREETS["V1"], g = g, sr= 31983)

a <- capture_output_lines(GriddedEmissionsArray(E_CO_g,
                                                rows = 19,
                                                cols = 16,
                                                times = 1))
test_that("GriddedEmissionsArray works", {
  expect_equal(round(GriddedEmissionsArray(E_CO_g,
                                           rows = 19,
                                           cols = 16,
                                           times = 1)[1]),
               18)
})



test_that("GriddedEmissionsArray works", {
  expect_error(GriddedEmissionsArray(E_CO_g,
                                     rows = 19,
                                     cols = 16,
                                     times = 168,
                                     "left"),
               ".?")
})

test_that("GriddedEmissionsArray works", {
  expect_equal(plot(GriddedEmissionsArray(E_CO_g,
                                          rows = 19,
                                          cols = 16,
                                          times = 1,
                                          flip = FALSE))$mfrow[1],
               1)
})


test_that("GriddedEmissionsArray works", {
  expect_equal(round(GriddedEmissionsArray(E_CO_g,
                                           rows = 16,
                                           cols = 19,
                                           times = 1, rotate = "left")[1]),
               0)
})

test_that("GriddedEmissionsArray works", {
  expect_equal(round(GriddedEmissionsArray(E_CO_g,
                                           rows = 16,
                                           cols = 19,
                                           times = 1, rotate = "right")[1]),
               0)
})

