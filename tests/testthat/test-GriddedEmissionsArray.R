context("GriddedEmissionsArray")

data(net)
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

a <- capture_output_lines(GriddedEmissionsArray(E_CO_g,rows = 19,
                                                cols = 23,
                                                times = 168,
                                                T))
test_that("GriddedEmissionsArray works", {
  expect_equal(round(GriddedEmissionsArray(E_CO_g,
                                           rows = 19,
                                           cols = 23,
                                           times = 168,
                                           T)[1]),
               10785)
})



test_that("GriddedEmissionsArray works", {
  expect_equal(capture_output_lines(GriddedEmissionsArray(E_CO_g,
                                                          rows = 19,
                                                          cols = 23,
                                                          times = 168,
                                                          T)),
               a)
})


test_that("GriddedEmissionsArray works", {
  expect_equal(round(GriddedEmissionsArray(sf::as_Spatial(E_CO_STREETS),
                                           rows = 19,
                                           cols = 23,
                                           times = 168,
                                           T)[1]),
               688)
})


test_that("GriddedEmissionsArray works", {
  expect_equal(round(GriddedEmissionsArray(sf::st_set_geometry(E_CO_STREETS, NULL),
                                           rows = 19,
                                           cols = 23,
                                           times = 168,
                                           T)[1]),
               688)
})


test_that("GriddedEmissionsArray works", {
  expect_equal(round(head(GriddedEmissionsArray(E_CO_g,
                                                rows = 19,
                                                cols = 23,
                                                times = 168,
                                                T))[1]),
               10785)
})


test_that("GriddedEmissionsArray works", {
  expect_equal(round(summary(GriddedEmissionsArray(E_CO_g,
                                                   rows = 19,
                                                   cols = 23,
                                                   times = 168,
                                                   TRUE))[1]),
               summary(0)[1])
})

test_that("GriddedEmissionsArray works", {
  expect_equal(plot(GriddedEmissionsArray(E_CO_g,
                                          rows = 19,
                                          cols = 23,
                                          times = 168))$mfrow[1],
               1)
})


test_that("GriddedEmissionsArray works", {
  expect_equal(round(GriddedEmissionsArray(E_CO_g,
                                           rows = 19,
                                           cols = 23,
                                           times = 168, rotate = TRUE)[1]),
               337)
})

test_that("GriddedEmissionsArray works", {
  expect_equal(print(round(GriddedEmissionsArray(E_CO_g,
                                             rows = 19,
                                             cols = 23,
                                             times = 168)[1])),
                 10785)
})

test_that("GriddedEmissionsArray prints", {
  a <- GriddedEmissionsArray(E_CO_g,
                        rows = 19,
                        cols = 23,
                        times = 168)
  expect_output(print(a), "This GriddedEmissionsArray has:?")
  expect_equal(print(round(a[1])), 10785)
})

