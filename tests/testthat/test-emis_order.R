context("emis_order")

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
E_CO <- emis(veh = pc1,
             lkm = net$lkm,
             ef = lef,
             speed = speed,
             profile = profiles$PC_JUNE_2014)
df <- emis_post(arra = E_CO, by = "streets", net = sf::st_as_sf(net))
test_that("emis_order works", {
  expect_equal(sf::st_set_geometry(emis_order(df,
                                              start = "sat",
                                              hours = 190,
                                              verbose = FALSE), NULL)[1][1, ],
               1)
})


test_that("emis_order message", {
  expect_message(emis_order(df,
                          start = "sat",
                          hours = 190,
                          verbose = TRUE)$V1,
                 "Class.?\\(?")
})

test_that("emis_order works", {
  expect_equal(round(emis_order(df,
                                start = "fri",
                                hours = 241)$V1[1]),
               986)
})

test_that("emis_order works", {
  expect_equal(round(emis_order(df,
                            start = as.Date("2016-04-06"),
                            hours = 241)$V1[1]),
                 986)
})

test_that("emis_order works", {
  expect_equal(round(emis_order(df,
                                start = as.Date("2016-04-07"),
                                hours = 241)$V1[1]),
               986)
})

test_that("emis_order works", {
  expect_equal(round(emis_order(df,
                                start = as.Date("2016-04-08"),
                                hours = 241)$V1[1]),
               986)
})

test_that("emis_order works", {
  expect_equal(round(emis_order(df,
                                start = as.Date("2016-04-09"),
                                hours = 241)$V1[1]),
               986)
})

test_that("emis_order works", {
  expect_equal(round(emis_order(df,
                                start = as.Date("2016-04-10"),
                                hours = 241)$V1[1]),
               986)
})

test_that("emis_order works", {
  expect_equal(round(emis_order(df,
                                start = as.Date("2016-04-11"),
                                hours = 241)$V1[1]),
               986)
})

test_that("emis_order works", {
  expect_equal(round(emis_order(df,
                                start = as.Date("2016-04-12"),
                                hours = 241)$V1[1]),
               986)
})

test_that("emis_order works", {
  expect_equal(round(emis_order(df,
                                start = as.Date("2016-04-13"),
                                hours = 241)$V1[1]),
               986)
})


test_that("emis_order works", {
  expect_equal(round(emis_order(df,
                                start = as.Date("2016-04-13"),
                                utc = -5,
                                hours = 241)$V1[1]),
               986)
})


test_that("emis_order works", {
  expect_equal(round(emis_order(df,
                                start = as.Date("2016-04-13"),
                                utc = 3,
                                hours = 70)$V70[1]),
               2307)
})


test_that("emis_order works", {
  expect_error(emis_order(E_CO,
                                start = as.Date("2016-04-13"),
                                hours = 241),
               "Please.?\\(?")
})

E_CO_STREETS <- emis_post(arra = E_CO, pollutant = "CO", by = "streets_wide")
net@data <- cbind(net@data, E_CO_STREETS)
head(net@data)
g <- make_grid(net, 1/102.47/2, 1/102.47/2) #500m in degrees
net@data <- net@data[,- c(1:9)]
names(net)
E_CO_g <- emis_grid(spobj = net, g = g, sr= 31983)
gr <- GriddedEmissionsArray(E_CO_g, rows = 19, cols = 23, times = 168, T)

test_that("emis_order works", {
  expect_equal(round(emis_order(gr,
                                start = as.Date("2016-04-13"),
                                utc = 3,
                                hours = 70)[1]),
               1017)
})

test_that("emis_order works", {
  expect_equal(round(emis_order(E_CO_g,
                                start = as.Date("2016-04-13"),
                                utc = 3,
                                hours = 70)$V70[1]),
               8485)
})

test_that("emis_order works", {
  expect_equal(round(emis_order(as.matrix(sf::st_set_geometry(E_CO_g, NULL)),
                                start = as.Date("2016-04-13"),
                                utc = 3,
                                hours = 70)$V70[1]),
               8485)
})

test_that("emis_order works", {
  expect_equal(round(emis_order(sf::as_Spatial(E_CO_g),
                                start = as.Date("2016-04-13"),
                                utc = 3,
                                hours = 70)$V70[1]),
               8485)
})

test_that("emis_order works", {
  expect_equal(round(emis_order(sf::st_set_geometry(E_CO_g, NULL),
                                start = "fri",
                                utc = 3,
                                hours = 70)$V70[1]),
               8485)
})
