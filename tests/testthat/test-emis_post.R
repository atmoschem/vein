context("emis_post")

data(net)
net <- sf::st_as_sf(net)[1:10, ]
data(pc_profile)
data(fe2015)
data(fkm)
PC_G <- c(33491,22340,24818,31808,46458,28574,24856,28972,37818,49050,87923,
          133833,138441,142682,171029,151048,115228,98664,126444,101027,
          84771,55864,36306,21079,20138,17439, 7854,2215,656,1262,476,512,
          1181, 4991, 3711, 5653, 7039, 5839, 4257,3824, 3068)
pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
# Estimation for morning rush hour and local emission factors
speed <- data.frame(S8 = net$ps)
p1h <- matrix(1)
lef <- EmissionFactorsList(fe2015[fe2015$Pollutant=="CO", "PC_G"])
# 4d
E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed,
             profile = pc_profile)

test_that("emis_post works", {
  expect_equal(round(emis_post(arra = E_CO,
                               pollutant = "CO",
                               by = "streets_wide")[1,1]),
               Emissions(542))
})

test_that("emis_post stops", {
  expect_error(emis_post(arra = 1,
                         pollutant = "CO",
                         by = "streets"),
               ".?")
})

test_that("emis_post works", {
  expect_equal(round(emis_post(arra = E_CO,
                               pollutant = "CO",
                               veh = "a",
                               size = "s",
                               fuel = "d",
                               type_emi = "exhaust",
                               by = "veh")$g[1]),
               Emissions(2))
})

test_that("emis_post works", {
  expect_equal(round(emis_post(arra = E_CO,
                               pollutant = "CO",
                               by = "streets_narrow")$g[1]),
               Emissions(542))
})

# 3d
E_CO <- emis(veh = pc1,
             lkm = net$lkm,
             ef = lef,
             speed = speed,
             profile = pc_profile,
             simplify = TRUE)
test_that("emis_post works", {
  expect_equal(round(emis_post(arra = E_CO,
                               pollutant = "CO",
                               by = "streets_wide")[1,1]),
               Emissions(542))
})

test_that("emis_post stops", {
  expect_error(emis_post(arra = 1,
                         pollutant = "CO",
                         by = "streets"),
               ".?")
})

test_that("emis_post works", {
  expect_equal(round(emis_post(arra = E_CO,
                               pollutant = "CO",
                               veh = "a",
                               size = "s",
                               fuel = "d",
                               type_emi = "exhaust",
                               by = "veh")$g[1]),
               Emissions(2))
})

test_that("emis_post works", {
  expect_equal(round(emis_post(arra = E_CO,
                               pollutant = "CO",
                               by = "streets_narrow")$g[1]),
               Emissions(542))
})

