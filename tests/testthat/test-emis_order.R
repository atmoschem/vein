context("emis_order")

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
# Estimation for morning rush hour and local emission factors
speed <- data.frame(S8 = net$ps)

lef <- EmissionFactorsList(fe2015[fe2015$Pollutant=="CO", "PC_G"])
E_CO <- emis(veh = pc1[1:2, ],lkm = net$lkm[1:2], ef = lef, speed = speed[1:2, ],
             profile = pc_profile)
df <- emis_post(arra = E_CO, by = "streets", net = sf::st_as_sf(net)[1:2, ])
test_that("emis_order works", {
  expect_equal(sf::st_set_geometry(emis_order(df,
                                               start = "sat",
                                               hours = 190,
                                               verbose = FALSE), NULL)[1][1, ],
               1)
})
