context("Emissions")

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
lef <- EmissionFactorsList(as.numeric(ef_cetesb("CO", "PC_G")))

test_that("Emissions works", {
  expect_equal(Emissions(emis(veh = pc1[1:5, ],
                               lkm = net$lkm[1:5],
                               ef = lef,
                               profile = pc_profile[1, ],
                               array = T)[,,1,1])[1,1],
               1.039899 + 2.63e-07)
})
