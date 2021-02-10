context("EmissionFactorsList")

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
a <- EmissionFactorsList(as.numeric(ef_cetesb("CO", "PC_G")))
print(a, default = T)
summary(a)

test_that("EmissionFactorsList works", {
  expect_equal(EmissionFactorsList(as.numeric(ef_cetesb("CO_0km", "PC_G")))[[1]](),
               0.1612112)
  expect_output(print(EmissionFactorsList(as.numeric(ef_cetesb("CO", "PC_G")))),
                ".?")

  expect_equal(EmissionFactorsList(ef_cetesb("CO_0km", c("PC_G", "LCV_G"))
  )[[1]][[1]](),
  EmissionFactors(0.1612112))

  expect_output(print(EmissionFactorsList(ef_cetesb("CO", c("PC_G", "LCV_G")))),
  ".?")
  expect_output(summary(EmissionFactorsList(ef_cetesb("CO", c("PC_G", "LCV_G")))),
                ".?")

  expect_output(plot(EmissionFactorsList(ef_cetesb("CO", c("PC_G", "LCV_G")))),
                ".?")

  expect_equal(plot(EmissionFactorsList(as.numeric(ef_cetesb("CO", "PC_G")))),
               NULL)
})

