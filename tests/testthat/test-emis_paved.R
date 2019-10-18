context("emis_paved")

veh <- array(pnorm(q=c(1:100), mean=500, sd = 100),
             c(100,24,7))
W <- veh*1e+05
lkm <-  rnorm(n = 100, mean = 10, sd = 1)
sL1 <- 0.6

test_that("emis_paved works", {
  expect_equal(round(emis_paved(veh = veh, lkm = lkm, k = 0.65,
                                sL1 = sL1, sL2 = sL1/4, sL3 = sL1/16, sL4 = sL1/32,
                                W = W)[1]),
               0)
  expect_output(print(emis_paved(veh = veh, lkm = lkm, k = 0.65,
                                sL1 = sL1, sL2 = sL1/4, sL3 = sL1/16, sL4 = sL1/32,
                                W = W)),
               "T.?")
})


