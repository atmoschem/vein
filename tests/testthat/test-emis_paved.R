context("emis_paved")
data(net)
net <- sf::st_as_sf(net)
veh <- matrix(pnorm(q=c(1:100), mean=500, sd = 100), nrow = 100, ncol = 24)
dim(veh)
W <- veh*1.5
lkm <-  rnorm(n = 100, mean = 10, sd = 1)
sL1 <- 0.6
adt = 1:100
test_that("emis_paved works", {
  expect_equal(round(emis_paved(veh = veh, adt = adt, lkm = lkm, k = 0.65,
                                sL1 = sL1, sL2 = sL1/4, sL3 = sL1/16, sL4 = sL1/32,
                                W = W))[[1]][1],
               Emissions(0))
  expect_equal(round(suppressWarnings(emis_paved(veh = veh, adt = adt, lkm = lkm, k = 0.65,
                                sL1 = sL1, sL2 = sL1/4, sL3 = sL1/16, sL4 = sL1/32,
                                W = W, net = net[1:10, ]))[[1]][1]),
               Emissions(0))
})


