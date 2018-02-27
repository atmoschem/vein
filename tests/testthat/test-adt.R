context("adt")
data(net)
net@data <- head(net@data)
data(pc_profile)
p1 <- pc_profile[, 1]

test_that("adt works", {
  expect_equal(adt(pc = net$ldv*0.75,
                   lcv = net$ldv*0.1,
                   hgv = net$hdv,
                   bus = 0,
                   mc = net$ldv*0.15,
                   p_pc = p1,
                   p_lcv = p1,
                   p_hgv = p1,
                   p_bus = p1,
                   p_mc = p1), c(64046.1649,
                                 22659.0914,
                                 8730.8910,
                                 12411.7051,
                                 809.7791,
                                 42285.1921)*units::as_units("d-1"))
})
