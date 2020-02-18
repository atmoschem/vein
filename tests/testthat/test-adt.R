context("adt")
data(net)
data(pc_profile)
p1 <- pc_profile[, 1]
adt1 <- adt(pc = net$ldv*0.75,
            lcv = net$ldv*0.1,
            hgv = net$hdv,
            bus = net$hdv*0.1,
            mc = net$ldv*0.15,
            p_pc = p1,
            p_lcv = p1,
            p_hgv = p1,
            p_bus = p1,
            p_mc = p1)

test_that("adt works", {
  expect_equal(round(adt1[1]),
               Vehicles(62445))
})

