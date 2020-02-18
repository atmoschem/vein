context("aw")

data(net)
data(pc_profile)
p1 <- pc_profile[, 1]

test_that("aw works", {
  aw1 <- aw(pc = net$ldv*0.75,
            lcv = net$ldv*0.1,
            hgv = net$hdv,
            bus = net$hdv*0.1,
            mc = net$ldv*0.15,
            p_pc = p1,
            p_lcv = p1,
            p_hgv = p1,
            p_bus = p1,
            p_mc = p1)
  expect_equal(round(aw1$V1[1]), 1)
})


