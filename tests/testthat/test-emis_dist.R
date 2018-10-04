context("emis_dist")
data(net)
data(pc_profile)
po <- 1000
t1 <- emis_dist(gy = po, spobj = net)
t2 <- emis_dist(gy = po, spobj = net, pro = pc_profile)

test_that("emis_dist works", {
  expect_equal(as.numeric(sum(t1$emission)), 1000)
})

test_that("emis_dist works", {
  expect_equal(as.numeric(sum(sf::st_set_geometry(t2, NULL))), 1000)
})
