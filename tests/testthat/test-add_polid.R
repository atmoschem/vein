context("add_polid")

data(net)
nets <- sf::st_as_sf(net)
bb <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(nets)))
bb$id <- "a"
a <- add_polid(polyg = bb, street = nets, by = "id")

test_that("emis_merge works", {
  expect_equal(add_polid(polyg = bb, street = nets, by = "id")$id[1],
                "a")
})

