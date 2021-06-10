context("split_emis")
data(net)
g <- make_grid(net, 1/102.47/2) #500m in degrees
netsf <- sf::st_as_sf(net)[, "ldv"]
x <- split_emis(netsf, g)
dim(x)

test_that("split_emis works", {
  expect_equal(round(as.numeric(sum(x$ldv))), 5638228)
})

g$A <- rep(letters, length = 20)[1:nrow(g)]
g$B <- rev(g$A)
netsf <- sf::st_as_sf(net)[, c("ldv", "hdv")]

test_that("split_emis works", {
  expect_equal(round(as.numeric(sum(split_emis(netsf,
                                         g,
                                         add_column = c("A", "B"))$ldv
                              ))), 5638228)
})


test_that("split_emis message", {
  expect_equal(as.numeric(split_emis(netsf, 1, verbose = TRUE)$ldv[1]),
                 4350)
})

