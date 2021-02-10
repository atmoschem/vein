context("emis_dist")

data(net)
data(pc_profile)
po <- 1000
t1 <- emis_dist(gy = po, spobj = net)
t2 <- emis_dist(gy = po, spobj = net, pro = pc_profile)

test_that("emis_dist works", {
  expect_equal(as.numeric(sum(t1$emission)), 1000)
  expect_output(emis_dist(gy = po, spobj = net, pro = pc_profile, verbose = TRUE),"C.?")
  expect_message(emis_dist(gy = po, spobj = net, verbose = TRUE),
                 "S.?")
  expect_output(emis_dist(gy = po, spobj = net, verbose = TRUE),
                "C.?")
  expect_equal(as.numeric(sum(sf::st_set_geometry(t2, NULL))), 1000)
  expect_error(emis_dist(gy = po, spobj = suppressWarnings(sf::st_centroid(sf::st_as_sf(net)))),
               "C.?")
})



net$highway <- c("motorway",
                 "trunk",
                 "primary",
                 "secondary",
                 "tertiary")[round(runif(n = nrow(net), min = 1,max = 5))]

test_that("emis_dist works", {
  net$highway <- NULL
  expect_error(emis_dist(gy = po, spobj = net, osm = c(5, 3, 2, 1, 1),pro = pc_profile),
               "N.?")
  net$highway <- c("motorway",
                   "trunk",
                   "primary",
                   "secondary")[round(runif(n = nrow(net), min = 1,max = 4))]
  expect_output(emis_dist(gy = po, spobj = net, osm = c(5, 3, 2, 1, 1), pro = pc_profile,
                          verbose = TRUE),
                "S.?")
  expect_equal(round(emis_dist(gy = po,
                               spobj = net,
                               osm = c(5, 3, 2, 1, 1),
                               pro = pc_profile)$V1[1]),
               0)
})


net$highway <- c("motorway",
                 "trunk",
                 "primary",
                 "secondary",
                 "tertiary")[round(runif(n = nrow(net), min = 1,max = 5))]

test_that("emis_dist works", {
  expect_equal(round(mean(emis_dist(gy = po,
                               spobj = net,
                               osm = c(5, 3, 2, 1, 1))$emission, 1)),
               0)
  expect_message(emis_dist(gy = po,
                           spobj = net,
                           osm = c(5, 3, 2, 1, 1),
                           verbose = TRUE),
                 "S.?")
})

net$highway <- c("motorway",
                 "trunk",
                 "primary",
                 "secondary",
                 "tertiary")[round(runif(n = nrow(net), min = 1,max = 5))]
# names(net)[length(names(net))] <- "lala"
net$highway <- NULL
test_that("emis_dist works", {
  expect_error(emis_dist(gy = po,
                         spobj = net,
                         osm = c(5, 3, 2, 1, 1)),
               "N.?")
})


