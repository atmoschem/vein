context("emis_to_streets fixes")

data(net)
nsf <- sf::st_as_sf(net)

test_that("emis_to_streets keeps the order of streets with several IDs", {
  d <- nsf[1:4, "ldv"]
  d$ID <- c("A", "B", "A", "B")
  L <- as.numeric(sf::st_length(d))
  dfe <- data.frame(CO = c(100, 1000), BC = c(1, 10), ID = c("A", "B"))
  r <- suppressMessages(emis_to_streets(d, dfe))
  expect_equal(r$ID, d$ID)
  expect_equal(r$ldv, d$ldv)
  expect_true(all(sf::st_equals(r, d, sparse = FALSE)[cbind(1:4, 1:4)]))
  expect_equal(r$CO, ifelse(d$ID == "A", 100 * L / sum(L[d$ID == "A"]),
    1000 * L / sum(L[d$ID == "B"])))
  expect_equal(r$BC, ifelse(d$ID == "A", 1 * L / sum(L[d$ID == "A"]),
    10 * L / sum(L[d$ID == "B"])))
})

test_that("emis_to_streets keeps the names of dfemis columns", {
  d1 <- nsf[1:3, "ldv"]
  d1$ID <- "A"
  r1 <- suppressMessages(emis_to_streets(d1, data.frame(CO = 1000, BC = 1, ID = "A")))
  expect_equal(sum(r1$CO), 1000)
  expect_equal(sum(r1$BC), 1)
  r2 <- suppressMessages(emis_to_streets(d1, data.frame(ID = "A", CO = 1000, BC = 1)))
  expect_equal(sum(r2$CO), 1000)
})

test_that("emis_to_streets with stpro keeps the order of streets", {
  d <- nsf[1:4, "ldv"]
  d$ID <- "A"
  d$stpro <- c("z", "a", "z", "a")
  L <- as.numeric(sf::st_length(d))
  r <- suppressMessages(emis_to_streets(d, data.frame(BC = 1, ID = "A"),
    stpro = data.frame(stpro = c("a", "z"), VAL = c(1, 3))))
  expect_equal(r$ldv, d$ldv)
  expect_equal(r$stpro, d$stpro)
  w <- L * ifelse(d$stpro == "z", 3, 1)
  expect_equal(r$BC, w / sum(w))
})

test_that("emis_to_streets keeps streets without emissions", {
  d3 <- nsf[1:3, "ldv"]
  d3$ID <- c("A", "A", "Z")
  r <- expect_warning(suppressMessages(emis_to_streets(d3, data.frame(BC = 1, ID = "A"))),
    "not present")
  expect_equal(nrow(r), 3)
  expect_equal(r$BC[3], 0)
  expect_equal(sum(r$BC), 1)
})

test_that("emis_to_streets keeps units of dfemis", {
  d1 <- nsf[1:3, "ldv"]
  d1$ID <- "A"
  r <- suppressMessages(emis_to_streets(d1, data.frame(CO = Emissions(10), ID = "A")))
  expect_true(inherits(r$CO, "units"))
  expect_equal(sum(as.numeric(r$CO)), 10)
})
