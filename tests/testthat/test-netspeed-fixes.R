context("netspeed fixes")

data(net)
q <- data.frame(h1 = c(100, 100, 0), h2 = c(200, 200, 0))

test_that("netspeed returns NA and warns for cap <= 0 or lkm <= 0", {
  w <- character(0)
  x <- withCallingHandlers(
    netspeed(q, ps = 30, ffs = c(60, 60, 60), cap = c(1000, 0, 1000),
             lkm = c(1, 1, 0)),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })
  expect_true(any(grepl("cap", w)))
  expect_true(any(grepl("lkm", w)))
  x <- remove_units(x)
  expect_true(is.na(x$S1[2]) && is.na(x$S1[3]))
  expect_equal(x$S1[1], 60 / (1 + 0.15 * (100 / 1000)^4))
})

test_that("netspeed BPR does not depend on lkm", {
  x <- netspeed(q, ps = 30, ffs = 60, cap = 1000, lkm = c(1, 2, 3))
  expect_equal(as.numeric(x$S1), 60 / (1 + 0.15 * (c(100, 100, 0) / 1000)^4))
})

test_that("netspeed accepts a vector q", {
  x <- netspeed(q = c(100, 200), ps = c(30, 30), ffs = c(60, 60),
                cap = c(1000, 1000), lkm = c(1, 1))
  expect_equal(dim(x), c(2, 1))
  expect_equal(names(x), "S1")
})

test_that("netspeed converts units of ffs and ps", {
  ffs <- units::set_units(units::set_units(c(60, 60), "km/h"), "miles/h")
  ps <- units::set_units(units::set_units(c(30, 30), "km/h"), "miles/h")
  x <- netspeed(q[1:2, ], ps, ffs, cap = c(1000, 1000), lkm = c(1, 1))
  expect_equal(as.character(units(x$S1)), "km/h")
  expect_equal(as.numeric(x$S1), 60 / (1 + 0.15 * (100 / 1000)^4) * c(1, 1))
  xm <- netspeed(q[1:2, ], ps, ffs, cap = c(1000, 1000), lkm = c(1, 1),
                 dist = "miles")
  expect_equal(as.character(units(xm$S1)), "miles/h")
  expect_equal(as.numeric(units::set_units(xm$S1, "km/h")), as.numeric(x$S1))
  s <- netspeed(ps = ps, ffs = ffs, scheme = TRUE)
  expect_equal(as.numeric(s$S1), c(60, 60))
})

test_that("netspeed scheme follows the documented hours", {
  s <- netspeed(ps = 20, ffs = 60, scheme = TRUE)
  expect_equal(dim(s), c(1, 24))
  expect_equal(names(s), paste0("S", 1:24))
  expect_equal(unname(unlist(remove_units(s))),
               c(rep(60, 6), 40, rep(20, 3), rep(40, 7), rep(20, 3),
                 rep(40, 2), rep(60, 2)))
  s <- netspeed(ps = net$ps, ffs = net$ffs, scheme = TRUE, net = net)
  expect_equal(dim(sf::st_drop_geometry(s)), c(nrow(net), 24))
})
