context("emis_cold_td")
name = file.path(tempdir(), "YourCity")
inventory(name = name)

test_that("emis_cold works", {
  expect_message(inventory(name = name, ),
                "P.?")
  expect_message(inventory(name = name, clear = FALSE),
                 "P.?")
  expect_warning(inventory(name = name, clear = FALSE),
                 ".?")
  expect_message(inventory(name = name, scripts = FALSE),
                 "n.?")
  expect_message(inventory(name = name,
                           vehcomp = c(PC = 0, LCV = 1, HGV = 1, BUS = 1,
                                                   MC = 1)),
                 "n.?")
  expect_message(inventory(name = name,
                           vehcomp = c(PC = 1, LCV = 0, HGV = 1, BUS = 1,
                                       MC = 1)),
                 "n.?")
  expect_message(inventory(name = name,
                           vehcomp = c(PC = 1, LCV = 1, HGV = 0, BUS = 1,
                                       MC = 1)),
                 "n.?")
  expect_message(inventory(name = name,
                           vehcomp = c(PC = 1, LCV = 1, HGV = 1, BUS = 0,
                                       MC = 1)),
                 "n.?")
  expect_message(inventory(name = name,
                           vehcomp = c(PC = 1, LCV = 1, HGV = 1, BUS = 1,
                                       MC = 0)),
                 "n.?")
  expect_output(inventory(name = name, show.dir = TRUE),
                 "D.?")
})

