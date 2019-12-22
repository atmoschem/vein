context("inventory")
name = file.path(tempdir(), "YourCity")
inventory(name = name, showWarnings = FALSE, show.scripts = TRUE)
inventory(name = name, showWarnings = FALSE, rush.hour = TRUE)
inventory(name = name,
          vehcomp = c(PC = 11, LCV = 11, HGV = 11, BUS = 11,
                      MC = 11),
            showWarnings = FALSE)

test_that("inventory works baby!", {
  expect_message(inventory(name = name, showWarnings = FALSE),
                "P.?")
  expect_message(inventory(name = name, clear = FALSE, showWarnings = FALSE),
                 "P.?")
  expect_message(inventory(name = name, scripts = FALSE, showWarnings = FALSE),
                 "n.?")
  expect_message(inventory(name = name,
                           vehcomp = c(PC = 0, LCV = 1, HGV = 1, BUS = 1,
                                                   MC = 1), showWarnings = FALSE),
                 "n.?")
  expect_message(inventory(name = name,
                           vehcomp = c(PC = 1, LCV = 0, HGV = 1, BUS = 1,
                                       MC = 1), showWarnings = FALSE),
                 "n.?")
  expect_message(inventory(name = name,
                           vehcomp = c(PC = 1, LCV = 1, HGV = 0, BUS = 1,
                                       MC = 1), showWarnings = FALSE),
                 "n.?")
  expect_message(inventory(name = name,
                           vehcomp = c(PC = 1, LCV = 1, HGV = 1, BUS = 0,
                                       MC = 1), showWarnings = FALSE),
                 "n.?")
  expect_message(inventory(name = name,
                           vehcomp = c(PC = 1, LCV = 1, HGV = 1, BUS = 1,
                                       MC = 0), showWarnings = FALSE),
                 "n.?")
  expect_output(inventory(name = name, show.dir = TRUE, showWarnings = FALSE),
                 "D.?")
})

