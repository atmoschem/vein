context("ef_cetesb fixes")

raw_ef <- function(p, v, years) {
  e <- vein:::sysdata$cetesb
  e[[v]][e$Pollutant == p & e$Year %in% years][order(-e$Year[e$Pollutant == p & e$Year %in% years])]
}

test_that("ef_cetesb: <pol>_0km is the table value and <pol> is deteriorated", {
  for (p in c("CO", "HC", "NMHC", "NOx", "NO", "NO2", "RCHO")) {
    ef0 <- as.numeric(ef_cetesb(paste0(p, "_0km"), "PC_G", year = 2020, agemax = 10))
    efd <- as.numeric(ef_cetesb(p, "PC_G", year = 2020, agemax = 10))
    expect_equal(ef0, raw_ef(p, "PC_G", 2011:2020), info = p)
    expect_true(all(efd >= ef0), info = p)
    expect_true(efd[10] > ef0[10], info = p)
  }
})

test_that("ef_cetesb: resuspension EF for trucks is not 0", {
  expect_true(all(as.numeric(ef_cetesb("PM25RES", "TRUCKS_H_D", agemax = 5)) > 0))
  expect_true(all(as.numeric(ef_cetesb("PM10RES", "TRUCKS_H_D", agemax = 5)) > 0))
})

test_that("ef_cetesb: SO2 uses sppm of each vehicle", {
  a <- ef_cetesb("SO2", c("PC_G", "LCV_D", "PC_E"), year = 2020, agemax = 3,
    sppm = c(50, 500, 50))
  b <- ef_cetesb("SO2", "LCV_D", year = 2020, agemax = 3, sppm = 500)
  expect_equal(as.numeric(a$LCV_D), as.numeric(b))
  expect_equal(as.numeric(a$PC_G),
    raw_ef("FC", "PC_G", 2018:2020) * 50 * 2e-6)
  expect_true(all(as.numeric(a$PC_E) == 0))
})

test_that("ef_cetesb: year before 1984 has no 0 for old PC_G", {
  x <- as.numeric(ef_cetesb("CO", "PC_G", year = 1980, agemax = 30))
  expect_true(all(x > 0))
})

test_that("ef_cetesb: Age of full is relative to year", {
  x <- ef_cetesb("FC", "PC_G", year = 2017, agemax = 3, full = TRUE)
  expect_equal(x$Age, 1:3)
  expect_equal(x$Year, 2017:2015)
})

test_that("ef_cetesb: agemax above 50 repeats the oldest EF", {
  x <- as.numeric(ef_cetesb("CO", "PC_G", year = 2020, agemax = 55))
  expect_length(x, 55)
  expect_false(anyNA(x))
  expect_equal(x[55], x[50])
  y <- ef_cetesb("CO", "PC_G", year = 2020, agemax = 55, full = TRUE)
  expect_equal(y$Age, 1:55)
  expect_equal(y$Year, 2020:1966)
})

test_that("ef_cetesb: several old vehicle names", {
  x <- suppressMessages(ef_cetesb("CO", c("SLT", "PC_G"), year = 2020, agemax = 2))
  expect_equal(names(x), c("TRUCKS_SL_D", "PC_G"))
  y <- ef_cetesb("CO", c("PC_G", "PC_FG"), year = 2020, agemax = 2, full = TRUE)
  expect_true(all(c("PC_G", "PC_FG") %in% names(y)))
})
