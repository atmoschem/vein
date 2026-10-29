context("ef_china fixes")

efc <- function(...) as.numeric(ef_china(...))

test_that("ef_china: load_factor classes use load_factor", {
  base <- efc(t = "Small", f = "G", standard = "IV", p = "CO", load_factor = 0.5)
  expect_equal(efc(t = "Small", f = "G", standard = "IV", p = "CO",
    load_factor = 0.55) / base, 1.07)
  expect_equal(efc(t = "Small", f = "G", standard = "IV", p = "CO",
    load_factor = 0.7) / base, 1.16)
  expect_equal(efc(t = "Small", f = "G", standard = "IV", p = "CO",
    load_factor = 0.9) / base, 1.33)
})

test_that("ef_china: speed of 80 km/h uses S80", {
  a <- efc(t = "Small", f = "G", standard = "IV", p = "CO", speed = Speed(80))
  b <- efc(t = "Small", f = "G", standard = "IV", p = "CO", speed = Speed(90))
  expect_gt(a, 0)
  expect_equal(a, b)
})

test_that("ef_china: vector and data.frame standard give the same EF", {
  for (p in c("CO", "NOx", "PM10")) {
    for (f in c("G", "D")) {
      a <- efc(t = "Small", f = f, standard = c("PRE", "III"), p = p,
        altitude = 2000, ta = celsius(30), humidity = 0.7)
      b <- ef_china(t = "Small", f = f, standard = data.frame(a = "PRE", b = "III"),
        p = p, altitude = 2000, ta = celsius(30), humidity = 0.7,
        speed = Speed(30), sulphur = 50)
      expect_equal(a, c(as.numeric(b$V1), as.numeric(b$V2)), info = paste(p, f))
    }
  }
})

test_that("ef_china: humidity of length 1 with data.frame standard", {
  x <- ef_china(t = "Small", f = "G", standard = data.frame(a = c("III", "III")),
    p = "NOx", ta = celsius(c(15, 15)), altitude = c(1000, 1000),
    speed = Speed(c(30, 30)), sulphur = c(50, 50))
  expect_false(anyNA(as.numeric(x$V1)))
})

test_that("ef_china: standard VI as V, CNG, errors", {
  expect_equal(efc(t = "Small", f = "G", standard = "VI", p = "CO"),
    efc(t = "Small", f = "G", standard = "V", p = "CO"))
  expect_false(is.na(efc(t = "Mini", f = "CNG", standard = "IV", p = "CO")))
  expect_error(ef_china(t = "Small", f = "ALL", standard = "IV", p = "CO"), "ALL")
  expect_error(ef_china(t = "Small", f = "G", standard = "IV", p = "CO",
    baseyear_det = 2014), "baseyear_det")
})

test_that("ef_china helpers: same sulphur classes, order kept, VI", {
  for (s in c(5, 30, 50, 100, 300, 400, 600, 900)) {
    expect_equal(ef_china_s(s = s, standard = "IV", p = "CO"),
      efc(t = "Small", f = "G", standard = "IV", p = "CO", sulphur = s) /
        efc(t = "Small", f = "G", standard = "IV", p = "CO", sulphur = 50),
      info = s)
  }
  sp <- c(90, 10, 50, 25)
  expect_equal(ef_china_speed(speed = sp, standard = "I", p = "CO", long = TRUE)[[1]],
    ef_china_speed(speed = sp, standard = "I", p = "CO")[[1]])
  expect_equal(as.numeric(ef_china_long(standard = c("V", "VI", "I"), p = "CO")),
    as.numeric(ef_china_long(standard = c("V", "V", "I"), p = "CO")))
  expect_length(ef_china_det(standard = c("V", "IV", "I"), p = "CO", yeardet = 2016), 3)
  expect_error(ef_china_long(standard = "X", p = "CO"))
})

test_that("ef_china: sysdata is not modified by reference", {
  invisible(ef_china_long(standard = "I", p = "CO"))
  invisible(ef_china_speed(speed = 30, standard = "I", p = "CO"))
  invisible(ef_china_s(s = 50, standard = "I", p = "CO"))
  invisible(ef_china_te(te = 5, p = "CO"))
  expect_false(inherits(vein:::sysdata$ef_china, "data.table"))
  expect_false(inherits(vein:::sysdata$speed_china, "data.table"))
  expect_false(inherits(vein:::sysdata$sulphur_china, "data.table"))
  expect_false(inherits(vein:::sysdata$te_china, "data.table"))
})

test_that("emis_china: factors by standard (columns) and hour (rows)", {
  x <- data.frame(a = c(1, 2), b = c(1, 2), c = c(1, 2))
  std <- c("PRE", "I", "II")
  E <- emis_china(x = x, lkm = c(1, 1), tfs = c(1, 1), t = "Small", f = "G",
    standard = std, s = 50,
    speed = data.frame(h1 = c(30, 30), h2 = c(30, 30)),
    te = c(5, 30), hu = c(50, 50), h = c(1000, 1000), p = "CO",
    verbose = FALSE)
  base <- as.numeric(ef_china_long(standard = std, p = "CO")) *
    ef_china_det(standard = std, p = "CO", yeardet = 2016) *
    unlist(ef_china_speed(speed = 30, standard = std, p = "CO"))
  tf <- ef_china_te(te = c(5, 30), p = "CO")
  expected <- rbind(base * tf[1], 2 * base * tf[1], base * tf[2], 2 * base * tf[2])
  got <- as.matrix(as.data.frame(lapply(E[, 1:3], as.numeric)))
  dimnames(got) <- NULL
  dimnames(expected) <- NULL
  expect_equal(got, expected)
  expect_equal(E$Hour, c(1, 1, 2, 2))
  expect_equal(as.character(units(E[[1]])), "g")
})
