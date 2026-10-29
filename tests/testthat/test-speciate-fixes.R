context("speciate and emis_chem2 fixes")

test_that("speciate: VOC profiles add up to 100 % of NMHC", {
  f <- function(...) sum(speciate(x = 100, ...)$x)
  expect_equal(f(spec = "nmhc", veh = "LDV", fuel = "E85", eu = "Evaporative"), 100,
    tolerance = 1e-6)
  expect_equal(f(spec = "voc", veh = "LDV", fuel = "E85", eu = "Evaporative"), 100,
    tolerance = 1e-6)
  expect_equal(f(spec = "voc", veh = "LDV", fuel = "G", eu = "OM-001"), 100,
    tolerance = 1e-3)
  expect_equal(f(spec = "nmhc", veh = "LDV", fuel = "D", eu = "OM-002-002"), 100,
    tolerance = 1e-3)
  expect_false(anyNA(speciate(x = 100, spec = "nmhc", veh = "LDV", fuel = "G",
    eu = "PRE")$x))
  expect_error(speciate(x = 1, spec = "nmhc", veh = "LDV", fuel = "G", eu = "XX"),
    "eu must be one of")
})

test_that("speciate: PM specs, metals, pmchar, nox, bcom, iag", {
  x <- speciate(x = c(10, 20), spec = "pm2023")
  expect_equal(x$E_PM25J, c(3.42, 6.84))
  m <- speciate(x = 10, spec = "metals", veh = "LDV", fuel = "G", eu = "all",
    list = TRUE)
  m2 <- speciate(x = 10, spec = "metals", veh = "LDV", fuel = "G", eu = "all")
  expect_equal(unname(unlist(m)), m2$x)
  p <- speciate(x = data.frame(pm = 2), spec = "pmchar", veh = "LDV", fuel = "D",
    eu = "I")
  expect_equal(names(p), c("pm", "pol", "unit"))
  expect_equal(nrow(speciate(x = 1, spec = "nox", veh = "LCV", fuel = "G",
    eu = "IV")), 1)
  expect_error(speciate(x = 1, spec = "bcom", veh = "PC", fuel = "G", eu = "XX"))
  b <- speciate(x = 100, spec = "bcom", veh = "LCV", fuel = "D", eu = "PRE")
  expect_lte(as.numeric(b$BC + b$OM), 100)
  expect_error(speciate(x = 1, spec = "petroiag_cb05v2", veh = "veh", fuel = "E",
    eu = "Exhaust"), "not available")
  expect_length(speciate(x = c(100, 200), spec = "iag", veh = "veh", fuel = "G",
    eu = "Exhaust", list = TRUE)[[1]], 2)
})

test_that("emis_chem2: grams to mol with the right molar mass", {
  df <- data.frame(id = 1, pol = "n-heptane", V1 = 100.20194)
  x <- emis_chem2(df, mech = "S99", nx = "V1")
  expect_equal(as.numeric(x$V1), 1, tolerance = 1e-4)
  expect_equal(as.character(units(x$V1)), "mol")
  # no side effects
  expect_false(inherits(df, "data.table"))
  expect_false(inherits(vein:::sysdata$chem, "data.table"))
  dfu <- data.frame(id = 1, pol = "ethane")
  dfu$V1 <- units::set_units(30.06904, "g/h")
  y <- emis_chem2(dfu, mech = "CB05opt2", nx = "V1")
  expect_equal(as.numeric(sum(y$V1)), 1, tolerance = 1e-4)
})

test_that("sysdata: molar masses agree with formulas", {
  ch <- vein:::sysdata$chem
  aw <- c(C = 12.0107, H = 1.00794, O = 15.9994)
  mw <- function(f) {
    p <- regmatches(f, gregexpr("([A-Z][a-z]?)([0-9]*)", f))[[1]]
    el <- sub("[0-9]+$", "", p)
    n <- sub("^[A-Za-z]+", "", p)
    if (!all(el %in% names(aw))) return(NA)
    sum(aw[el] * ifelse(n == "", 1, as.numeric(n)))
  }
  calc <- vapply(ch$Formula, mw, numeric(1))
  ok <- !is.na(calc)
  expect_true(all(abs(as.numeric(ch$Mwt)[ok] - calc[ok]) < 0.02))
  expect_false(anyDuplicated(ch$pol) > 0)
})

test_that("sysdata: other regional tables", {
  s <- vein:::sysdata
  b <- s$bcom
  expect_equal(b$BC[b$VEH == "LCV" & b$FUEL == "D"][1:5],
    b$BC[b$VEH == "PC" & b$FUEL == "D"][1:5])
  expect_equal(sort(unique(s$det_china$YEAR)), 2015:2018)
  expect_equal(sort(unique(s$det_china_long$YEAR)), 2015:2018)
  expect_false("Large" %in% s$det_china_long$VEH)
  expect_false(anyDuplicated(s$nox[, c("VEH", "STANDARD", "FUEL")]) > 0)
  e <- s$cetesb
  v <- c("MC_150_500_FG", "MC_500_FG", "MC_150_500_FE", "MC_500_FE")
  r <- as.matrix(e[e$Pollutant == "CO2", v]) / as.matrix(e[e$Pollutant == "FC", v])
  expect_true(all(apply(r, 2, function(z) diff(range(z, na.rm = TRUE))) < 0.01))
  expect_false("pollutantname" %in% s$decoder$pollutants$pollutant)
})
