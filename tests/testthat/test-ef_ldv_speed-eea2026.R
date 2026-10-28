context("ef_ldv_speed EMEP/EEA 2026")

test_that("PRE PC power-law rows use a*V^b (QA Quito 2024-2025, F2)", {
  ldv <- vein:::sysdata$ldv
  bad <- ldv$VEH == "PC" & ldv$EURO == "PRE" &
    grepl("a*V+b", ldv$Y, fixed = TRUE)
  expect_equal(sum(bad), 0)
})

test_that("PRE gasoline PC FC is plausible", {
  for (cc in c("<=1400", "1400_2000", ">2000")) {
    for (t in c("4S", "ECE_1501", "ECE_1502", "ECE_1503", "ECE_1504")) {
      ef <- ef_ldv_speed(v = "PC", t = t, cc = cc, f = "G", eu = "PRE", p = "FC")
      expect_true(all(ef(10:130) < 300))
    }
  }
  # values reported in the QA after correcting the equation
  expect_equal(ef_ldv_speed("PC", "4S", "1400_2000", "G", "PRE", "FC")(c(10, 50)),
               c(178.6, 69.3), tolerance = 1e-3)
})

test_that("EC and CO reproduce Appendix 4 (PC petrol Medium Euro 4 PFI)", {
  # coefficients of the guidebook Appendix 4 (July 2026) rows:
  # Alpha, Beta, Gamma, Delta, Epsilon, Zita, Hta, Reduction factor
  eea <- function(V, p) {
    (p[1] * V^2 + p[2] * V + p[3] + p[4] / V) / (p[5] * V^2 + p[6] * V + p[7]) * (1 - p[8])
  }
  p_ec <- c(0.0001317230068, 0.005485959295, 2.619195051, 1.727746124e-09,
            -8.504150127e-05, 0.02358438407, 0.3442975778, 0.2319672923)
  p_co <- c(5.496706977e-12, -0.03341761208, 5.109834522, -1.043727103e-07,
            0.001871536276, -0.5288309062, 37.50573903, 0)
  r <- vein:::sysdata$ldv
  r <- r[r$VEH == "PC" & r$TYPE == "4S" & r$CC == "1400_2000" & r$FUEL == "G" &
           r$EURO == "IV" & r$POLLUTANT == "EC", ]
  expect_equal(nrow(r), 1)
  expect_match(r$REF, "Passenger Cars | Petrol | Medium | Euro 4 | PFI | EC", fixed = TRUE)
  V <- c(10, 30, 50, 80, 120)
  ec <- ef_ldv_speed("PC", "4S", "1400_2000", "G", "IV", "EC")
  fc <- ef_ldv_speed("PC", "4S", "1400_2000", "G", "IV", "FC")
  co <- ef_ldv_speed("PC", "4S", "1400_2000", "G", "IV", "CO")
  expect_equal(ec(V), eea(V, p_ec), tolerance = 1e-8)
  expect_equal(fc(V), eea(V, p_ec) * 1000 / 43.774, tolerance = 1e-8)
  expect_equal(co(V), eea(V, p_co), tolerance = 1e-8)
})

test_that("CO2/FC is constant for every LDV row", {
  ldv <- vein:::sysdata$ldv
  keys <- unique(ldv[ldv$POLLUTANT == "CO2", c("VEH", "TYPE", "CC", "FUEL", "EURO")])
  set.seed(1)
  keys <- keys[sample(nrow(keys), 40), ]
  ratio <- c(G = 3.169, D = 3.169, FH = 3.169, PHEV_G = 3.169, PHEV_D = 3.169,
             LPG = 3.024, CNG = 2.743)
  for (i in seq_len(nrow(keys))) {
    k <- keys[i, ]
    co2 <- ef_ldv_speed(k$VEH, k$TYPE, k$CC, k$FUEL, k$EURO, "CO2")(c(10, 50, 100))
    fc <- ef_ldv_speed(k$VEH, k$TYPE, k$CC, k$FUEL, k$EURO, "FC")(c(10, 50, 100))
    expect_equal(co2 / fc, rep(ratio[[k$FUEL]], 3), tolerance = 1e-3)
  }
})

test_that("diesel PC CO2 is not FC and motorcycle CO2 is positive (QA F4, F5)", {
  co2 <- ef_ldv_speed("PC", "4S", "<=1400", "D", "III", "CO2")(50)
  fc <- ef_ldv_speed("PC", "4S", "<=1400", "D", "III", "FC")(50)
  expect_equal(co2 / fc, 3.169, tolerance = 1e-3)
  mc <- ef_ldv_speed("Motorcycle", "4S", "<=250", "G", "PRE", "CO2")(c(20, 50, 80))
  expect_true(all(mc > 0))
})

test_that("NMHC equals HC minus CH4", {
  V <- c(20, 60, 100)
  hc <- ef_ldv_speed("PC", "4S", "<=1400", "G", "III", "HC")(V)
  ch4 <- ef_ldv_speed("PC", "4S", "<=1400", "G", "III", "CH4")(V)
  nm <- ef_ldv_speed("PC", "4S", "<=1400", "G", "III", "NMHC")(V)
  expect_equal(nm, pmax(hc - ch4, 0))
})

test_that("fcorr is applied when length(eu) == 1", {
  a <- ef_ldv_speed("PC", "4S", "<=1400", "G", "III", "CO")(50)
  b <- ef_ldv_speed("PC", "4S", "<=1400", "G", "III", "CO", fcorr = rep(0.5, 8))(50)
  expect_equal(b, a / 2)
})

test_that("all paths return the same non-negative values", {
  V <- c(5, 20, 50, 80, 110)
  f1 <- ef_ldv_speed("LCV", "4S", "N1-II", "D", "VI", "HC")
  m <- ef_ldv_speed("LCV", "4S", "N1-II", "D", c("VI", "V"), "HC", speed = Speed(V))
  pr <- vein:::ef_ldv_programs("LCV", "4S", "N1-II", "D", "VI", "HC")[[1]]
  expect_true(all(f1(V) >= 0))
  expect_equal(f1(V), as.numeric(m[[1]]))
  expect_equal(f1(V), vein:::ef_eval_program(pr, V))
})

test_that("LCV euro V is not replaced by PC", {
  a <- ef_ldv_speed("LCV", "4S", "<3.5", "D", c("IV", "V"), "NOx", speed = Speed(50))
  b <- ef_ldv_speed("LCV", "4S", "<3.5", "D", "IV", "NOx", speed = Speed(50))
  expect_equal(as.numeric(a[[1]]), as.numeric(b))
})

test_that("unknown combinations give a clear error", {
  expect_error(ef_ldv_speed("PC", "4S", "<=1400", "G", "XX", "CO"), "No emission factor")
  expect_error(ef_ldv_speed("PC", "4S", "<=1400", "G", "I", "SO2"), "'x'")
})

test_that("get_ef_ref returns a copy", {
  x <- get_ef_ref("ldv")
  x[, VEH := "HACKED"]
  expect_false(vein:::sysdata$ldv$VEH[1] == "HACKED")
  expect_false(data.table::is.data.table(vein:::sysdata$ldv))
  expect_equal(ef_ldv_speed("PC", "4S", "<=1400", "G", "III", "CO")(50),
               ef_ldv_speed("PC", "4S", "<=1400", "G", "III", "CO")(50))
})
