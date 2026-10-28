context("hbefa_eea")

skip_if(is.null(vein:::sysdata$hbefa), "sysdata$hbefa not available")

test_that("every EEA key of vein has an HBEFA4 class", {
  tab <- hbefa_eea()
  expect_true(all(c("VEH", "TYPE", "CC", "FUEL", "EURO", "class", "exact",
                    "note") %in% names(tab)))
  expect_false(anyNA(tab$class))
  expect_true(all(tab$class %in% hbefa_classes()$class))
  expect_true(all(tab$exact == !nzchar(tab$note)))
})

test_that("direct equivalences", {
  expect_equal(
    hbefa_eea("PC", "4S", "1400_2000", "G",
              c("PRE", "I", "IV", "VI", "VIc", "VId", "VII")),
    paste0("HBEFA4/PC_petrol_", c("ltECE", "Euro-1", "Euro-4", "Euro-6ab",
                                  "Euro-6d-temp", "Euro-6d", "Euro-7")))
  expect_equal(hbefa_eea("PC", "ECE_1504", ">2000", "G", "PRE"),
               "HBEFA4/PC_petrol_ECE-15_04")
  expect_equal(hbefa_eea("PC", "4S", "<=1400", "D", "III+DPF"),
               "HBEFA4/PC_diesel_Euro-3_(DPF)")
  expect_equal(hbefa_eea("LCV", "4S", c("<3.5", "N1-I"), "D", "V"),
               c("HBEFA4/LCV_diesel_N1-III_Euro-5",
                 "HBEFA4/LCV_diesel_M+N1-I_Euro-5"))
  expect_equal(hbefa_eea("Motorcycle", "4S", c("<=250", "250_750"), "G", "III"),
               c("HBEFA4/MC_4S_le250cc_Euro-3", "HBEFA4/MC_4S_gt250cc_Euro-3"))
  expect_equal(hbefa_eea("Trucks", "TT", ">34 & <=40", "D", "VI"),
               "HBEFA4/TT_AT_gt34-40t_Euro-VI_A-C")
  expect_equal(hbefa_eea(v = "Ubus", t = "Artic", eu = "II"),
               "HBEFA4/UBus_Artic_gt18t_Euro-II")
  expect_equal(hbefa_eea(v = "Coach", t = "3Axes", eu = "III"),
               "HBEFA4/Coach_3-Axes_gt18t_Euro-III")
  d <- hbefa_eea("Trucks", "RT", ">14 & <=20", "D", c("III", "V"), details = TRUE)
  expect_equal(d$exact, c(TRUE, FALSE))
  expect_error(hbefa_eea("PC", "4S", "<=1400", "X", "IV"), "No HBEFA4 equivalent")
})

test_that("ef_hbefa_speed equals ef_hbefa of the equivalent class", {
  V <- c(10, 30, 50, 90, 120)
  f1 <- ef_hbefa_speed("PC", "4S", "1400_2000", "G", "IV", "CO2")
  expect_true(is.function(f1))
  expect_equal(f1(V), ef_hbefa("HBEFA4/PC_petrol_Euro-4", "CO2")(V))
  f2 <- ef_hbefa_speed(v = "Trucks", t = "RT", g = ">14 & <=20", eu = "V",
                       p = "NOx", gr = 0.02)
  expect_equal(f2(V), ef_hbefa("HBEFA4/RT_gt14-20t_Euro-V_SCR", "NOx",
                               slope = atan(0.02) * 180 / pi)(V))
  # k and fcorr
  f3 <- ef_hbefa_speed("PC", "4S", "1400_2000", "G", "IV", "CO2", k = 2,
                       fcorr = c(1, 1, 1, 1, 3, 1, 1, 1))
  expect_equal(f3(V), 6 * f1(V))
})

test_that("ef_hbefa_speed returns the shapes of ef_ldv_speed", {
  eu <- c("I", "IV", "VI")
  a <- ef_hbefa_speed("PC", "4S", "<=1400", "G", eu, "CO")
  b <- ef_ldv_speed("PC", "4S", "<=1400", "G", eu, "CO")
  expect_equal(class(a), class(b))
  expect_equal(names(a), names(b))
  s <- Speed(c(20, 50))
  a <- ef_hbefa_speed("PC", "4S", "<=1400", "G", eu, "CO", speed = s)
  b <- ef_ldv_speed("PC", "4S", "<=1400", "G", eu, "CO", speed = s)
  expect_equal(class(a), class(b))
  expect_equal(names(a), names(b))
  expect_equal(dim(a), dim(b))
  s2 <- Speed(data.frame(h1 = c(20, 50), h2 = c(30, 60)))
  a <- ef_hbefa_speed("PC", "4S", "<=1400", "G", "IV", "CO", speed = s2)
  expect_equal(dim(a), c(2L, 2L))
  eud <- data.frame(a = c("I", "II"), b = c("IV", "V"))
  a <- ef_hbefa_speed("PC", "4S", "<=1400", "G", eud, "CO", speed = s)
  b <- ef_ldv_speed("PC", "4S", "<=1400", "G", eud, "CO", speed = s)
  expect_equal(dim(a), dim(b))
  expect_equal(a$row_eu, b$row_eu)
})

test_that("NO2 and NO use the EEA NOx shares", {
  V <- c(20, 50, 90)
  nox <- ef_hbefa_speed("PC", "4S", "<=1400", "D", "VI", "NOx")(V)
  no2 <- ef_hbefa_speed("PC", "4S", "<=1400", "D", "VI", "NO2")(V)
  no <- ef_hbefa_speed("PC", "4S", "<=1400", "D", "VI", "NO")(V)
  expect_equal(no2 + no, nox)
  expect_equal(no2 / nox, rep(0.3, 3))
  h <- ef_hbefa_speed(v = "Trucks", t = "RT", g = "<=7.5", eu = "IV", p = "NO2")(V)
  expect_true(all(h >= 0))
  expect_error(ef_hbefa_speed("PC", "4S", "<=1400", "G", "IV", "CH4"),
               "not available in HBEFA4")
})

test_that("ef_hbefa_speed works in emis()", {
  veh <- age_veh(x = 1:5 * 100, name = "PC", agemax = 3)
  set.seed(1)
  sp <- Speed(as.data.frame(matrix(runif(5 * 168, 10, 90), nrow = 5)))
  data(pc_profile, envir = environment())
  ef <- ef_hbefa_speed("PC", "4S", "<=1400", "G", c("VI", "V", "IV"), "FC")
  e <- emis(veh = veh, lkm = units::set_units(rep(1, 5), "km"), ef = ef,
            speed = sp, profile = pc_profile, verbose = FALSE)
  expect_equal(dim(e), c(5L, 3L, 24L, 7L))
  expect_true(all(as.numeric(e) >= 0))
})
