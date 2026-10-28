context("emis_emfac and moves fixes")

library(data.table)

test_that("ef_emfac fills zeros with the non-zero mean and keeps speed order", {
  ef <- CJ(ModelYear = 2021:2020, Speed = c(5, 10, 15, 20, 25))
  ef[, `:=`(`Vehicle Category` = "LDA", Fuel = "Gasoline",
            CO_RUNEX = Speed / 10 + (2021 - ModelYear), FuelConsumption = 0.03)]
  ef[ModelYear == 2021 & Speed == 10, CO_RUNEX := 0]
  f <- tempfile(fileext = ".csv")
  fwrite(ef, f)
  d <- ef_emfac(f, verbose = FALSE)
  co <- d[pollutant == "CO_RUNEX" & ModelYear == 2021]
  expect_equal(co$Speed, c(5, 10, 15, 20, 25))
  expect_equal(co$gmiles[co$Speed == 10], mean(c(0.5, 1.5, 2, 2.5)))

  veh <- data.frame(a = c(1, 1, 1, 1), b = c(1, 1, 1, 1))
  sp <- Speed(data.frame(S1 = c(22, 7, 3, 16)), dist = "miles")
  lkm <- units::set_units(c(1, 10, 100, 1000), "miles")
  # whole ef_emfac output: only pol is used
  r <- emis_emfac(ef = d, veh = veh, lkm = lkm, tfs = 1, speed = sp,
                  vehname = "LDA", pol = "CO_RUNEX", modelyear = 2021:2020,
                  verbose = FALSE)
  # bins 20, 5, 5 (below first bin), 15
  expect_equal(as.numeric(r[age == 1]$emi), c(2, 0.5 * 10, 0.5 * 100, 1.5 * 1000))
  expect_equal(r[age == 1]$id, 1:4)
  expect_error(emis_emfac(ef = d, veh = veh, lkm = lkm, tfs = 1, speed = sp,
                          vehname = "LDA", pol = "NOx_RUNEX", modelyear = 2021,
                          verbose = FALSE), "pol")
  expect_error(emis_emfac(ef = d, veh = veh, lkm = lkm, tfs = 1,
                          vehname = "LDA", pol = "CO_RUNEX", modelyear = 2021,
                          verbose = FALSE), "speed")
})

mk_moves <- function(nages = 3, procs = 1, pols = 2, bins = 0:16) {
  CJ(hourID = 1:24, pollutantID = pols, processID = procs, sourceTypeID = 21,
     fuelTypeID = 1, roadTypeID = 5L, modelYearID = 2020:(2020 - nages + 1),
     avgSpeedBinID = bins)[
       , `:=`(EF = avgSpeedBinID + (2020 - modelYearID + 1) / 100 + processID * 1000,
              ratePerDistance = avgSpeedBinID * 1)]
}
speed_bin <- as.data.frame(matrix(c(10L, 2L, 5L), nrow = 3, ncol = 24))
lkm <- c(1, 10, 100)
profile <- as.data.frame(matrix(1, 24, 7))
veh3 <- data.frame(a = c(1, 1, 1), b = c(1, 1, 1), c = c(1, 1, 1))
exp3 <- sapply(1:3, function(r) lkm[r] * sum(c(10, 2, 5)[r] + (1:3) / 100 + 1000))

test_that("moves_rpd keeps link order", {
  r <- moves_rpd(veh = veh3, lkm = lkm, ef = mk_moves(nages = 1),
                 speed_bin = speed_bin, profile = profile, pollutant_id = 2,
                 vehicle = "PC", vehicle_type = "PC", fuel_subtype = "G")
  expect_equal(r$streets$H1, 3 * c(10, 2 * 10, 5 * 100))
  expect_error(moves_rpd(veh = veh3, lkm = lkm, ef = mk_moves(nages = 3),
                         speed_bin = speed_bin, profile = profile,
                         pollutant_id = 2, vehicle = "PC", vehicle_type = "PC",
                         fuel_subtype = "G"), "modelYearID")
})

test_that("moves_ef stops with several rates per bin", {
  expect_error(moves_ef(ef = mk_moves(nages = 3), vehicles = "PC",
                        speed_bin = speed_bin), "modelYearID")
  x <- moves_ef(ef = mk_moves(nages = 1), vehicles = "PC", speed_bin = speed_bin)
  expect_equal(x[["1"]], c(10, 2, 5))
})

test_that("moves_rpdy keeps link order, profile and process", {
  r <- moves_rpdy(veh = veh3, lkm = lkm, ef = mk_moves(), pollutant_id = 2,
                  speed_bin = speed_bin, profile = profile, vehicle = "PC",
                  vehicle_type = "PC", fuel_subtype = "G", process_id = 1)
  expect_equal(r$streets$H1, exp3)
  r <- moves_rpdy(veh = list(veh3), lkm = lkm, ef = mk_moves(), pollutant_id = 2,
                  speed_bin = speed_bin, profile = profile, vehicle = "PC",
                  vehicle_type = "PC", fuel_subtype = "G", process_id = 1)
  expect_equal(r$streets$H1, exp3)
  p2 <- profile
  p2[1, 1] <- 2
  r <- moves_rpdy(veh = veh3, lkm = c(1, 1, 1), ef = mk_moves(procs = 1:2),
                  pollutant_id = 2, speed_bin = as.data.frame(matrix(5L, 3, 24)),
                  profile = p2, vehicle = "PC", vehicle_type = "PC",
                  fuel_subtype = "G", process_id = 1)
  expect_equal(r$streets$H1, rep(2 * sum(5 + (1:3) / 100 + 1000), 3))
})

test_that("moves_rpdy_sf and moves_rpsy_sf", {
  sb <- as.data.frame(matrix(rep(c(10L, 2L), each = 3), nrow = 3, ncol = 24))
  r <- moves_rpdy_sf(veh = veh3, lkm = c(1, 1, 1), ef = mk_moves(pols = c(2, 3)),
                     speed_bin = sb, profile = rep(1, 24), vehicle = "PC",
                     vehicle_type = "PC", fuel_subtype = "G")
  s1 <- r$streets[id == 1]
  expect_equal(s1$H1, rep(sum(10 + (1:3) / 100 + 1000), 2))
  expect_equal(s1$H2, rep(sum(2 + (1:3) / 100 + 1000), 2))
  expect_equal(nrow(r$veh), 24 * 2 * 3)
  r <- moves_rpsy_sf(veh = veh3, lkm = c(1, 1, 1),
                     ef = mk_moves(bins = 0L, pols = c(2, 3)),
                     profile = rep(1, 24), vehicle = "PC", vehicle_type = "PC",
                     fuel_subtype = "G")
  expect_equal(r$streets[id == 1]$H1, rep(sum((1:3) / 100 + 1000), 2))
})

test_that("moves_rpdy_meta and moves_rpsy_meta", {
  wd <- tempfile()
  dir.create(file.path(wd, "veh"), recursive = TRUE)
  old <- setwd(wd)
  on.exit(setwd(old), add = TRUE)
  md <- data.table(vehicles = "PC_G", name = "PC", fuel = "G", fuelTypeID = 1,
                   sourceTypeID = 21)
  saveRDS(veh3, "veh/PC_G.rds")
  sb <- as.data.frame(matrix(rep(c(10L, 2L), each = 3), nrow = 3, ncol = 24))
  r <- suppressMessages(
    moves_rpdy_meta(metadata = md, lkm = c(1, 1, 1), ef = mk_moves(procs = 1:2),
                    speed_bin = sb, profile = data.frame(PC_G = rep(1, 24)),
                    agemax = 3, simplify = FALSE))
  r1 <- r[id == 1 & hour %in% 1:2]
  expect_equal(r1[hour == 1 & processID == 2]$age_total, sum(10 + (1:3) / 100 + 2000))
  expect_equal(r1[hour == 2 & processID == 1]$age_total, sum(2 + (1:3) / 100 + 1000))
  ef1 <- mk_moves(bins = 0L)
  ef1[, EF := 1]
  r <- suppressMessages(
    moves_rpsy_meta(metadata = md, lkm = c(1, 1, 1), ef = ef1,
                    profile = data.frame(OTHER = rep(5, 24), PC_G = rep(1, 24)),
                    agemax = 3, simplify = FALSE))
  expect_equal(as.numeric(unlist(r[hour == 1, c("age_1", "age_2", "age_3")])),
               rep(1, 9))
})

test_that("moves_speed converts units, bins are lower inclusive", {
  expect_equal(moves_speed(Speed(c(100, 30))), c(13, 5))
  expect_equal(moves_speed(c(0, 2.5, 7.5, 72.5, 80, NA)), c(0, 2, 3, 16, 16, NA))
  expect_equal(moves_speed(list(a = c(10, 20)))$a, c(3, 5))
})
