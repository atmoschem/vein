context("fuel_corr fixes")

# EMEP/EEA 1.A.3.b.i-iv, Tables 3-82 to 3-84
fpm_ldv <- function(den, pah, cn, t95, s) {
  (-0.3879873 + 0.0004677*den + 0.0004488*pah + 0.0004098*cn + 0.0000788*t95) *
    (1 - 0.015*(450 - s)/100)
}
fco_hdv <- function(den, pah, cn, t95) {
  2.24407 - 0.0011*den + 0.00007*pah - 0.00768*cn - 0.00087*t95
}
d_new <- c(den = 820, pah = 2, cn = 55, t95 = 330, s = 10)
d_96 <- c(den = 840, pah = 9, cn = 51, t95 = 350, s = 400)

test_that("LDV diesel PM uses the PM equation (not NOx)", {
  f <- fuel_corr("PRE", d = d_new)
  expect_equal(f$LDVD$PM[[1]],
               do.call(fpm_ldv, as.list(d_new)) / do.call(fpm_ldv, as.list(d_96)))
  expect_false(isTRUE(all.equal(f$LDVD$PM[[1]], f$LDVD$NOx[[1]])))
})

test_that("gasoline VOC correction uses the user e150", {
  g1 <- c(e100 = 52, aro = 39, o2 = 0.4, e150 = 86, olefin = 10, s = 165)
  g2 <- g1; g2[["e150"]] <- 95
  expect_equal(fuel_corr("PRE", g = g1)$LDVG$COV[[1]], 1)
  expect_lt(fuel_corr("PRE", g = g2)$LDVG$COV[[1]], 1)
  expect_equal(fuel_corr("PRE", g = g2)$LDVG$COV[[1]],
               (1 - 0.01*(95 - 90.2)) / (1 - 0.01*(86 - 90.2)))
})

test_that("HDV CO uses -0.00087 T95", {
  d <- c(den = 840, pah = 9, cn = 51, t95 = 370, s = 400)
  f <- fuel_corr("PRE", d = d)
  expect_equal(f$HDV$CO[[1]], fco_hdv(840, 9, 51, 370) / fco_hdv(840, 9, 51, 350))
  # with -0.00087 T95 a higher T95 lowers CO (with +0.0001706 it was capped at 1)
  expect_lt(f$HDV$CO[[1]], 1)
})
