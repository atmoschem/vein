# context("ef_ive")
#
# test_that("ef_ive works", {
#   expect_equal(as.numeric(ef_ive(mileage = 10, pol = "CO_gkm")),
#                22.255)
# })
#
# test_that("ef_ive stops", {
#   expect_error(ef_ive(description = "Auto/Sml",
#                       mileage = 10, pol = "CO_gkm"),
#                "No.?\\(?")
# })
#
# test_that("ef_ive works", {
#   expect_equal(ef_ive(mileage = 10, pol = "CO_gstart"),
#                24.371)
# })
#
# test_that("ef_ive works", {
#   expect_equal(as.numeric(ef_ive(mileage = 10, pol = "CO_gkm", details = TRUE)),
#                22.255)
# })
