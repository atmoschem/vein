context("test-vkm")

pc <- lkm <- 1:10*100
pro <- matrix(rep(1, 24), ncol=7, nrow=24)


test_that("vkm works", {
  expect_equal(vkm(veh = pc, lkm = lkm, profile = pro)[1,1], 10000)
})

test_that("vkm works", {
  expect_equal(vkm(veh = pc, lkm = lkm, profile = pro, array = FALSE)[[1]][[1]][[1]], 10000)
})

test_that("vkm works", {
  expect_equal(vkm(veh = pc, lkm = lkm, profile = pro[, 1])[1,1], 10000)
})

test_that("vkm works", {
  expect_equal(vkm(veh = pc, lkm = lkm, profile = pro[, 1], as_df = FALSE)[1], 10000)
})
