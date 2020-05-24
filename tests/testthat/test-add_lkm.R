context("test-add_lkm")

test_that("add_lkm works", {
  expect_equal(as.numeric(add_lkm(1)),1)
})

test_that("add_lkm works", {
  expect_equal(add_lkm(matrix(1, ncol = 10))$V1[1],
               1)
})

test_that("add_lkm works", {
  expect_equal(add_lkm(as.data.frame(matrix(1, ncol = 10)))$V1[1],
  1)
})
