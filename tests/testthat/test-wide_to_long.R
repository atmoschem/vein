context("wide_to_long")

data(net)
dnet <- net@data
dnet$l <- 1
test_that("wide_to_long works", {
  expect_equal(round(wide_to_long(df = dnet)$V1[1]),
               4350)
  expect_equal(round(wide_to_long(df = dnet, column_fixed = "l")$V1[1]),
               4350)
  expect_error(wide_to_long(df = dnet, geometry = net),
               ".?")
  })


