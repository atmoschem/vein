context("add_polid")

data(net)
test_that("st_explode works", {
  expect_equal(round(st_explode(net)$ldv[1]),
                993)
  expect_output(st_explode(net),
               ".?")
})

