context("emis_merge")
data(net)

test_that("emis_merge works", {
  expect_error(emis_merge(),
                ".?")
  expect_error(emis_merge(streets = FALSE),
               ".?")
  expect_error(emis_merge(net = net),
               ".?")
})

