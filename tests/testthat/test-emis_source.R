context("emis_source")
data(net)

test_that("emis_merge works", {
  expect_error(emis_source(ask = FALSE),
                ".?")
})

