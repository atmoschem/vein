context("ef_im")


test_that("multiplication works", {
  expect_equal(as.numeric(ef_im(ef = seq(0.1, 2, 0.2),
                                seq(0.1, 1, 0.1),
                                10)[1]),
               0.2)
})
