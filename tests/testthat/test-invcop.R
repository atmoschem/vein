context("invcop")


test_that("invcop works", {
  expect_output(print(invcop(test = TRUE, out_name = tempfile())[1]),
                ".?")
})

