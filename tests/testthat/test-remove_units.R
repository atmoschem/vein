context("test-remove_units")

test_that("remove_units works", {
  ef_cetesb(p = "CO", c("PC_G"))[1]
  expect_equal(remove_units(ef_cetesb(p = "CO", c("PC_G"))[1]),
               0.21)
})

test_that("remove_units works", {
  df <- ef_cetesb(p = "CO", c("PC_G", "PC_FG"))[1, ]
  row.names(df) <- NULL
  expect_equal(remove_units(df),
               data.frame(PC_G = 0.21, PC_FG = 0.34))
})

test_that("remove_units works", {
  df <- ef_cetesb(p = "CO", c("PC_G", "PC_FG"))[1, ]
  row.names(df) <- NULL
  expect_equal(remove_units(as.matrix(df)),
               data.frame(PC_G = 0.21,
                          PC_FG = 0.34))
})

