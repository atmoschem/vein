context("test-remove_units")

test_that("remove_units works", {
  ef_cetesb(p = "CO_0km", c("PC_G"))[1]
  expect_equal(remove_units(ef_cetesb(p = "CO_0km", c("PC_G"))[1]),
               0.1612112)
})

test_that("remove_units works", {
  df <- ef_cetesb(p = "CO_0km", c("PC_G", "PC_FG"))[1, ]
  row.names(df) <- NULL
  expect_equal(as.data.frame(remove_units(df)),
               data.frame(PC_G = 0.1612112, PC_FG = 0.2580112))
})

test_that("remove_units works", {
  df <- ef_cetesb(p = "CO_0km", c("PC_G", "PC_FG"))[1, ]
  row.names(df) <- NULL
  expect_equal(remove_units(as.matrix(df)),
               data.frame(PC_G = 0.1612112,
                          PC_FG = 0.2580112))
})

