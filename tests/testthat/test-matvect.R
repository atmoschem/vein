context("test-matvect")

data(net)
veh <- age_ldv(net$ldv[1], agemax = 4)


test_that("matvect works", {
  expect_equal(as.numeric(matvect(veh, 1)[1]),
               1029.404 + 0.000148)
})

test_that("matvect works", {
  expect_equal(as.numeric(matvect(veh[1], matrix(1, ncol = 1),
                                  by = "col")),
               1029.404 + 0.000148)
})


test_that("matvect error", {
  expect_error(matvect(df = veh, x = c(1,1)),
               "Rows.?\\(?")
})

test_that("matvect error", {
  expect_error(matvect(df = veh, x = 1, by = "col"),
               "Cols.?\\(?")
})
