context("ef_ldv_cold_list fixes")

test_that("one function per euro standard", {
  veh <- data.frame(age1 = 1:5, age2 = 1:5, age3 = 1:5)
  eu <- c("III", "II", "I")
  l <- ef_ldv_cold_list(veh, ta = 17, cc = "<=1400", f = "G", eu = eu, p = "CO")
  expect_equal(length(l), 3)
  for (i in seq_along(eu)) {
    expect_equal(l[[i]](20),
      ef_ldv_cold(ta = 17, cc = "<=1400", f = "G", eu = eu[i], p = "CO")(20))
  }
  expect_false(anyNA(sapply(l, function(f) f(20))))
})
