context("test-temp_fact")

data(net)
data(pc_profile)


test_that("temp_fact works", {
  expect_equal(round(temp_fact(net$ldv+net$hdv, pc_profile)[1,1]),
               round(Vehicles(689.1404)))
})

test_that("temp_fact works", {
  expect_equal(round(temp_fact(net$ldv+net$hdv, pc_profile, net = net)$V1[1]),
               round(Vehicles(689.1404)))
})

test_that("temp_fact stops", {
  expect_error(temp_fact(q = NULL, pro = 1),
               "No.?\\(?")
})
