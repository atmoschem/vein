context("ef_nitro")

test_that("multiplication works", {
  expect_equal( ef_nitro(v = "PC", t = "Hot", cond = "Urban", cc = "<=1400", f = "G",
                         eu = "III", p = "NH3", S = 10,
                         show.equation = FALSE)(10),
                0.001637825)
})
