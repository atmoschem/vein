context("ef_eea")


round(ef_eea(category = "PC",
fuel = "G",
segment = "Small",
euro = "I",
tech = NA,
pol = "CO",
mode = NA,
slope = 0,
load = 0)(10))


test_that("ef_eea works", {
  expect_equal(round(ef_eea(category = "PC",
                            fuel = "G",
                            segment = "Small",
                            euro = "I",
                            tech = NA,
                            pol = "CO",
                            mode = NA,
                            slope = 0,
                            load = 0)(10)),
                5
               )
})


test_that("ef_eea works", {
  expect_equal(round(ef_eea(category = "PC",
                            fuel = "D",
                            segment = "Small",
                            euro = "I",
                            tech = NA,
                            pol = "CO",
                            mode = NA,
                            slope = 0,
                            load = 0)(10)),
               1
  )
})

test_that("ef_eea works", {
  expect_equal(round(ef_eea(category = "HDV",
                            fuel = "D",
                            segment = "Rigid <=7.5 t",
                            euro = "I",
                            tech = NA,
                            pol = "NOx",
                            mode = NA,
                            slope = 0,
                            load = 0)(10)),
               4
  )
})

