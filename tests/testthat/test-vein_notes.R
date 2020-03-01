context("vein_notes")

a <- tempfile()

test_that("emis_cold works", {
  expect_output(readLines(vein_notes("notes", file = a)),
                ".?")
})

