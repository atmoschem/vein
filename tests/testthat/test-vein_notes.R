context("vein_notes")

a <- tempfile()
vein_notes("notes",
           file = a)

test_that("emis_cold works", {
  expect_output(print(readLines(paste0(a, '.txt'))[2]),
                "V.?")
})

