context("invcop")

name = file.path(tempdir(), "YourCity")
inventory(name = name)

test_that("invcop works", {
  expect_error(invcop(in_name = name,
                      out_name = "Hola"),
               ".?")
})

