context("emis_chem")
df <- data.frame(emission = 1:10)
df$pollutant = "CO"
df$emission <- units::set_units(df$emission, "g")

test_that("adt works", {
  expect_equal(emis_chem(df, "CBMZ")[2, "mol"],
               (1.963584434130667411)*units::as_units("mol"))
})
