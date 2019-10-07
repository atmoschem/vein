context("emis_chem")
df <- data.frame(emission = 1:10)
df$pollutant = "CO"
df$emission <- units::set_units(df$emission, "g")

test_that("emis_chem works", {
  expect_equal(emis_chem(df, "CBMZ", long = TRUE)[4, "emission"],
               (1.963584434130667411))
})

df2 <- data.frame(emission = 1:10)
test_that("emis_chem stops", {
  expect_error(emis_chem(df2, "CBMZ"),
               "The.?\\(?")
})

df2 <- data.frame(pollutant = 1:10)
test_that("emis_chem stops", {
  expect_error(emis_chem(df2, "CBMZ"),
               "The.?\\(?")
})

df <- data.frame(emission = 1:10)
df$pollutant = "CO"
test_that("emis_chem stops", {
  expect_error(emis_chem(df, "CBMZ"),
               "df.?\\(?")
})

df <- data.frame(emission = Emissions(1:10))
df$pollutant = "CO"
df$emission <- units::set_units(df$emission, "g")
df2 <- df1 <- df
df1$pollutant = "propadiene"
df2$pollutant = "NO2"
dfe <- rbind(df1, df2)
dfe$region <- rep(letters[1:2], 10)
test_that("emis_chem works", {
  expect_equal(emis_chem(dfe, "CBMZ_MOSAIC", "region", TRUE)[4, "emission"],
               0)
})


test_that("emis_chem works", {
  expect_equal(round(as.numeric(emis_chem(dfe = dfe,
                                          mechanism = "CBMZ_MOSAIC",
                                          long = FALSE)$NO2), 3),
               1.195)
})

test_that("emis_chem works", {
  expect_error(emis_chem(dfe = dfe,
                         mechanism = "CBMZ_MOSAIC",
                         colby = "region",
                         long = FALSE),
               "emis.?\\(?")
})
