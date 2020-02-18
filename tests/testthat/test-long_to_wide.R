context("long_to_wide")
df <- data.frame(pollutant = rep(c("CO", "propadiene", "NO2"), 10),
                 emission = vein::Emissions(1:30),
                 region = rep(letters[1:2], 15))
df
data(net)
test_that("long_to_wide works", {
  expect_equal(long_to_wide(df)$CO[1],
               1)
})


test_that("long_to_wide works", {
  expect_equal(long_to_wide(df, column_fixed = "region")$CO[1],
               1)
})

test_that("long_to_wide works", {
  expect_equal(long_to_wide(df, column_fixed = "region", net = sf::st_as_sf(net)[1:10, ])$CO[1],
               1)
})
