context("to_latex")

df <- data.frame(pollutant = rep(c("CO", "propadiene", "NO2"), 10),
                 emission = vein::Emissions(1:30),
                 region = rep(letters[1:2], 15))
df2 <- long_to_wide(df, column_fixed = "region")

aa <- paste0(tempfile(), ".tex")

test_that("to_latex works", {
  expect_output(to_latex(df2),
                "%.?")
  expect_output(to_latex(df2, caption = ""),
                "%.?")
  expect_output(to_latex(df2,  file = aa),
                ".?")
})

