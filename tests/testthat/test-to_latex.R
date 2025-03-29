context("to_latex")

df <- ef_cetesb(p = "CO",
                veh = "PC_FG",
                full = T)

aa <- paste0(tempfile(), ".tex")

test_that("to_latex works", {
  expect_output(to_latex(df),
                "%.?")
  expect_output(to_latex(df, caption = ""),
                "%.?")
  expect_output(to_latex(df,  file = aa),
                ".?")
})

