context("ef_china")

test_that("ef_china works", {
  expect_equal(ef_china(standard = c( "I"), p = "CO"),
               EmissionFactors(6.47876))
})

df_st <- matrix(c("V", "IV", "III", "III", "II", "I", "PRE"),
                nrow = 10, ncol = 7, byrow = TRUE)
df_st <- as.data.frame(df_st)

test_that("ef_china works", {
  expect_equal(ef_china(standard = df_st,
                        p = "CO",
                        ta = rep(celsius(20), 10),
                        altitude = rep(1501, 10),
                        speed = rep(Speed(29), 10),
                        sulphur = rep(50, 10))[1,1],
               EmissionFactors(0.7234567 + 2.764505e-08))
})
