context("ef_hdv_speed")

test_that("ef_hdv_speed works", {
  expect_equal(ef_hdv_speed(v = "Trucks",t = "RT", g = "<=7.5", e = "II", gr = 0,
                            l = 0.5, p = "HC", show.equation = FALSE)(30), 0.207398745)
})

test_that("ef_hdv_speed works", {
  expect_equal(round(ef_hdv_speed(v = "Trucks",
                            t = "RT",
                            g = "<=7.5",
                            e = data.frame(eu = "II"),
                            gr = 0,
                            l = 0.5,
                            p = "HC",
                            speed = Speed(0))[1,1]),
               EmissionFactors(1))
})
