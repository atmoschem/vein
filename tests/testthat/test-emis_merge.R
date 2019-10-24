context("emis_merge")
data(net)

test_that("emis_merge works", {
  expect_error(emis_merge(),
                ".?")
  expect_error(emis_merge(streets = FALSE),
               ".?")
  expect_error(emis_merge(net = net),
               ".?")
  expect_error(emis_merge(under = "before"),
               ".?")
  expect_error(emis_merge(ignore = TRUE),
               ".?")
})

# trying inventory ####
name = file.path(tempdir(), "YourCity")
inventory(name = name)
source(paste0(name, "/main.R"))


test_that("emis_merge works", {
  expect_equal(round(emis_merge(path = paste0(name, "/emi"),
                          pol = 'CO',
                          net = net)$V1[1]),
               Emissions(756))

  expect_warning(emis_merge(path = paste0(name, "/emi"),
                                pol = 'CO',
                                net = net),
               ".?")

  expect_output(emis_merge(path = paste0(name, "/emi"),
                            pol = 'CO',
                            net = net),
                 ".?")

  expect_equal(round(emis_merge('CO', what = 'DF.rds', FALSE,
                                path = paste0(name, "/emi"))$g[1]),
               Emissions(6))

})
