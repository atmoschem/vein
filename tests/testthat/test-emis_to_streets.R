context("emis_to_streets")

data(net)
stpro = data.frame(stpro = as.character(unique(net$tstreet)),
                   VAL = 1:9)
dnet <- net["ldv"]
dnet$stpro <- as.character(net$tstreet)
dnet$ID <- "A"
df2 <- data.frame(BC = 10, CO = 20, ID = "A")


test_that("emis_to_streets works", {
  expect_equal(round(emis_to_streets(streets = dnet,
                                     dfemis = df2)$BC[1]),
                0)
  expect_message(round(emis_to_streets(streets = dnet,
                                     dfemis = df2)$BC[1]),
               "f.?")


    expect_equal(round(emis_to_streets(streets = dnet, dfemis = df2, stpro = stpro)$BC[1]),
               0)
    expect_message(round(emis_to_streets(streets = dnet, dfemis = df2, stpro = stpro)$BC[1]),
                 "f.?")
})


