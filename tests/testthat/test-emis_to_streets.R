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
    stpro2 <- stpro
    names(stpro)[1] = "caca"
    names(stpro2)[2] = "caca"
    expect_error(emis_to_streets(streets = dnet, dfemis = df2, stpro = stpro),
                 ".?")
    expect_error(emis_to_streets(streets = dnet, dfemis = df2, stpro = stpro2),
                 ".?")
})



test_that("emis_to_streets works", {
  netsf <- sf::st_as_sf(net)
  expect_equal(round(emis_to_streets(streets = dnet,
                                     dfemis = st_sf(df2, geometry = netsf$geometry[1]))$BC[1]),
               0)
})

