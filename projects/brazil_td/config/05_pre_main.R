years <- 2018:2019

for (i in seq_along(years)) {
    fs::dir_copy(
        path = "config/2018",
        new_path = paste0("estimation/", years[i])
    )
}
