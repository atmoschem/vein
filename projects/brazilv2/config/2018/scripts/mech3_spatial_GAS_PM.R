dir.create("post/spec_grid", showWarnings = F)

gs <- paste0("post/grids/",
             dom,
             "emis_grid_0",
             month,
             ".rds")



tg <- paste0("post/spec_grid/",
             dom,
             "emis_voc_0",
             month,
             ".rds")

x <- readRDS(tg)
for(i in 2:ncol(x)){
  x[[i]] <- x[[i]]*units::set_units(1, "g")/units::set_units(1, "h")/31/24
}

y <- readRDS(gs)

for(i in seq_along(gas)){
  x[[gas[i]]] <- y[[gas[i]]]/units::set_units(mw[i], h/mol)/31/24
}

# PM ####
head(y$PM)
y$PM <- y$PM*units::set_units(mw[i], g/h)/31/24

gPM1 <- speciate(x = y["PM"], 
                 spec = aer, 
                 list = T)

for(i in seq_along(gPM1)) {
  x[[names(gPM1[i])]] <- gPM1[[i]][[1]]
}

x$E_PM_10 <- y$PM10*units::set_units(mw[i], g/h)/31/24
x$E_PM10 <- y$PM10*units::set_units(mw[i], g/h)/31/24

gs <- paste0("post/grids/",
             dom,
             "emis_grid_0",
             month,
             ".rds")

saveRDS(x,
        paste0("post/spec_grid/",
               dom,
               "emis_grid_0",
               month,
               "_mol.rds"))
switch (language,
        "portuguese" = message("\n\nArquivos em:"),
        "english" = message("\n\nFiles in:"),
        "spanish" = message("\n\nArchivos en:"))

message("post/spec_grid/*\n")
