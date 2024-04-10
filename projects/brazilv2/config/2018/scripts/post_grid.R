
switch(language,
       "portuguese" = cat("Distribuindo os comprimentos na grade\n"),
       "english" = cat("Gridding lengths\n"),
       "spanish" = cat("Distribuyendo en la grilla\n")
)

f <- list.files(
  path = "post/streets/",
  pattern = "emis_street",
  full.names = T)

fa <- list.files(
  path = "post/streets/",
  pattern = "emis_street",
  full.names = F)

fa


  x <- readRDS(f[month])[pols]
  
  x <- st_crop(x, st_as_sfc(st_bbox(g)))
  gx <- emis_grid(spobj = x, 
                  g = g, 
                  sr = crs)
  
  print(paste0("post/grids/",
               dom,
               "emis_grid_",
               sprintf("%02d", month),
               ".rds"))
  
  saveRDS(gx,
          paste0("post/grids/",
                 dom,
                 "emis_grid_",
                 sprintf("%02d", month),
                 ".rds"))
          
  rm(gx)
  gc()

switch (language,
        "portuguese" = message("\n\nArquivos em:"),
        "english" = message("\n\nFiles in:"),
        "spanish" = message("\n\nArchivos en:"))

message("post/streets/emi_grid_XX.rds\n")

