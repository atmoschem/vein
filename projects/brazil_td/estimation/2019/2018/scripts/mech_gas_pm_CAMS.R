
year_selected  <- as.numeric(substr(x = getwd(),
                                    start = nchar(getwd()) - 3,
                                    stop = nchar(getwd()) ))

gs <- paste0("post/grids/",
             "emis_grid_",
             sprintf("%02d", 1:12),
             ".rds")

voc <- paste0("post/spec_grid/",
              "emis_voc_",
              sprintf("%02d", 1:12),
              ".rds")

for(kk in seq_along(gs)){
print(gs[kk])
  # gas
  x <- readRDS(gs[kk])

  for(i in 1:nrow(df_gas)){

    # g/km^2/s
    x[[df_gas$gas[i]]] <- x[[df_gas$gas[i]]]*units::set_units(1,"g")
    
  seconds <- units::set_units(dmonth(year_selected,kk)*24*3600,"s")

  x[[df_gas$gas[i]]] <-x[[df_gas$gas[i]]]/seconds

    # kg / m^2 / s
    x[[df_gas$gas[i]]] <- units::set_units(x[[df_gas$gas[i]]], "kg/m^2/s")

  }


  # voc
  y <- readRDS(voc[kk])

  ny <- names(y)[2:ncol(y)]

  for(i in seq_along(ny)){

    seconds <- units::set_units(dmonth(year_selected,kk)*24*3600,"s")
    
    # g/km^2/s
    y[[ny[i]]] <- y[[ny[i]]]/seconds
    
   # kg / m^2 / s
    y[[ny[i]]] <- units::set_units(y[[ny[i]]], "kg/m^2/s")

  }

  # bring y to grid by id

  g <- x[, c("id", df_gas$gas)]

  gx <- merge(g, y, by  = "id", all.x = T)


  pols <- c(df_gas$gas,
            ny)

  for(i in seq_along(pols)) {
    gx[[pols[i]]][is.na(gx[[pols[i]]])] <- 0
  }


  #gx <- st_transform(gx, 4326)

  #gx <- st_as_stars(gx)



  print(paste0("post/spec_grid/",
               "emis_cams_",
               sprintf("%02d", kk),
               "_kg_m2_s.rds"))
  saveRDS(gx,
          paste0("post/spec_grid/",
                 "emis_cams_",
                 sprintf("%02d", kk),
                 "_kg_m2_s.rds"))




}

