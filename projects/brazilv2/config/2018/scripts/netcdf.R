
dim(g$tro_1[])

lx <- list.files("post/grids", 
                 full.names = T)

m <- sprintf("%02d", 1:12)

for(i in seq_along(gpol)) {
  print(gpol[i])
  
  for(j in seq_along(lx)) {
    print(m[j])
    g[[j]][] <- readRDS(lx[j])[[gpol[i]]]
    writeCDF(g[[j]],  
             paste0("nc/", 
                    gpol[i], "_", 
                    m[j], 
                    ".nc"), 
             overwrite = T)
  }
}


lv <- list.files("post/spec_grid", 
                 full.names = T)


for(i in seq_along(gvoc)) {
  print(gvoc[i])
  
  
  for(j in seq_along(lv)) {
    
    x <- readRDS(lv[j])

      print(m[j])
      
      g[[j]][] <- x[[gvoc[i]]]
      writeCDF(g[[j]],  
               paste0("nc/", 
                      gvoc[i], "_", 
                      m[j], 
                      ".nc"), 
               overwrite = T)
  }
}
