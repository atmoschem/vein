# streets  ####
suppressWarnings(file.remove("post/emi_street.rds"))
switch(language,
       "portuguese" = cat("Distribuindo as emissões nas ruas\n"),
       "english" = cat("Distributing emissions on streets\n"),
       "spanish" = cat("Distribuyendo las emisiones en las calles\n")
)


emi <- fread("post/emi_table.csv") 

nonmhc_m <- emi[!pollutant %in% c("NMHC"),
                sum(emissions,
                    na.rm = T),
                by = .(pollutant,
                       region,
                       month)]

nmhc_m <- emi[pollutant %in% c("NMHC"),
              sum(emissions,
                  na.rm = T),
              by = .(type_emi,
                     pollutant,
                     region,
                     fuel,
                     month)]

nmhc_m$type_emi <- toupper(gsub(" ", "_", nmhc_m$type_emi))

nmhc_m$polf <- paste0(nmhc_m$pollutant, 
                      "_",
                      nmhc_m$fuel,
                      "_",
                      nmhc_m$type_emi)
rm(emi)
gc()

roads <- list.files(path = roads_path,
                    pattern = "gpkg",
                    full.names = T)

na_roads <- list.files(path = roads_path,
                       pattern = "gpkg",
                       full.names = F)

na_roads <- gsub("_roads.gpkg", "", na_roads)

na_roads 

for(k in 1:12) {
  
  nonmhc <- nonmhc_m[month == k]
  
  nmhc <- nmhc_m[month == k]
  
  do.call("rbind", pbapply::pblapply(seq_along(roads), function(i) {
    x <- st_read(roads[i], quiet = T)
    x$region <- na_roads[i]
    x$length <- st_length(x)
    x$lengthHDV <- ifelse(
      x[[osm_name]] %in% c("tertiary",
                           "secondary"),
      0,
      x$length
    )
    
    
    dt <- nonmhc[region == na_roads[i]]
    dtpol <- unique(dt$pollutant)
    
    for(j in seq_along(dtpol)) {
      
      if(dtpol[j] %in% c("NO", "NOx", "NO2")) {
        # assuming most of NOx comes from heavy vehicles
        x[[dtpol[j]]] <- dt[pollutant == dtpol[j]]$V1*x$lengthHDV/sum(x$lengthHDV)
      } else {
        x[[dtpol[j]]] <- dt[pollutant == dtpol[j]]$V1*x$length/sum(x$length)
      }
    }
    
    dt <- nmhc[region == na_roads[i]]
    dtpol <- unique(dt$polf)
    
    for(j in seq_along(dtpol)) {
      if(dtpol[j] %in% c("NMHC_D")) {
        # assuming most of NMHC_D comes from heavy vehicles
        x[[dtpol[j]]] <- dt[polf == dtpol[j]]$V1*x$lengthHDV/sum(x$lengthHDV)
      } else {
        x[[dtpol[j]]] <- dt[polf == dtpol[j]]$V1*x$length/sum(x$length)
      }
    }
    
    # saveRDS(x, 
    #         paste0("post/region/",
    #                na_roads[i],
    #                "_",
    #                sprintf("%02d", k),
    #                ".rds"))
    x
  })) ->  emis_street
  
  saveRDS(emis_street, 
          paste0("post/streets/emis_street_",
                 sprintf("%02d", (1:12)[k]),
                 ".rds"))
  
}

switch (language,
        "portuguese" = message("\n\nArquivos em:"),
        "english" = message("\n\nFiles in:"),
        "spanish" = message("\n\nArchivos en:"))

message("post/streets/emi_table_XX.rds\n")

