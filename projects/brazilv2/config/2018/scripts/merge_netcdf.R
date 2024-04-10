f <- list.files(path = "nc", 
                full.names = T)

n <- list.files(path = "nc", 
                full.names = F)

dt <- data.table(f = f,
                 n = gsub(".nc", "", n))


dt$pol = gsub('.{3}$', '', dt$n)
dt$m <- rep(sprintf("%02d", 1:12), 33)

cat(unique(dt$pol))

polu <- c("CH4",
          "CO",
          "CO2",
          "N2O",
          "NH3",
          "NO",
          "NO2",
          "NOx",
          "PM",
          "PM10",
          "SO2",
          "voc1",
          "voc2",
          "voc3",
          "voc4",
          "voc5",
          "voc6",
          "voc8",
          "voc9",
          "voc13",
          "voc14",
          "voc16",
          "voc17",
          "voc21",
          "voc22",
          "voc23",
          "voc25"
)

# kg m-2 s-1
dir.create("nc/merged")
for (i in seq_along(polu)){
  print(polu[i])
  x <- dt[pol == polu[i]]
  nc <- rast(lapply(x$f, rast))
  d <- nc*km/m*(1/1000)*(1/(365*24*3600))
  writeCDF(d, 
           paste0("nc/merged/", polu[i], ".nc"),
           overwrite = T)
}

nmhc <- c("NMHC_D_EXHAUST",
          "NMHC_E_EVAPORATIVES_HISTORIC",
          "NMHC_E_EXHAUST",
          "NMHC_G_EVAPORATIVES_HISTORIC",
          "NMHC_G_EXHAUST")

lnmhc <- lapply (seq_along(nmhc), function(i){
  print(nmhc[i])
  x <- dt[pol == nmhc[i]]
  nc <- rast(lapply(x$f, rast))
  nc*km/m*(1/1000)*(1/(365*24*3600))
})
names(lnmhc) <- nmhc
rnmhc <- lnmhc$NMHC_D_EXHAUST + 
  lnmhc$NMHC_E_EVAPORATIVES_HISTORIC + 
  lnmhc$NMHC_E_EXHAUST + 
  lnmhc$NMHC_G_EVAPORATIVES_HISTORIC + 
  lnmhc$NMHC_G_EXHAUST

names(rnmhc) <- paste0("NMHC_", sprintf("%02d", 1:12))
writeCDF(rnmhc, 
         paste0("nc/merged/NMHC.nc"),
         overwrite = T)
