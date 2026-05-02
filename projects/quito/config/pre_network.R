library(sf)
net <- st_read("network/manizales.gpkg")

names(net)
net$pc <- net$Autos
net$lcv <- net$Camion.C2
net$trucks <- net$Camion.C2.
net$bus <- net$Buses
net$mc <- net$Motos
net$taxi <- net$TaxiO + net$TaxiD

x <- st_sf(
  data.frame(
    pc = Vehicles(net$Autos),
    lcv = Vehicles(net$Camion.C2),
    trucks = Vehicles(net$Camion.C2.),
    bus = Vehicles(net$Buses),
    mc = Vehicles(net$Motos),
    taxi = Vehicles(net$TaxiO + net$TaxiD),
    ps = Speed(net$Velocidad),
    ffs = Speed(net$Velocidad * 2), #simulando free flow speed
    capacity = Vehicles((net$Autos + 
                  net$Buses +
                  net$Camion.C2 + 
                  net$Camion.C2. + 
                  net$Motos + 
                  net$TaxiO + 
                  net$TaxiD)*1.2) # simulando capacidad
  ),
  geometry = st_geometry(net)
)
x
st_write(x, "network/manizales_simu.gpkg")
