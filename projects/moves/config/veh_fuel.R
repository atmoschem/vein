library(data.table)
fuel <- readxl::read_excel(path = "../USdata/annual-motor-vehicle-population-by-type-of-fuel-used.xlsx")

unique(fuel$type)
fuel$veh <- ifelse(
  fuel$type == "Cars", "LDV",
  ifelse(
    fuel$type == "Goods and Other Vehicles", "HDV",
    ifelse(
      fuel$type == "Motorcycles", "MC",
      NA)))

unique(fuel$engine)
# https://data.gov.sg/dataset/annual-motor-vehicle-population-by-type-of-fuel-used?view_id=623935d0-3584-43b1-9a03-974b21a77db9&resource_id=39e17d4c-3bd3-4814-9ae7-bf9df39a2f5d
fuel$fuel <- ifelse(
  fuel$engine == "Petrol", "G",
  ifelse(
    fuel$engine == "Diesel", "D",
    ifelse(
      fuel$engine %in% c("Petrol-Electric", "Petrol-Electric (Plug-In)", "Electric", "Diesel-Electric"), "ELEC",
      ifelse(
        fuel$engine %in% c("Petrol-CNG", "CNG"), "CNG",
        NA))))
fuel
setDT(fuel)
fuel[!is.na(veh) &
       !is.na(fuel), 
     sum(number),
     by = .(year, veh, fuel)] -> xx
setorderv(xx, cols = "year", order = -1)

dcast.data.table(data = xx, 
                 formula = year ~ veh+fuel, 
                 value.var = "V1") -> dx
setorderv(dx, cols = "year", order = -1)
writexl::write_xlsx(dx, "config/veh_fuel.xlsx")
# edit inventory.xlsx
rm(list = ls())
gc()
