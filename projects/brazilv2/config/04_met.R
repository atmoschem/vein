library(data.table)
x <- readxl::read_excel("config/inventory_all.xlsx",
                        "met")
setDT(x)

x$date <- ISOdate(x$Year, x$Month, 1, 0,0,0)
x[,
  mean(Temperature),
  by = .(region, capitals, date, scenario, Year, Month)] -> mett

names(mett)[ncol(mett)] <- "Temperature"
unique(mett$scenario)
mett[Year %in% 2020:2022 &
       scenario == "historic"]$scenario <- "SSP 1.9"

mett[Year %in% 2020:2022, unique(scenario)]

saveRDS(mett, "config/met.rds")


