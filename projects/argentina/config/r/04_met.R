library(data.table)
x <- readxl::read_excel("config/inventory.xlsx",
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


mett[is.na(Temperature), unique(Year)]

met1 <- mett[Year %in% 1960:2020]
met2 <- mett[Year == 2022]

met2$Year <- 2021
met3 <- mett[Year %in% 2022:2100]
met <- rbind(met1, met2, met3)

met[is.na(Temperature), unique(Year)]

saveRDS(met, "config/rds/met.rds")
writexl::write_xlsx(met, "config/xlsx/met.xlsx")
