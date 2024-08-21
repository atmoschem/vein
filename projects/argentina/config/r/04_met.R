library(data.table)
x <- readxl::read_excel("config/inventory.xlsx",
                        "met")
setDT(x)

x$Month <- month(x$date)


x[,
  mean(Temperature),
  by = .(region, capitals, date, Year, Month)] -> mett

names(mett)[ncol(mett)] <- "Temperature"
unique(mett$scenario)


saveRDS(mett, "config/rds/met.rds")
writexl::write_xlsx(met, "config/xlsx/met.xlsx")

# rain ####
x <- readxl::read_excel("config/inventory.xlsx",
                        "rain")
setDT(x)
x$date <- as.Date(paste0(x$Fecha, "-01"))
x
x$Month <- month(x$date)
x$month <- month(x$date)

nd <- unlist(lapply(1:nrow(x), function(i) {
vein::dmonth(year = year(x$date[i]), month =  month(x$date[i]))
}))

x$P <- x$numDias
x$N <- nd
x$PN <- x$P/x$N
saveRDS(x, "config/rds/rain.rds")
saveRDS(x, "estimation/2019/config/rain.rds")
