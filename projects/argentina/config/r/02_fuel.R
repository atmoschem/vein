library(data.table)
library(ggplot2)
library(units)


x <- readxl::read_excel("config/inventory.xlsx",
                        "fuel")
setDT(x)

x

x$region <- toupper(iconv(x$region, to = "ASCII//TRANSLIT"))

fn <- readxl::read_excel("config/inventory.xlsx",
                         "geocode")

fn$region <- toupper(iconv(fn$Provincia, to = "ASCII//TRANSLIT"))

unique(intersect(unique(x$region),
                 fn$region))
x <- merge(x,
           fn[, c("region", "UF"),
              with = F],
           all.x = TRUE)

x




x$date <- ISOdate(x$Year, x$Month, 1, 0,0,0)

x$consumption_t <- x$density_tm3*x$FUEL_M3

x$m3 <- x$FUEL_M3

ggplot(x,
  aes(x = date,
      y = consumption_t,
      colour = fuel)) +
geom_line() +
facet_wrap(~UF,
        scales = "free_y") -> p

png(filename = "figs/fuel_consumed.png", 
width = 3000, 
height = 2000, 
res = 300
)
print(p)
dev.off()

setorderv(x, "date", order = -1)

saveRDS(x, "config/rds/fuel_month.rds")

writexl::write_xlsx(x, "config/xlsx/fuel_month.xlsx")

xy <- x[, lapply(.SD, sum),
      .SDcols = c("consumption_t",
                  "m3"),
       by = .(fuel, region, Year)]

setorderv(xy, "Year", order = -1)

setorderv(df,
c("region",
  "Year",
  "fuel"),
order = c(1, 1, 1))

saveRDS(df, "config/rds/fuel.rds")
writexl::write_xlsx(df, "config/xlsx/fuel.xlsx")

ggplot(df,
  aes(x = Year,
      y = consumption_t,
      colour = fuel)) +
geom_line() +
facet_wrap(~region,
        scales = "free_y") -> p

png(filename = "figs/fuel_consumed_projected.png", 
width = 3000, 
height = 2000, 
res = 300
)
print(p)
dev.off()

