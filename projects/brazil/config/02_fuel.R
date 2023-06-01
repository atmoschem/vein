library(data.table)
library(ggplot2)
library(units)
library(forecast)
x <- readxl::read_excel("config/inventory_all.xlsx",
                        "fuel_all")
setDT(x)

x$ESTADO <- toupper(iconv(x$ESTADO, to = "ASCII//TRANSLIT"))

fn <- readxl::read_excel("config/inventory_all.xlsx",
                         "ibge")
fn$ESTADO <- toupper(iconv(fn$ESTADO, to = "ASCII//TRANSLIT"))

unique(intersect(unique(x$ESTADO),
                 fn$ESTADO))
x <- merge(x,
           fn[, c("ESTADO", "UF"),
              with = F],
           all.x = TRUE)

x

x$Year <- x$ANO
x <- x[Year < 2023]
x$m <- rep(1:12, nrow(x)/12)

setorderv(x, 
          cols = c("ESTADO", "Year", "m"), 
          order = c(1, -1, 1))


x$m3 <- units::set_units(x$m3, m^3)

x[, d := ifelse(
  fuel == "E", 0.809,
  ifelse(
    fuel == "D", 0.84,
    0.75425))]
x$d <- units::set_units(x$d, "t/m^3")

x$consumption_t <- x$m3*x$d


x$date <- ISOdate(x$Year, x$m, 1, 0,0,0)



ggplot(x, 
       aes(x = date,
           y = consumption_t,
           colour = fuel)) +
  geom_line() +
  facet_wrap(~UF,
             scales = "free_y")

setorderv(x, "date", order = -1)
saveRDS(x, "config/fuel_month.rds")

xy <- x[, lapply(.SD, sum),
        .SDcols = c("consumption_t",
                    "m3"),
        by = .(fuel, UF, Year)]

setorderv(xy, "Year", order = -1)

saveRDS(xy, "config/fuel.rds")

# projection did not work ####
# older is 2000 and newer 2022
x$UFF <- paste0(x$UF, x$fuel)
uff <- unique(x$UFF)

x$consumption_t <- as.numeric(x$consumption_t)
x$date <- as.Date(x$date)
x$type <- "data"

rbindlist(pbapply::pblapply(seq_along(uff), function(i) {
  dt <- x[UFF == uff[i]]
  # pasado
  date <- rev(seq.Date(as.Date("1990-01-01"),
                       as.Date("1999-12-01"),
                       by = "month"))
  
  m <- auto.arima(dt$consumption_t)
  f <- forecast(m, length(date))
  
  df_pasado <- data.table(
    date = date,
    UF = dt[1]$UF,
    fuel = dt[1]$fuel,
    UFF = uff[i],
    consumption_t = as.numeric(f$mean),
    type = "projection")   
  
  # futuro
  date = seq.Date(as.Date("2023-01-01"),
                  as.Date("2100-12-01"),
                  by = "month")
  
  m <- auto.arima(dt$consumption_t)
  f <- forecast(m, length(date))
  
  df_futuro <- data.table(
    date = date,
    UF = dt[1]$UF,
    fuel = dt[1]$fuel,
    UFF = uff[i],
    consumption_t = if(any(as.numeric(f$mean) < 0)){ 
      as.data.frame(f$upper)[[1]] 
    } else {
      as.numeric(f$mean)
    },
    type = "projection")   
  
  df <- rbind(df_pasado,
              dt[, c("date", 
                     "UF", 
                     "fuel", 
                     "UFF",
                     "consumption_t",
                     "type"),
                 with = FALSE])
  df
  
})) -> DT

ggplot(DT, 
       aes(x = date,
           y = consumption_t,
           colour = fuel)) +
  geom_line() +
  facet_wrap(~UF,
             scales = "free_y")

saveRDS(x, "config/fuel_month.rds")
