library(data.table)
library(ggplot2)
library(units)
library(forecast)


x <- readxl::read_excel("config/inventory.xlsx",
                        "fuel_all")
setDT(x)

x

x$ESTADO <- toupper(iconv(x$ESTADO, to = "ASCII//TRANSLIT"))

fn <- readxl::read_excel("config/inventory.xlsx",
                         "geocode")
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
        scales = "free_y") -> p

png(filename = "figs/fuel_consumed.png", 
width = 3000, 
height = 2000, 
res = 300
)
print(p)
dev.off()

setorderv(x, "date", order = -1)

names(x)[6] <- "region"
saveRDS(x, "config/rds/fuel_month.rds")

writexl::write_xlsx(x, "config/xlsx/fuel_month.xlsx")

xy <- x[, lapply(.SD, sum),
  .SDcols = c("consumption_t",
              "m3"),
  by = .(fuel, region, Year)]

setorderv(xy, "Year", order = -1)


# all ratios gives 0.9
fs <- c("E", "D", "G")

rbindlist(lapply(1:3,function(j){
  rbindlist(lapply(1:40,function(i){
    l <- xy[fuel == fs[j],
            consumption_t[.N]*0.9^i,
            by = region]
    l$Year <- 2000 - i
    names(l)[2] <- "consumption_t"
    l$fuel <- fs[j]
    l$type <- "data"
    l
  } ))
} )) -> fuel_past

fuel_past$consumption_t <- as.numeric(fuel_past$consumption_t)


fd <- function(x) {
  as.numeric(
    vein::ef_fun(ef = x,
                 x = 1:(length(x) + 78),
                 x0 = 10,
                 k = 0.15,
                 L = max(x)*0.95,
                 verbose = FALSE))
}

ufs <- unique(xy$region)


rbindlist(lapply(1:3,function(j){
  rbindlist(lapply(seq_along(ufs),function(i){

    d <- as.numeric(rev(xy[fuel == fs[j] &
                             region == ufs[i]]$consumption_t))

    df1 <- data.table(
      Year = 2000:2022,
      consumption_t = d,
      region = ufs[i],
      fuel = fs[j],
      type = "data"
    )

    df2 <- data.table(
      Year = 2000:2100,
      consumption_t = fd(d),
      region = ufs[i],
      fuel = fs[j],
      type = "projection"
    )

    rbind(df1, df2)
  } ))
} )) -> fuel_future


df <- rbind(fuel_past,
  fuel_future[type == "data"],
  fuel_future[type == "projection" &
                Year > 2022])

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

