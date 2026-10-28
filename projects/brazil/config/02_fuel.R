library(data.table)
library(ggplot2)
library(units)
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
# month index from the month name in column `Data`, not from the row order
meses <- c("JANEIRO", "FEVEREIRO", "MARCO", "ABRIL", "MAIO", "JUNHO",
           "JULHO", "AGOSTO", "SETEMBRO", "OUTUBRO", "NOVEMBRO", "DEZEMBRO")
x$m <- match(toupper(chartr("\u00e7\u00c7", "cC", enc2utf8(x$Data))), meses)
if (anyNA(x$m)) stop("Unknown month names in fuel_all$Data: ",
                     paste(unique(x$Data[is.na(x$m)]), collapse = ", "))

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

setnames(x, "UF", "region")
# chronological order (Jan..Dec) for each region and fuel: monthly
# profiles (pmonth) are read in this order downstream
setorderv(x, c("region", "fuel", "date"))
saveRDS(x, "config/fuel_month.rds")

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
saveRDS(df, "config/fuel.rds")


ggplot(df, 
       aes(x = Year,
           y = consumption_t,
           colour = fuel)) +
  geom_line() +
  facet_wrap(~region,
             scales = "free_y")

