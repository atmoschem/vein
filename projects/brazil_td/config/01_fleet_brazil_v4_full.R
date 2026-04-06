library(data.table)
library(colorout)

list.files()

x <- readxl::excel_sheets("config/inventory.xlsx")

v <- readxl::read_excel("config/inventory.xlsx", "fleet_age")
setDT(v)

v <- v[Year > 1978]

v[is.na(v)] <- 0

veh <- readxl::read_excel("config/inventory.xlsx", "metadata")$vehicles

denatran <- readxl::read_excel("config/inventory_all.xlsx", "denatran")

setDT(denatran)

denatran$UF <- toupper(iconv(denatran$UF, to = "ASCII//TRANSLIT"))

# projetando frota no passado ####
# considerar taxa crecimento ultimo ano
lapply(seq_along(veh), function(i) {
  lapply(1:40, function(j) {
    v[, eval(parse(text = veh[i]))[.N] * 0.95^j]
  }) -> dd
  unlist(dd)
}) -> df_pasada

df_pasada <- do.call("cbind", df_pasada)

df <- as.data.table(as.data.frame(df_pasada))

names(df) <- veh

df[is.na(df)] <- 0

df$Year <- 1978:(1978 - 39)

# projection ####
fd <- function(x) {
  pro <- as.numeric(vein::ef_fun(
    ef = x,
    x = 1:(length(x) + 80),
    x0 = 35,
    k = 0.15,
    L = max(x) * 1.2,
    verbose = FALSE
  ))

  dt <- data.table(x = c(x, pro))

  dt[["type"]] <- c(rep("original", length(x)), rep("projected", length(pro)))
  dt
}

rbindlist(lapply(seq_along(veh), function(i) {
  dfx <- fd(rev(v[[veh[i]]]))
  dfx$Year <- c(1979:2020, 1979:2100)
  dfx$veh <- veh[i]
  dfx
})) -> dxr

# further projection for gasoline engines
# constant, no growth

df[, type := "projected"]


melt(data = df, id.vars = c("Year", "type")) -> dff

names(dff) <- c("Year", "type", "veh", "x")


DT <- rbind(dff, dxr[, names(dff), with = F])

gveh <- grep("_G", veh, value = T)

eveh <- grep("_E", veh, value = T)

geveh <- c(gveh, eveh)

#for(i in seq_along(geveh)) {
#DT[Year > 2020 &
#   veh == geveh[i]]$x <-
#
#DT[Year == 2020 &
#   veh == geveh[i] &
#   type == "original"]$x
#
#}

dx <- rbind(
  DT[
    type == "projected" &
      !Year %in% 1979:2020
  ],
  DT[type == "original"]
)

library(ggplot2)

p <- ggplot(dx, aes(x = Year, y = x, colour = type)) +
  geom_point() +
  facet_wrap(~veh, scales = "free_y")

fn <- readxl::read_excel("config/inventory_all.xlsx", "ibge")

setDT(fn)

fn$region <- toupper(iconv(fn$ESTADO, to = "ASCII//TRANSLIT"))

# dcast
dx[is.na(dx)] <- 0

dcast.data.table(data = dx, formula = Year + type ~ veh) -> dff

rbindlist(lapply(seq_along(fn$UF), function(i) {
  dff$region <- fn$UF[i]

  dff
})) -> DD

setorderv(DD, c("Year", "region"), c(-1, 1))

saveRDS(DD, "config/fleet_age_all.rds")
saveRDS(DD, "config/fleet_age.rds")
