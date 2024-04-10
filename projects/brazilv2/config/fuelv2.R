library(data.table)
# https://legado.dados.gov.br/dataset/a-producao-nacional-de-derivados-de-petroleo-m3
x <- fread("config/vendas-derivados-petroleo-etanol-m3-1990-2023.csv")
names(x) <- c("Year", "MES", "REGIAO", "ESTADO", "PRODUTO", "VENDAS")
range(x$Year)
x$ESTADO <- toupper(iconv(x$ESTADO, to = "ASCII//TRANSLIT"))
x$PRODUTO <- toupper(iconv(x$PRODUTO, to = "ASCII//TRANSLIT"))

x$m3 <- as.numeric(gsub(",", ".", x$VENDAS))

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
unique(x$MES)

x[, m :=  fifelse(
  MES == "MAI", 5,
  fifelse(
    MES == "ABR", 4,
    fifelse(
      MES == "JUN", 6,
      fifelse(
        MES == "JAN", 1,
        fifelse(
          MES == "MAR", 3,
          fifelse(
            MES == "AGO", 8,
            fifelse(
              MES == "JUL",7,
              fifelse(
                MES == "FEV", 2,
                fifelse(
                  MES == "OUT", 10,
                  fifelse(
                    MES == "DEZ",12,
                    fifelse(
                      MES == "SET", 7, 
                      13)))))))))))]

unique(x$PRODUTO)
x[, fuel := fifelse(
  PRODUTO == "ETANOL HIDRATADO", "E",
  fifelse(
    PRODUTO == "GASOLINA C", "G",
    fifelse(
      PRODUTO == "OLEO DIESEL", "D",
      "OTHER")))]

x <- x[Year < 2023 & fuel != "OTHER"]

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
saveRDS(x, "config/fuel_month.rds")

# fuel annual ####
x[, lapply(.SD, sum),
  .SDcols = c("consumption_t", "m3"),
  by = .(Year, UF, fuel)] -> fuel

saveRDS(fuel, "config/fuel.rds")
