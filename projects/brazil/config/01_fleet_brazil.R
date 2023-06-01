
library(data.table)

list.files()
x <- readxl::excel_sheets("config/inventory_all.xlsx")
denatran <- readxl::read_excel("config/inventory_all.xlsx", "denatran")
metadata <- readxl::read_excel("config/inventory_all.xlsx", "metadata")
setDT(denatran)
setDT(metadata)

names(denatran) <- iconv(names(denatran),
                         to="ASCII//TRANSLIT") |>
  toupper()

names(denatran) <- gsub(" ", "_", names(denatran))

denatran$UF <- iconv(denatran$UF,
                     to="ASCII//TRANSLIT") |>
  toupper()

setorderv(denatran, cols = c("UF", "ANO"), order = c(1, -1))

# family ####
denatran[, pc:= AUTOMOVEL]

denatran$lcv <- rowSums(denatran[, 
                                 c("CAMIONETA",
                                   "UTILITARIO",
                                   "CAMINHONETE"),
                                 with = FALSE],
                        na.rm = TRUE)

denatran$trucks <- rowSums(denatran[, 
                                    c("CAMINHAO",
                                      "CAMINHAO_TRATOR"),
                                    with = FALSE],
                           na.rm = TRUE)

denatran$bus <- rowSums(denatran[, 
                                 c("MICROONIBUS",
                                   "ONIBUS"),
                                 with = FALSE],
                        na.rm = TRUE)


denatran$mc <- rowSums(denatran[, 
                                c("CICLOMOTOR",
                                  "MOTOCICLETA",
                                  "MOTONETA",
                                  "QUADRICICLO",
                                  "SIDECAR"),
                                with = FALSE],
                       na.rm = TRUE)



# projetando frota no passado ####
# considerar taxa crecimento ultimo ano

ratio_pc <- denatran[, mean(pc[.N]/pc[.N-1]), by = UF][, mean(V1)]
ratio_lcv <- denatran[, mean(lcv[.N]/lcv[.N-1]), by = UF][, mean(V1)]
ratio_trucks <- denatran[, mean(trucks[42]/trucks[41]), by = UF][, mean(V1)]
ratio_bus <- denatran[, mean(bus[42]/bus[41]), by = UF][, mean(V1)]
ratio_mc <- denatran[, mean(mc[42]/mc[41])]

# all ratios gives 0.9
l <- denatran[, pc[.N]*0.9, by = UF]$V1

rbindlist(lapply(1:40,function(i){
  l <- denatran[, pc[.N]*0.9^i, by = UF]
  l$ANO <- 1979 - i
  names(l)[2] <- "pc"
  
  l$lcv <- denatran[, lcv[.N]*0.9^i, by = UF]$V1
  l$trucks <- denatran[, trucks[.N]*0.9^i, by = UF]$V1
  l$bus <- denatran[, bus[.N]*0.9^i, by = UF]$V1
  l$mc <- denatran[, mc[.N]*0.9^i, by = UF]$V1
  l
} )) -> frota_pasada

dt <- denatran
fam <- c("pc", "lcv", "trucks", "bus", "mc")
ufs <- unique(dt$UF)[c(3, 4, 11, 13, 16, 17, 18, 21, 22,23, 25, 26)]
yy =  c(2000, 2002)
for(i in seq_along(ufs)) {
  for(j in seq_along(fam)){
    for(k in seq_along(yy)){
      dt[UF == ufs[i] & ANO %in% yy[k]][[fam[j]]] <- 
        dt[UF == ufs[i] & ANO %in% (yy[k] - 1)][[fam[j]]]*0.5 + 
        dt[ UF == ufs[i] & ANO %in% (yy[k] + 1)][[fam[j]]]*0.5 
    }
  }
}
library(ggplot2)
ggplot(dt, 
       aes(x = ANO,
           y = pc)) +
  geom_point() +
  facet_wrap(~UF,
             scales = "free_y")



# projection ####
fd <- function(dt, x, n) {
  pro <- as.numeric(vein::ef_fun(ef = x, 
                           x = 1:(length(x) + 78),
                           x0 = 35, 
                           k = 0.15, 
                           L = max(x)*1.2, 
                           verbose = FALSE))
  
  dt[[n]] <- c(x, pro)
  
  dt[["type"]] <- c(rep("original", length(x)),
                    rep("logistic", length(pro))
  )
  dt
}

uf <- unique(denatran$UF)

rbindlist(lapply(seq_along(uf), function(i) {
  dfx <- data.frame(y = c(1979:2022, 1979:2100))
  dfx <- fd(dfx, rev(dt[UF == unique(UF)[i]]$pc), n = "pc")
  dfx <- fd(dfx, rev(dt[UF == unique(UF)[i]]$lcv), n = "lcv")
  dfx <- fd(dfx, rev(dt[UF == unique(UF)[i]]$trucks), n = "trucks")
  dfx <- fd(dfx, rev(dt[UF == unique(UF)[i]]$bus), n = "bus")
  dfx <- fd(dfx, rev(dt[UF == unique(UF)[i]]$mc), n = "mc")
  dfx$UF <- uf[i]
  dfx
})) -> dxr


ggplot(dxr, 
       aes(x = y, 
           y = trucks, 
           colour = type)) +
  geom_point() +
  facet_wrap(~UF,
             scales = "free_y")

names(frota_pasada) <- c("UF", "pc", "y", "lcv", "trucks", "bus", "mc")
frota_pasada$type <- "projected"

frota_pasada <- frota_pasada[, names(dxr), with = F]
dxr_final <- rbind(frota_pasada[, names(dxr), with = F],
                   dxr[type == "original"],
                   dxr[type == "logistic" &
                         y > 2022])

fleet <- data.table(Year = dxr_final$y,
                    region = dxr_final$UF)

uve <- unique(metadata$family)
for(j in seq_along(uve)) {
  ve <- metadata[family == uve[j]]$vehicles
  for(i in seq_along(ve)){
    fleet[[ve[i]]] <- dxr_final[[tolower(uve[j])]]*metadata$prop_fam[i]
  }
}

fleet[, PC_E := ifelse(!Year %in% 1979:2006, 0, PC_E)]
fleet[, LCV_E := ifelse(!Year %in% 1979:2006, 0, LCV_E)]
fleet[, PC_FE := ifelse(Year < 2003, 0, PC_FE)]
fleet[, PC_FG := ifelse(Year < 2003, 0, PC_FG)]
fleet[, LCV_FE := ifelse(Year < 2003, 0, LCV_FE)]
fleet[, LCV_FG := ifelse(Year < 2003, 0, LCV_FG)]

fleet[, MC_150_FG := ifelse(Year < 2009, 0, MC_150_FG)]
fleet[, MC_150_500_FG := ifelse(Year < 2009, 0, MC_150_500_FG)]
fleet[, MC_500_FG := ifelse(Year < 2009, 0, MC_500_FG)]

fleet[, MC_150_FE := ifelse(Year < 2009, 0, MC_150_FE)]
fleet[, MC_150_500_FE := ifelse(Year < 2009, 0, MC_150_500_FE)]
fleet[, MC_500_FE := ifelse(Year < 2009, 0, MC_500_FE)]


fn <- readxl::read_excel("config/inventory_all.xlsx",
                         "ibge")
fn$region <- toupper(iconv(fn$ESTADO, to = "ASCII//TRANSLIT"))

unique(intersect(unique(fleet$region),
                 fn$region))
fleet <- merge(fleet,
           fn[, c("region", "UF"),
              with = F],
           all.x = TRUE)

setorderv(fleet, cols = "Year", order = -1)

saveRDS(fleet, "config/fleet_age.rds")
