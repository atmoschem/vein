


library(data.table)

list.files()
x <- readxl::excel_sheets("config/inventory_all.xlsx")
denatran <- readxl::read_excel("config/inventory_all.xlsx", "denatran")
metadata <- readxl::read_excel("config/inventory_all.xlsx", "metadata")
setDT(denatran)
setDT(metadata)



names(denatran) <- toupper(iconv(names(denatran),
                         to="ASCII//TRANSLIT"))

names(denatran) <- gsub(" ", "_", names(denatran))

denatran$UF <- toupper(iconv(denatran$UF,
                     to="ASCII//TRANSLIT"))

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

#names(frota_pasada) <- c("UF", "pc", "y", "lcv", "trucks", "bus", "mc")
frota_pasada$type <- "projected"

#stop("para")

names(frota_pasada)

names(dxr)

names(frota_pasada)[3] <- "y"

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

# 1979 - 2018

m <- readxl::read_xlsx("/home/sibarra/BR/tables/inventory.xlsx",
		       "fleet_age")

setDT(m)

setorderv(m, "Year")

mm <- m

v <- c("PC", "LCV", "TRUCKS", "BUS", "MC")

for(i in seq_along(v)) {

	mm[[v[i]]] <- rowSums(mm[, grep(v[i], 
				      names(mm), 
				      value = T), 
			    with = F])

}

mm[, pPC_G := PC_G / PC]
mm[, pPC_E := PC_E / PC]
mm[, pPC_FG := PC_FG / PC]
mm[, pPC_FE := PC_FE / PC]

mm[, pLCV_G := LCV_G / LCV]
mm[, pLCV_E := LCV_E / LCV]
mm[, pLCV_FG := LCV_FG / LCV]
mm[, pLCV_FE := LCV_FE / LCV]
mm[, pLCV_D := LCV_D / LCV]

mm[, pTRUCKS_SL_D := TRUCKS_SL_D / TRUCKS]
mm[, pTRUCKS_L_D := TRUCKS_L_D / TRUCKS]
mm[, pTRUCKS_M_D := TRUCKS_M_D / TRUCKS]
mm[, pTRUCKS_SH_D := TRUCKS_SH_D / TRUCKS]
mm[, pTRUCKS_H_D := TRUCKS_H_D / TRUCKS]

mm[, pBUS_URBAN_D := BUS_URBAN_D / BUS]
mm[, pBUS_MICRO_D := BUS_MICRO_D / BUS]
mm[, pBUS_COACH_D := BUS_COACH_D / BUS]

mm[, pMC_150_G := MC_150_G / MC]
mm[, pMC_150_500_G := MC_150_500_G / MC]
mm[, pMC_500_G := MC_500_G / MC]
mm[, pMC_150_FG := MC_150_FG / MC]
mm[, pMC_150_500_FG := MC_150_500_FG / MC]
mm[, pMC_500_FG := MC_500_FG / MC]
mm[, pMC_150_FE := MC_150_FE / MC]
mm[, pMC_150_500_FE := MC_150_500_FE / MC]
mm[, pMC_500_FE := MC_500_FE / MC]

names(mm)[33:58]

for(i in seq_along(v)) {

        fleet[[v[i]]] <- rowSums(fleet[, grep(v[i],
                                      names(fleet),
                                      value = T),
                            with = F],
	na.rm = T)

}


mmm <- mm[, c("Year", names(mm)[33:58]), with = F]

mmm[is.na(mmm)] <- 0

fleetx <- merge(fleet, mmm, by = "Year", all.x =T)

umm <-names(mm)[33:58]

for(i in seq_along(umm)) {
fleetx[[umm[i]]] <- ifelse(fleet$Year < 1979, 
			   mmm[Year == 1979][[umm[i]]],
			   ifelse(fleet$Year > 2018,
                           mmm[Year == 2018][[umm[i]]],
                          fleetx[[umm[i]]] 
				  ))
}

fleetx[, PC_E := ifelse(!Year %in% 1979:2006, 0, PC_E)]
fleetx[, LCV_E := ifelse(!Year %in% 1979:2006, 0, LCV_E)]


fn <- readxl::read_excel("config/inventory_all.xlsx",
                         "ibge")


setDT(fn)


fn$region <- toupper(iconv(fn$ESTADO, to = "ASCII//TRANSLIT"))

unique(intersect(unique(fleetx$region),
                 fn$region))

fleetx <- merge(fleetx,
           fn[, c("region", "UF"),
              with = F],
	   by = "region",
           all.x = TRUE)

setorderv(fleetx, cols = "Year", order = -1)

fleetx$region <- fleetx$UF


uv <- gsub("p", "", umm)

for(i in seq_along(uv)) {
fleetx[[uv[i]]] <- fleetx[[uv[i]]]*fleetx[[umm[i]]]
}

fleetx <- fleetx[, c("Year", uv, "region", "UF"), with = F]

saveRDS(fleetx, "config/fleet_age.rds")

ur <- unique(fleetx$region)

dt <- melt(fleetx, id.vars = c("region", "UF", "Year"))

for(i in seq_along(ur)) {


p <- ggplot(dt[region == ur[i]],
	    aes(x = Year,
		y = value))+
geom_point() + 
facet_wrap(~variable, scales = "free_y")

png(filename = paste0("figs/intial_fleet_", ur[i], ".png"), 
width = 3000, 
height = 2000, 
res = 300
)
print(p)
dev.off()

}
