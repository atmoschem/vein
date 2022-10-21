library(data.table)
library(vein)
load("/media/sergio/ext41/models/vein/R/sysdata.rda")
names(sysdata)

sysdata$te_china
# veh ####

j = 1
net <- readRDS("/media/sergio/My Passport1/inventarios/CHN_beijing/network/net.rds")
metadata <- readRDS("/media/sergio/My Passport1/inventarios/CHN_beijing/config/metadata.rds")
tfs <- readRDS("/media/sergio/My Passport1/inventarios/CHN_beijing/config/tfs.rds")

x <- readRDS(paste0("/media/sergio/My Passport1/inventarios/CHN_beijing/veh/", metadata$vehicles[j], ".rds"))
x <- remove_units(x)
pro <- tfs[[metadata$vehicles[j]]]
rbindlist(lapply(seq_along(pro), function(i) {
  x <- x*pro[i]
  x$Hour <- tfs$Hour[i]
  x
})) -> xx

# ef ####
chi <- as.data.table(sysdata$ef_china)
chidet <- as.data.table(sysdata$det_china)
chisp <- as.data.table(sysdata$speed_china)
chisul <- as.data.table(sysdata$sulphur_china)
melt.data.table(data = chisp, 
                id.vars = c("FUEL", "STANDARD", "POLLUTANT"), 
                measure.vars = c("S20", "S20_30", "S30_40", "S40_80", "S80"), 
                variable.name = "speed", 
                value.name = "x") -> efsp


# EF = BEF*FI*GAMMA*LAMDA*THETA*K
# std
std <- readRDS("/media/sergio/My Passport1/inventarios/CHN_beijing/config/std.rds")
std1 <- std[[metadata$vehicles[j]]]

chi[VEH == metadata$v[j] &
      TYPE == metadata$t[j] &
      FUEL == metadata$f[j] &
      POLLUTANT == "CO", 
    c("STANDARD", "EF")] -> base

efb <- EmissionFactors(unlist(lapply(seq_along(std1), function(i) {
  base[STANDARD == std1[i]]$EF
})))
efb

# det ####
path <- "~/Dropbox/buildvein/ef_chinav3.xlsx"
readxl::excel_sheets(path) # For libre office, readODS::read_ods()
det <- readxl::read_xlsx(path = path, sheet = "detlong") |> as.data.table()
std <- readRDS("/media/sergio/My Passport1/inventarios/CHN_beijing/config/std.rds")
std1 <- std[[metadata$vehicles[j]]]

det[VEH == metadata$v[j] &
      TYPE == metadata$t[j] &
      FUEL == metadata$f[j] &
      YEAR == 2015 &
      POLLUTANT == "CO", 
    c("STANDARD", 
      "DET")] -> basedet

efs <- EmissionFactors(unlist(lapply(seq_along(std1), function(i) {
  basedet[STANDARD == std1[i]]$DET*  base[STANDARD == std1[i]]$EF
})))
efs

# speed
speed <- readRDS("/media/sergio/My Passport1/inventarios/CHN_beijing/network/speed.rds")


efsp[FUEL == metadata$f[j] &
       POLLUTANT == "CO", 
     c("STANDARD", "speed", "x")] -> basesp

sp <- as.numeric(speed$V1)
efb <- as.numeric(efb)

efs <- lapply(seq_along(std1), function(i) {
  sp_std <- basesp[STANDARD == std1[i]]$x
  ifelse(
    sp < 20, efb[i]*sp_std[1],
    ifelse(
      sp >= 20 & sp < 30, efb[i]*sp_std[2],
      ifelse(
        sp >= 30 & sp < 40, efb[i]*sp_std[3],
        ifelse(
          sp >= 40 & sp < 80, efb[i]*sp_std[4],
          efb[i]*sp_std[5]
        )
      )
      
    ))
  
})
efs <- as.data.frame(do.call("cbind", efs))
dim(efs)

efss <- EmissionFactors(efs)
plot(efss)
# new ef ####
path <- "~/Dropbox/buildvein/ef_chinav3.xlsx"
readxl::excel_sheets(path) # For libre office, readODS::read_ods()

# temperature
efte <- readxl::read_xlsx(path = path, sheet = "temperature")
efte <- as.data.table(efte)
met <- readRDS("/media/sergio/My Passport1/inventarios/CHN_beijing/config/met.rds")

efte[VEH == metadata$v[j] &
       TYPE == metadata$t[j] &
       FUEL == metadata$f[j] &
       POLLUTANT == "CO", ] -> eftes


rbindlist(lapply(seq_along(met$Temperature), function(i) {
  efss
})) -> efssx

tx <- efssx
te <- rep(met$Temperature, each = nrow(efs))
for(i in 1:ncol(tx)) {
  tx[[i]] <- ifelse(
    te < 10, tx[[i]]*eftes$T10,
    ifelse(
      te > 25, tx[[i]]*eftes$T25,
      tx[[i]]*1      
    ))
}

# humidity
efh <- readxl::read_xlsx(path = path, sheet = "humidity")
efh <- as.data.table(efh)
met <- readRDS("/media/sergio/My Passport1/inventarios/CHN_beijing/config/met.rds")

efh[VEH == metadata$v[j] &
      TYPE == metadata$t[j] &
      FUEL == metadata$f[j] &
      POLLUTANT == "CO", ] -> efhs

txx <- tx
hu <- rep(met$Humidity, each = nrow(efs))
for(i in 1:ncol(tx)) {
  txx[[i]] <- ifelse(
    hu < 50, txx[[i]]*efhs$L50,
    ifelse(
      hu > 50, txx[[i]]*efhs$H50,
      txx[[i]]*1      
    ))
}

# temperature and humidity
readxl::excel_sheets(path)
efth <- readxl::read_xlsx(path = path, sheet = "tehu")
efth <- as.data.table(efth)
met <- readRDS("/media/sergio/My Passport1/inventarios/CHN_beijing/config/met.rds")

efth[VEH == metadata$v[j] &
       TYPE == metadata$t[j] &
       FUEL == metadata$f[j] &
       POLLUTANT == "CO", ] -> efths

txxx <- txx
hu <- rep(met$Humidity, each = nrow(efs))
te <- rep(met$Temperature, each = nrow(efs))
for(i in 1:ncol(txx)) {
  txxx[[i]] <- ifelse(
    hu < 50 & te > 24, txxx[[i]]*efths$TH24L50,
    ifelse(
      hu > 50 & te > 24, txxx[[i]]*efths$TH24H50,
      txxx[[i]]*1      
    ))
}


# altitude
readxl::excel_sheets(path)
efhi <- readxl::read_xlsx(path = path, sheet = "h")
efhi <- as.data.table(efhi)

efhi[VEH == metadata$v[j] &
       TYPE == metadata$t[j] &
       FUEL == metadata$f[j] &
       POLLUTANT == "CO", ] -> efhis

efhi <- readxl::read_xlsx(path = "/media/sergio/My Passport1/inventarios/CHN_beijing/config/inventory_chn_bu.xlsx", 
                          sheet = "h")
efhi <- rep(efhi$h, length(tfs[[metadata$vehicles[j]]]))

txxxx <- txxx
for(i in 1:ncol(txxxx)) {
  txxxx[[i]] <- ifelse(
    efhi > 1500, txxxx[[i]]*efhis$H,
    txxxx[[i]]
    )
}


txxxx$Hour <- xx$Hour

# emis ####
lkm <- do.call("cbind", lapply(1:30, function(i) {
  rep(net$lkm, nrow(tfs))
})) |> add_lkm()

EF <- EmissionFactors(as.data.frame(txxxx[, 1:30]))

dim(lkm)
dim(xx)
dim(EF)
veh <- Vehicles(remove_units(as.data.frame(xx[, 1:30])))

E <- veh*EF*lkm



